-module(erldantic_abstract_code).

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-export([types_in_module/1]).

-spec types_in_module(atom()) ->
                         {ok, erldantic:type_info()} | {error, [erldantic:error()]}.
types_in_module(Module) ->
    case code:which(Module) of
        Error
            when Error =:= non_existing
                 orelse Error =:= cover_compiled
                 orelse Error =:= preloaded ->
            {error,
             [#ed_error{type = module_types_not_found,
                        location = [],
                        ctx = #{module => Module, error => Error}}]};
        FilePath ->
            case beam_lib:chunks(FilePath, [abstract_code]) of
                {ok, {Module, [{abstract_code, {_, Forms}}]}} ->
                    NamedTypes =
                        lists:filtermap(fun(F) ->
                                           %%io:format("F ~p~n", [F]),
                                           type_in_form(F)
                                        end,
                                        Forms),
                    TypeInfo = maps:from_list(NamedTypes),
                    {ok, TypeInfo};
                {error, beam_lib, Reason} ->
                    {error,
                     [#ed_error{type = beam_lib_error,
                                location = [],
                                ctx = #{module => Module, reason => Reason}}]}
            end
    end.

-spec type_in_form(term()) ->
                      false | {true, {erldantic:a_type_reference(), erldantic:a_type()}}.
type_in_form({attribute, _, record, {RecordName, Fields}})
    when is_list(Fields) andalso is_atom(RecordName) ->
    FieldInfos = lists:map(fun record_field_info/1, Fields),
    {true, {{record, RecordName}, #a_rec{name = RecordName, fields = FieldInfos}}};
type_in_form({attribute, _, type, {TypeName, {type, _, record, Attrs}, [] = Args}})
    when is_atom(TypeName) ->
    [{atom, _, RecordName} | FieldInfo] = Attrs,
    true = is_atom(RecordName),
    FieldTypes =
        lists:map(fun({type, _, field_type, [{atom, _, FieldName}, RowFieldInfo]})
                         when is_atom(FieldName) ->
                     [A] = field_info_to_type(RowFieldInfo),
                     {FieldName, A}
                  end,
                  FieldInfo),
    TypeArity = length(Args),
    {true, {{type, TypeName, TypeArity}, #a_rec{name = RecordName, fields = FieldTypes}}};
type_in_form({attribute, _, type, {TypeName, {type, _, _, _} = Type, Args}})
    when is_atom(TypeName) andalso is_list(Args) ->
    [FieldInfo] = field_info_to_type(Type),
    Vars = lists:map(fun({var, _, VarName}) when is_atom(VarName) -> VarName end, Args),
    TypeArity = length(Args),
    case Vars of
        [] ->
            {true, {{type, TypeName, TypeArity}, FieldInfo}};
        _ ->
            {true, {{type, TypeName, TypeArity}, #a_type{type = FieldInfo, vars = Vars}}}
    end;
type_in_form({attribute, _, type, {TypeName, {_Literal, _, Value}, [] = Args}})
    when (is_atom(Value) orelse is_integer(Value)) andalso is_atom(TypeName) ->
    TypeArity = length(Args),
    {true, {{type, TypeName, TypeArity}, {literal, Value}}};
type_in_form({attribute, _, type, {TypeName, {user_type, _, _, _} = ReferedType, Args}})
    when is_atom(TypeName) andalso is_list(Args) ->
    [FieldInfo] = field_info_to_type(ReferedType),
    Vars = lists:map(fun({var, _, VarName}) when is_atom(VarName) -> VarName end, Args),
    TypeArity = length(Args),
    case Vars of
        [] ->
            {true, {{type, TypeName, TypeArity}, FieldInfo}};
        _ ->
            {true, {{type, TypeName, TypeArity}, #a_type{type = FieldInfo, vars = Vars}}}
    end;
type_in_form({attribute,
              _,
              type,
              {TypeName,
               {remote_type, _, [{atom, _, Module}, {atom, _, RemotTypeName}, TypeArgs]},
               [] = Args}})
    when is_atom(TypeName)
         andalso is_atom(Module)
         andalso is_atom(RemotTypeName)
         andalso is_list(TypeArgs) ->
    MyTypeArgs =
        lists:map(fun ({type, _, field_type, [{atom, _, FieldName}, FieldInfo]})
                          when is_atom(FieldName) ->
                          %% TODO should I handle record field and other types in one function clause (one clause for all remote types)?
                          [AType] = field_info_to_type(FieldInfo),
                          AType;
                      (Type) ->
                          [AType] = field_info_to_type(Type),
                          AType
                  end,
                  TypeArgs),
    TypeArity = length(Args),
    {true,
     {{type, TypeName, TypeArity},
      #remote_type{mfargs = {Module, RemotTypeName, MyTypeArgs}}}};
type_in_form({attribute,
              _,
              type,
              {TypeName, {ann_type, _, [{var, _, _VarName}, Type]}, [] = Args}})
    when is_atom(TypeName) andalso is_list(Args) ->
    [FieldInfo] = field_info_to_type(Type),
    Vars = lists:map(fun({var, _, VarName}) when is_atom(VarName) -> VarName end, Args),
    TypeArity = length(Args),
    case Vars of
        [] ->
            {true, {{type, TypeName, TypeArity}, FieldInfo}};
        _ ->
            {true, {{type, TypeName, TypeArity}, #a_type{type = FieldInfo, vars = Vars}}}
    end;
type_in_form({attribute, _, type, _} = T) ->
    error({not_supported, T}); % TODO: Support this
type_in_form(_) ->
    false.

-spec field_info_to_type(term()) -> [erldantic:a_type()].
field_info_to_type({ann_type, _, [{var, _, _VarName}, Type]}) ->
    field_info_to_type(Type);
field_info_to_type({atom, _, Value}) when is_atom(Value) ->
    [{literal, Value}];
field_info_to_type({integer, _, Value}) when is_integer(Value) ->
    [{literal, Value}];
field_info_to_type(Op) when element(1, Op) =:= op ->
    Value = integer_value(Op),
    [{literal, Value}];
field_info_to_type({var, _, VarName}) when is_atom(VarName) ->
    [{var, VarName}];
field_info_to_type({remote_type, _, [{atom, _, Module}, {atom, _, Type}, Args]})
    when is_atom(Module) andalso is_atom(Type) andalso is_list(Args) ->
    MyArgs =
        lists:map(fun(Arg) ->
                     [A] = field_info_to_type(Arg),
                     A
                  end,
                  Args),
    [#remote_type{mfargs = {Module, Type, MyArgs}}];
field_info_to_type({type, _, map, any}) ->
    %% FIXME: Add test for map()
    [#a_map{fields = [{map_field_type_assoc, {type, term}, {type, term}}]}];
field_info_to_type({type, _, tuple, any}) ->
    [{type, tuple}];
field_info_to_type({TypeOfType, _, Type, TypeAttrs}) when is_list(TypeAttrs) ->
    case {TypeOfType, Type} of
        {type, record} ->
            [{atom, _, SubTypeRecordName} | TypeArgs] = TypeAttrs,
            true = is_atom(SubTypeRecordName),
            FieldTypes =
                lists:map(fun({type, _, field_type, [{atom, _, FieldName}, FieldInfo]})
                                 when is_atom(FieldName) ->
                             [AType] = field_info_to_type(FieldInfo),
                             {FieldName, AType}
                          end,
                          TypeArgs),
            [{record_ref, SubTypeRecordName, FieldTypes}];
        {user_type, Type} ->
            true = is_atom(Type),
            TAttrs = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [{user_type_ref, Type, TAttrs}];
        {type, map} ->
            MapFields = lists:flatmap(fun map_field_info/1, TypeAttrs),
            [#a_map{fields = MapFields}];
        {type, tuple} ->
            TupleFields = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [#a_tuple{fields = TupleFields}];
        {type, union} ->
            UnionFields = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [{union, UnionFields}];
        {type, Fun} when Fun =:= 'fun' orelse Fun =:= function ->
            case TypeAttrs of
                [] ->
                    [{type, 'fun'}];
                [{type, _, product, FunArgTypes}, ReturnType] ->
                    true = is_list(FunArgTypes),
                    AFunArgTypes = lists:flatmap(fun field_info_to_type/1, FunArgTypes),
                    [AReturnType] = field_info_to_type(ReturnType),
                    [#a_function{args = AFunArgTypes, return = AReturnType}]
            end;
        {type, arity} ->
            [{range, integer, 0, 255}];
        {type, byte} ->
            [{range, integer, 0, 255}];
        {type, char} ->
            [{range, integer, 0, 16#10ffff}];
        {type, mfa} ->
            [#a_tuple{fields = [{type, atom}, {type, atom}, {range, integer, 0, 255}]}];
        {type, any} ->
            [{type, term}];
        {type, timeout} ->
            [{union, [{type, non_neg_integer}, {literal, infinity}]}];
        {type, pid} ->
            [{type, pid}];
        {type, iodata} ->
            [{type, iodata}];
        {type, iolist} ->
            [{type, iolist}];
        {type, port} ->
            [{type, port}];
        {type, reference} ->
            [{type, reference}];
        {type, node} ->
            [{type, atom}];
        {type, range} ->
            [MinValue, MaxValue] = TypeAttrs,
            Min = integer_value(MinValue),
            Max = integer_value(MaxValue),
            %% FIXME: check that it is a integer range
            [{range, integer, Min, Max}];
        {type, list} ->
            case lists:flatmap(fun field_info_to_type/1, TypeAttrs) of
                [ListType] ->
                    [{list, ListType}];
                [] ->
                    [{list, {type, term}}]
            end;
        {type, nonempty_list} ->
            [ListType] = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [{nonempty_list, ListType}];
        {type, maybe_improper_list} ->
            [A, B] = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [{maybe_improper_list, A, B}];
        {type, PrimaryType} when ?is_primary_type(PrimaryType) ->
            [{type, PrimaryType}];
        {type, PartailRangeInteger} when ?is_predefined_int_range(PartailRangeInteger) ->
            [{type, PartailRangeInteger}];
        {type, term} ->
            [{type, term}];
        {literal, Literal} ->
            [{literal, Literal}];
        {type, nil} ->
            [{literal, []}]
    end.

integer_value({integer, _, Value}) when is_integer(Value) ->
    Value;
integer_value({op, _, Operator, Left, Right}) ->
    case Operator of
        '-' ->
            integer_value(Left) - integer_value(Right);
        '+' ->
            integer_value(Left) + integer_value(Right);
        '*' ->
            integer_value(Left) * integer_value(Right);
        'div' ->
            integer_value(Left) div integer_value(Right);
        'rem' ->
            integer_value(Left) rem integer_value(Right);
        'band' ->
            integer_value(Left) band integer_value(Right);
        'bor' ->
            integer_value(Left) bor integer_value(Right);
        'bxor' ->
            integer_value(Left) bxor integer_value(Right);
        'bsl' ->
            integer_value(Left) bsl integer_value(Right);
        'bsr' ->
            integer_value(Left) bsr integer_value(Right)
    end;
integer_value({op, _, Operator, Unary}) ->
    case Operator of
        '-' ->
            -integer_value(Unary);
        '+' ->
            integer_value(Unary);
        'bnot' ->
            bnot integer_value(Unary)
    end.

-spec map_field_info(term()) ->
                        [{map_field_assoc | map_field_exact, atom(), erldantic:a_type()} |
                         {map_field_type_assoc | map_field_type_exact,
                          erldantic:a_type(),
                          erldantic:a_type()}].
map_field_info({TypeOfType, _, Type, TypeAttrs}) ->
    case {TypeOfType, Type} of
        {type, map_field_assoc} ->
            case TypeAttrs of
                [{atom, _, MapFieldName}, FieldInfo] when is_atom(MapFieldName) ->
                    [AType] = field_info_to_type(FieldInfo),
                    [{map_field_assoc, MapFieldName, AType}];
                [KeyFieldInfo, ValueFieldInfo] ->
                    [KeyType] = field_info_to_type(KeyFieldInfo),
                    [ValueType] = field_info_to_type(ValueFieldInfo),
                    [{map_field_type_assoc, KeyType, ValueType}]
            end;
        {type, map_field_exact} ->
            case TypeAttrs of
                [{atom, _, MapFieldName}, FieldInfo] ->
                    %%                    beam_core_to_ssa:format_error(Arg1),
                    true = is_atom(MapFieldName),
                    [AType] = field_info_to_type(FieldInfo),
                    [{map_field_exact, MapFieldName, AType}];
                [KeyFieldInfo, ValueFieldInfo] ->
                    [KeyType] = field_info_to_type(KeyFieldInfo),
                    [ValueType] = field_info_to_type(ValueFieldInfo),
                    [{map_field_type_exact, KeyType, ValueType}]
            end
    end.

-spec record_field_info(term()) -> {atom(), erldantic:a_type()}.
record_field_info({record_field, _, {atom, _, FieldName}, _Type})
    when is_atom(FieldName) ->
    %% FIXME: Handle default values in record fields. Also handle default values in typed_record_field?
    {FieldName, {type, term}};
record_field_info({record_field, _, {atom, _, FieldName}}) when is_atom(FieldName) ->
    {FieldName, {type, term}};
record_field_info({typed_record_field, {record_field, _, {atom, _, FieldName}}, Type})
    when is_atom(FieldName) ->
    [TypeInfo] = field_info_to_type(Type),
    {FieldName, TypeInfo};
record_field_info({typed_record_field, {record_field, _, {atom, _, FieldName}, _}, Type})
    when is_atom(FieldName) ->
    [TypeInfo] = field_info_to_type(Type),
    {FieldName, TypeInfo}.
