-module(erldantic_abstract_code).

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-export([types_in_module/1]).

%% Due to erl_parse:af_wild_attribute() I can't use some of the types.
-type erl_parse__af_field_decl() :: term().

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

-spec type_in_form(erl_parse:abstract_form() | erl_parse:form_info()) ->
                      false | {true, {erldantic:a_type_reference(), erldantic:a_type()}}.
type_in_form({attribute, _, record, {RecordName, Fields}})
    when is_list(Fields) andalso is_atom(RecordName) ->
    FieldInfos = lists:map(fun record_field_info/1, Fields),
    {true,
     {{record, RecordName},
      #a_rec{name = RecordName,
             fields = FieldInfos,
             arity = length(FieldInfos) + 1}}};
type_in_form({attribute, _, TypeOrOpaque, {TypeName, {_, _, record, Attrs}, [] = Args}})
    when is_atom(TypeName) andalso (TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque) ->
    %% FIXME: Sort out why this function clause differs from all others.
    true = is_list(Attrs),
    {RecordName, FieldTypes} = record_field_types(Attrs),
    TypeArity = length(Args),
    Record = {record_ref, RecordName, FieldTypes},
    {true, {{type, TypeName, TypeArity}, Record}};
type_in_form({attribute, _, TypeOrOpaque, {TypeName, Type, Args}})
    when is_atom(TypeName)
         andalso is_list(Args)
         andalso (TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque) ->
    [FieldInfo] = field_info_to_type(Type),
    Vars = lists:map(fun({var, _, VarName}) when is_atom(VarName) -> VarName end, Args),
    TypeArity = length(Args),
    case Vars of
        [] ->
            {true, {{type, TypeName, TypeArity}, FieldInfo}};
        _ ->
            {true,
             {{type, TypeName, TypeArity}, #type_with_arguments{type = FieldInfo, vars = Vars}}}
    end;
type_in_form({attribute, _, TypeOrOpaque, _} = T)
    when TypeOrOpaque =:= opaque orelse TypeOrOpaque =:= type ->
    error({not_supported, T});
type_in_form(_) ->
    false.

-spec record_field_types(list()) -> {atom(), [{atom(), erldantic:a_type()}]}.
record_field_types(Attrs) ->
    [{atom, _, RecordName} | FieldInfo] = Attrs,
    true = is_atom(RecordName),
    FieldTypes =
        lists:map(fun({type, _, field_type, [{atom, _, FieldName}, RowFieldInfo]})
                         when is_atom(FieldName) ->
                     [A] = field_info_to_type(RowFieldInfo),
                     {FieldName, A}
                  end,
                  FieldInfo),
    {RecordName, FieldTypes}.

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
field_info_to_type({Type, _, map, any}) when Type =:= type orelse Type =:= opaque ->
    %% FIXME: Add test for map()
    [#a_map{fields = [{map_field_type_assoc, {type, term}, {type, term}}]}];
field_info_to_type({Type, _, tuple, any}) when Type =:= type orelse Type =:= opaque ->
    [#a_tuple{fields = any}];
field_info_to_type({user_type, _, Type, TypeAttrs})
    when is_atom(Type) andalso is_list(TypeAttrs) ->
    TAttrs = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
    [{user_type_ref, Type, TAttrs}];
field_info_to_type({TypeOrOpaque, _, Type, TypeAttrs})
    when is_list(TypeAttrs) andalso (TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque) ->
    case Type of
        record ->
            {SubTypeRecordName, FieldTypes} = record_field_types(TypeAttrs),
            [{record_ref, SubTypeRecordName, FieldTypes}];
        map ->
            MapFields = lists:flatmap(fun map_field_info/1, TypeAttrs),
            [#a_map{fields = MapFields}];
        tuple ->
            TupleFields = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [#a_tuple{fields = TupleFields}];
        union ->
            UnionFields = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [{union, UnionFields}];
        Fun when Fun =:= 'fun' orelse Fun =:= function ->
            case TypeAttrs of
                [] ->
                    [#a_function{args = any, return = {type, term}}];
                [{type, _, any}, ReturnType] ->
                    [AReturnType] = field_info_to_type(ReturnType),
                    [#a_function{args = any, return = AReturnType}];
                [{type, _, product, FunArgTypes}, ReturnType] ->
                    true = is_list(FunArgTypes),
                    AFunArgTypes = lists:flatmap(fun field_info_to_type/1, FunArgTypes),
                    [AReturnType] = field_info_to_type(ReturnType),
                    [#a_function{args = AFunArgTypes, return = AReturnType}]
            end;
        arity ->
            [{range, integer, 0, 255}];
        byte ->
            [{range, integer, 0, 255}];
        char ->
            [{range, integer, 0, 16#10ffff}];
        mfa ->
            [#a_tuple{fields = [{type, atom}, {type, atom}, {range, integer, 0, 255}]}];
        any ->
            [{type, term}];
        timeout ->
            [{union, [{type, non_neg_integer}, {literal, infinity}]}];
        pid ->
            [{type, pid}];
        iodata ->
            [{type, iodata}];
        iolist ->
            [{type, iolist}];
        port ->
            [{type, port}];
        reference ->
            [{type, reference}];
        node ->
            [{type, atom}];
        identifier ->
            [{union, [{type, pid}, {type, port}, {type, reference}]}];
        range ->
            [MinValue, MaxValue] = TypeAttrs,
            Min = integer_value(MinValue),
            Max = integer_value(MaxValue),
            %% FIXME: check that it is a integer range
            [{range, integer, Min, Max}];
        list ->
            case lists:flatmap(fun field_info_to_type/1, TypeAttrs) of
                [ListType] ->
                    [{list, ListType}];
                [] ->
                    [{list, {type, term}}]
            end;
        nonempty_list ->
            case lists:flatmap(fun field_info_to_type/1, TypeAttrs) of
                [ListType] ->
                    [{nonempty_list, ListType}];
                [] ->
                    %% FIXME: missing test.
                    [{nonempty_list, {type, term}}]
            end;
        maybe_improper_list ->
            case lists:flatmap(fun field_info_to_type/1, TypeAttrs) of
                [A, B] ->
                    [#maybe_improper_list{elements = A, tail = B}];
                [] ->
                    [#maybe_improper_list{elements = {type, term}, tail = {type, term}}]
            end;
        nonempty_improper_list ->
            [A, B] = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [#nonempty_improper_list{elements = A, tail = B}];
        module ->
            [{type, atom}];
        PrimaryType when ?is_primary_type(PrimaryType) ->
            [{type, PrimaryType}];
        PartailRangeInteger when ?is_predefined_int_range(PartailRangeInteger) ->
            [{type, PartailRangeInteger}];
        bitstring ->
            [{type, bitstring}];
        nonempty_bitstring ->
            [{type, nonempty_bitstring}];
        dynamic ->
            [{type, term}];
        nil ->
            [{literal, []}];
        none ->
            [{type, none}];
        no_return ->
            [{type, none}]
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

-spec record_field_info(erl_parse__af_field_decl()) -> {atom(), erldantic:a_type()}.
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
