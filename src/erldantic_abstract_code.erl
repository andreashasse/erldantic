-module(erldantic_abstract_code).

-include("../include/record_type_introspect.hrl").

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
            {ok, {Module, [{abstract_code, {_, Forms}}]}} =
                beam_lib:chunks(FilePath, [abstract_code]),
            NamedTypes = lists:filtermap(fun type_in_form/1, Forms),
            TypeInfo = maps:from_list(NamedTypes),
            {ok, TypeInfo}
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
    %% TODO: Might not need #a_type here.
    TypeArity = length(Args),
    {true, {{type, TypeName, TypeArity}, #a_type{type = FieldInfo, vars = Vars}}};
type_in_form({attribute, _, type, {TypeName, {_Literal, _, Value}, [] = Args}})
    when (is_atom(Value) orelse is_integer(Value)) andalso is_atom(TypeName) ->
    TypeArity = length(Args),
    {true, {{type, TypeName, TypeArity}, {literal, Value}}};
type_in_form({attribute, _, type, {TypeName, {user_type, _, _, _} = ReferedType, Args}})
    when is_atom(TypeName) andalso is_list(Args) ->
    [FieldInfo] = field_info_to_type(ReferedType),
    Vars = lists:map(fun({var, _, VarName}) when is_atom(VarName) -> VarName end, Args),
    TypeArity = length(Args),
    {true, {{type, TypeName, TypeArity}, #a_type{type = FieldInfo, vars = Vars}}};
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
type_in_form({attribute, _, type, _} = T) ->
    error({not_supported, T}); % TODO: Support this
type_in_form(_) ->
    false.

-spec field_info_to_type(term()) -> [erldantic:a_type()].
field_info_to_type({ann_type, _, _Attr}) ->
    [];
field_info_to_type({atom, _, Value}) when is_atom(Value) ->
    [{literal, Value}];
field_info_to_type({integer, _, Value}) when is_integer(Value) ->
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
field_info_to_type({TypeOfType, _, Type, TypeAttrs}) ->
    true = is_list(TypeAttrs),
    case {TypeOfType, Type} of
        {type, record} ->
            %% HERE
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
        {type, range} ->
            [{RangeType, _, Min}, {RangeType, _, Max}] = TypeAttrs,
            case {RangeType, Min, Max} of
                {integer, Min, Max} when is_integer(Min), is_integer(Max) ->
                    [{range, RangeType, Min, Max}]
            end;
        {type, list} ->
            [ListType] = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [{list, ListType}];
        {type, nonempty_list} ->
            [ListType] = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [{nonempty_list, ListType}];
        {type, PrimaryType} when ?is_primary_type(PrimaryType) ->
            [{type, PrimaryType}];
        {type, PartailRangeInteger} when ?is_predefined_int_range(PartailRangeInteger) ->
            [{type, PartailRangeInteger}];
        {type, term} ->
            [{type, term}];
        {literal, Literal} ->
            [{literal, Literal}]
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
                [{type, _, _, _} = KeyFieldInfo, ValueFieldInfo] ->
                    [KeyType] = field_info_to_type(KeyFieldInfo),
                    [ValueType] = field_info_to_type(ValueFieldInfo),
                    [{map_field_type_assoc, KeyType, ValueType}]
            end;
        {type, map_field_exact} ->
            case TypeAttrs of
                [{atom, _, MapFieldName}, FieldInfo] ->
                    true = is_atom(MapFieldName),
                    [AType] = field_info_to_type(FieldInfo),
                    [{map_field_exact, MapFieldName, AType}];
                [{type, _, _, _} = KeyFieldInfo, ValueFieldInfo] ->
                    [KeyType] = field_info_to_type(KeyFieldInfo),
                    [ValueType] = field_info_to_type(ValueFieldInfo),
                    [{map_field_type_exact, KeyType, ValueType}]
            end
    end.

-spec record_field_info(term()) -> {atom(), erldantic:a_type()}.
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
