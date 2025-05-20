-module(erldantic_parse_transform).

-export([parse_transform/2, type_in_form/1, types_in_module/1]).

-include("../include/record_type_introspect.hrl").

-compile({parse_transform, parse_trans_codegen}).

parse_transform(Forms, _Options) ->
    io:format("Inspecting records at compile-time...~n"),

    NamedTypes = lists:filtermap(fun(F) -> type_in_form(F) end, Forms),

    trans_add_to_from_json(Forms, NamedTypes).

trans_add_to_from_json(Forms, NamedTypes) ->
    {BeforeFunctions, Functions} =
        lists:splitwith(fun ({function, _Pos, _F, _A, _Body}) ->
                                false;
                            ({attribute, _, spec, _}) ->
                                false;
                            (_) ->
                                true
                        end,
                        Forms),
    {BeforeExports, ExportsUntilFunctions} =
        lists:splitwith(fun ({attribute, _, export, _}) ->
                                false;
                            (_) ->
                                true
                        end,
                        BeforeFunctions),
    %io:format("Before Exports: ~p~n", [BeforeExports]),
    %io:format("Exports Until Functions: ~p~n", [ExportsUntilFunctions]),
    %io:format("Functions: ~p~n", [Functions]),
    BeforeExports
    ++ named_types_to_exports(NamedTypes)
    ++ ExportsUntilFunctions
    ++ named_types_to_functions(NamedTypes)
    ++ Functions.

named_types_to_functions(NamedTypes) ->
    % {NewNamedTypes, _} =
    %     lists:mapfoldl(fun({{TypeOfThing, TypeName}, Info}, Acc) ->
    %                       Name =
    %                           case sets:is_element(TypeName, Acc) of
    %                               true ->
    %                                   binary_to_atom(iolist_to_binary([TypeOfThing, "_", TypeName]),
    %                                                  utf8);
    %                               _ -> TypeName
    %                           end,
    %                       {{Name, Info}, sets:add_element(Name, Acc)}
    %                    end,
    %                    sets:new([{version, 2}]),
    %                    NamedTypes),
    % [].
    M = maps:from_list(NamedTypes),
    lists:flatmap(fun(A) -> named_type_to_function(A, M) end, NamedTypes).

named_types_to_exports(NamedTypes) ->
    Exports =
        lists:flatmap(fun({{_, TypeName}, _}) ->
                         [{function_name(TypeName, "_from_json"), 1},
                          {function_name(TypeName, "_to_json"), 1}]
                      end,
                      NamedTypes),
    [{attribute, {5, 2}, export, Exports}].

named_type_to_function({{TypeOfThing, TypeName}, _Info}, Infos) ->
    [codegen:gen_function(function_name(TypeName, "_from_json"),
                          fun(Json) ->
                             record_type_introspect:from_json({'$var', Infos},
                                                              {{'$var', TypeOfThing},
                                                               {'$var', TypeName}},
                                                              Json)
                          end),
     codegen:gen_function(function_name(TypeName, "_to_json"),
                          fun(Data) ->
                             record_type_introspect:to_json({'$var', Infos},
                                                            {{'$var', TypeOfThing},
                                                             {'$var', TypeName}},
                                                            Data)
                          end)].

function_name(TypeName, PostFix) ->
    binary_to_atom(iolist_to_binary([atom_to_list(TypeName), PostFix])).

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
            NamedTypes = lists:filtermap(fun erldantic_parse_transform:type_in_form/1, Forms),
            TypeInfo = maps:from_list(NamedTypes),
            {ok, TypeInfo}
    end.

-spec type_in_form(term()) ->
                      false |
                      {true,
                       {record_type_introspect:a_type_reference(),
                        record_type_introspect:a_type()}}.
type_in_form({attribute, _, record, {RecordName, Fields}})
    when is_list(Fields) andalso is_atom(RecordName) ->
    FieldInfos = lists:map(fun record_field_info/1, Fields),
    {true, {{record, RecordName}, #a_rec{name = RecordName, fields = FieldInfos}}};
type_in_form({attribute, _, type, {TypeName, {type, _, record, Attrs}, []}})
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
    {true, {{type, TypeName}, #a_rec{name = RecordName, fields = FieldTypes}}};
type_in_form({attribute, _, type, {TypeName, {type, _, _, _} = Type, A}})
    when is_atom(TypeName) andalso is_list(A) ->
    [FieldInfo] = field_info_to_type(Type),
    Vars = lists:map(fun({var, _, VarName}) when is_atom(VarName) -> VarName end, A),
    %% TODO: Might not need #a_type here.
    {true, {{type, TypeName}, #a_type{type = FieldInfo, vars = Vars}}};
type_in_form({attribute, _, type, {TypeName, {_Literal, _, Value}, []}})
    when (is_atom(Value) orelse is_integer(Value)) andalso is_atom(TypeName) ->
    {true, {{type, TypeName}, {literal, Value}}};
type_in_form({attribute, _, type, {TypeName, {user_type, _, _, _} = ReferedType, A}})
    when is_atom(TypeName) andalso is_list(A) ->
    [FieldInfo] = field_info_to_type(ReferedType),
    Vars = lists:map(fun({var, _, VarName}) when is_atom(VarName) -> VarName end, A),
    {true, {{type, TypeName}, #a_type{type = FieldInfo, vars = Vars}}};
type_in_form({attribute,
              _,
              type,
              {TypeName,
               {remote_type, _, [{atom, _, Module}, {atom, _, RemotTypeName}, TypeArgs]},
               []}})
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
    {true, {{type, TypeName}, #remote_type{mfargs = {Module, RemotTypeName, MyTypeArgs}}}};
type_in_form({attribute, _, type, _} = T) ->
    error({not_supported, T}); % TODO: Support this
type_in_form(_) ->
    false.

-spec field_info_to_type(term()) -> [record_type_introspect:a_type()].
field_info_to_type({ann_type, _, _Attr}) ->
    [];
field_info_to_type({atom, _, Value}) ->
    [{literal, Value}];
field_info_to_type({integer, _, Value}) ->
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
                        [{map_field_assoc | map_field_exact,
                          atom(),
                          record_type_introspect:a_type()} |
                         {map_field_type_assoc | map_field_type_exact,
                          record_type_introspect:a_type(),
                          record_type_introspect:a_type()}].
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

-spec record_field_info(term()) -> {atom(), record_type_introspect:a_type()}.
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
