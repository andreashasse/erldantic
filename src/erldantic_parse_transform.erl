-module(erldantic_parse_transform).

-export([parse_transform/2]).

-include("../include/record_type_introspect.hrl").

-compile({parse_transform, parse_trans_codegen}).

parse_transform(Forms, _Options) ->
    io:format("Inspecting records at compile-time...~n"),

    NamedTypes = lists:filtermap(fun type_in_form/1, Forms),

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
    io:format("Before Exports: ~p~n", [BeforeExports]),
    io:format("Exports Until Functions: ~p~n", [ExportsUntilFunctions]),
    io:format("Functions: ~p~n", [Functions]),
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
    lists:map(fun(A) -> named_type_to_function(A, M) end, NamedTypes).

named_types_to_exports(NamedTypes) ->
    Exports =
        lists:map(fun({{_, TypeName}, _}) -> {function_name(TypeName), 1} end, NamedTypes),
    [{attribute, {5, 2}, export, Exports}].

named_type_to_function({{TypeOfThing, TypeName}, _Info}, Infos) ->
    codegen:gen_function(function_name(TypeName),
                         fun(Json) ->
                            record_type_introspect:from_json({'$var', Infos},
                                                             {{'$var', TypeOfThing},
                                                              {'$var', TypeName}},
                                                             Json)
                         end).

function_name(TypeName) ->
    binary_to_atom(iolist_to_binary([atom_to_list(TypeName), "_from_json"])).

type_in_form({attribute, _, record, {RecordName, Fields}})
    when is_list(Fields) andalso is_atom(RecordName) ->
    FieldInfos = lists:map(fun record_field_info/1, Fields),
    {true, {{record, RecordName}, #a_rec{name = RecordName, fields = FieldInfos}}};
type_in_form({attribute, _, type, {TypeName, {type, _, record, Attrs}, []}})
    when is_atom(TypeName) ->
    [{atom, _, RecordName} | FieldInfo] = Attrs,
    true = is_atom(RecordName),
    FieldTypes =
        lists:map(fun({type, _, field_type, [{atom, _, FieldName}, {type, _, FieldType, []}]})
                         when is_atom(FieldName) ->
                     {FieldName, to_a_type({type, FieldType})}
                  end,
                  FieldInfo),
    {true, {{type, TypeName}, #a_rec{name = RecordName, fields = FieldTypes}}};
type_in_form({attribute, _, type, {TypeName, {type, _, map, Attrs}, []}})
    when is_list(Attrs) andalso is_atom(TypeName) ->
    FieldInfos = lists:flatmap(fun map_field_info/1, Attrs),
    {true, {{type, TypeName}, #a_map{fields = FieldInfos}}};
type_in_form({attribute, _, type, {TypeName, {type, _, Type, Attrs}, []}})
    when is_list(Attrs) andalso is_atom(TypeName) ->
    FieldInfos = lists:flatmap(fun field_into_to_type/1, Attrs),
    {true, {{type, TypeName}, to_a_type({Type, FieldInfos})}};
type_in_form(_) ->
    false.

-spec field_into_to_type(term()) -> [record_type_introspect:a_type()].
field_into_to_type({ann_type, _, [{var, _, _VarName}, {type, _, _TypeAnnType, []}]}) ->
    [];
field_into_to_type({TypeOfType, _, Type, TypeAttrs}) ->
    true = is_list(TypeAttrs),
    case {TypeOfType, Type} of
        {type, record} ->
            [{atom, _, SubTypeRecordName}] = TypeAttrs,
            true = is_atom(SubTypeRecordName),
            [{record_ref, SubTypeRecordName}];
        {user_type, Type} ->
            true = is_atom(Type),
            [{user_type_ref, Type}];
        {type, map} ->
            MapFields = lists:flatmap(fun map_field_info/1, TypeAttrs),
            [#a_map{fields = MapFields}];
        {type, tuple} ->
            TupleFields = lists:flatmap(fun field_into_to_type/1, TypeAttrs),
            [#a_tuple{fields = TupleFields}];
        {type, union} ->
            UnionFields =
                lists:map(fun ({type, _, UnionType, []}) ->
                                  to_a_type({type, UnionType});
                              ({atom, _, UnionType, undefined}) ->
                                  to_a_type({literal, UnionType});
                              ({atom, _, UnionType}) ->
                                  to_a_type({literal, UnionType})
                          end,
                          TypeAttrs),
            [{union, UnionFields}];
        {type, Type} ->
            [to_a_type({type, Type})]
    end.

-spec map_field_info(term()) ->
                        [{map_field_assoc | map_field_exact,
                          atom(),
                          record_type_introspect:a_type()}].
map_field_info({TypeOfType, _, Type, TypeAttrs}) ->
    case {TypeOfType, Type} of
        {type, map_field_assoc} ->
            [{atom, _, MapFieldName}, {type, _, MapFieldType, []}] = TypeAttrs,
            true = is_atom(MapFieldName),
            [{map_field_assoc, MapFieldName, to_a_type({type, MapFieldType})}];
        {type, map_field_exact} ->
            [{atom, _, MapFieldName}, {type, _, MapFieldType, []}] = TypeAttrs,
            true = is_atom(MapFieldName),
            [{map_field_exact, MapFieldName, to_a_type({type, MapFieldType})}]
    end.

-spec to_a_type(term()) -> record_type_introspect:a_type().
to_a_type({type, PrimaryType} = Type) when ?is_primary_type(PrimaryType) ->
    Type;
to_a_type({literal, _Literal} = Type) ->
    Type.

-spec record_field_info(term()) -> {atom(), record_type_introspect:a_type()}.
record_field_info({typed_record_field, {record_field, _, {atom, _, FieldName}}, Type}) ->
    [TypeInfo] = field_into_to_type(Type),
    true = is_atom(FieldName),
    {FieldName, TypeInfo}.
