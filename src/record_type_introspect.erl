-module(record_type_introspect).

-include("../include/record_type_introspect.hrl").

-compile({parse_transform, parse_trans_codegen}).

-export([parse_transform/2, record_from_json/3, from_json/3]).
%% export for test
-export([type_in_form/1]).

-type user_type_name() :: atom().
-type a_type() ::
    {type, string | integer | boolean} |
    {record_ref, user_type_name()} |
    {user_type_ref, user_type_name()} |
    #a_map{} |
    #a_rec{} |
    #a_tuple{} |
    {union, [a_type()]} |
    {literal, term()}.

-export_type([a_type/0]).

-define(is_primary_type(PrimaryType),
        PrimaryType =:= string orelse PrimaryType =:= integer orelse PrimaryType =:= boolean).

from_json(TypeInfo, {record, RecordName}, Json) ->
    record_from_json(TypeInfo, RecordName, Json);
from_json(TypeInfo, {record_ref, RecordName}, Json) ->
    record_from_json(TypeInfo, RecordName, Json);
from_json(TypeInfo, {user_type_ref, TypeName}, Json) ->
    type_from_json(TypeInfo, TypeName, Json);
from_json(_TypeInfo, {type, PrimaryType}, Json) when ?is_primary_type(PrimaryType) ->
    Json;
from_json(TypeInfo, {type, TypeName}, Json) ->
    type_from_json(TypeInfo, TypeName, Json).

type_from_json(TypeInfo, TypeName, Json) ->
    case maps:get({type, TypeName}, TypeInfo) of
        #a_map{fields =  MapFieldType} ->
            Fields =
                lists:zf(fun ({map_field_assoc, FieldName, _FieldType}) ->
                                 case maps:find(atom_to_binary(FieldName), Json) of
                                     {ok, FieldData} ->
                                         {true, {FieldName, FieldData}};
                                     _ ->
                                         false
                                 end;
                             ({map_field_exact, FieldName, _FieldType}) ->
                                 FieldData = maps:get(atom_to_binary(FieldName), Json),
                                 {true, {FieldName, FieldData}}
                         end,
                         MapFieldType),
            maps:from_list(Fields);
        #a_rec{name = RecordName, fields = RecordInfo} ->
            do_record_from_json(TypeInfo, RecordName, RecordInfo, Json)
    end.

record_from_json(TypeInfo, RecordName, Json) ->
    #a_rec{name = RecordName, fields = RecordInfo} = maps:get({record, RecordName}, TypeInfo),
    do_record_from_json(TypeInfo, RecordName, RecordInfo, Json).

do_record_from_json(TypeInfo, RecordName, RecordInfo, Json) ->
    Fields =
        lists:map(fun({FieldName, FieldType}) ->
                     true = is_atom(FieldName),
                     RecordFieldData = maps:get(atom_to_binary(FieldName), Json),
                     from_json(TypeInfo, FieldType, RecordFieldData)
                  end,
                  RecordInfo),
    list_to_tuple([RecordName | Fields]).

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
    lists:map(fun (A) -> named_type_to_function(A, M) end, NamedTypes).

named_types_to_exports(NamedTypes) ->
    Exports =
        lists:map(fun({{_, TypeName}, _}) -> {function_name(TypeName), 1} end, NamedTypes),
    [{attribute, {5, 2}, export, Exports}].

named_type_to_function({{TypeOfThing, TypeName}, _Info}, Infos) ->
    codegen:gen_function(function_name(TypeName),
                         fun(Json) ->
                            record_type_introspect:from_json({'$var', Infos},
                                                             {{'$var', TypeOfThing}, {'$var', TypeName}},
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
    FieldInfos = lists:flatmap(fun type_field_info/1, Attrs),
    {true, {{type, TypeName}, to_a_type({Type, FieldInfos})}};
type_in_form(_) ->
    false.

-spec type_field_info(term()) -> [a_type()].
type_field_info({ann_type, _, [{var, _, _VarName}, {type, _, _TypeAnnType, []}]}) ->
    [];
type_field_info({TypeOfType, _, Type, TypeAttrs}) ->
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
            TupleFields = lists:flatmap(fun type_field_info/1, TypeAttrs),
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

-spec map_field_info(term()) -> [{map_field_assoc | map_field_exact, atom(), a_type()}].
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

-spec to_a_type(term()) -> a_type().
to_a_type({type, PrimaryType} = Type) when ?is_primary_type(PrimaryType) ->
    Type;
to_a_type({literal, _Literal} = Type) ->
    Type.

-spec record_field_info(term()) -> {atom(), a_type()}.
record_field_info({typed_record_field, {record_field, _, {atom, _, FieldName}}, Type}) ->
    [TypeInfo] = type_field_info(Type),
    true = is_atom(FieldName),
    {FieldName, TypeInfo}.
