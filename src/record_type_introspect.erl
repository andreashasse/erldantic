-module(record_type_introspect).

-export([parse_transform/2, record_from_json/3, from_json/3]).
%% export for test
-export([type_in_form/1]).

-type user_type_name() :: atom().

-type a_type() :: {type, string | integer} | {record_ref, user_type_name()} | {user_type_ref, user_type_name()}.

-record(a_field, {name :: atom(), type :: a_type()}).
-record(a_map, {fields :: [#a_field{}]}).
-record(a_rec, {name :: string(), fields :: [#a_field{}]}).

from_json(TypeInfo, {record, RecordName}, Json) ->
    record_from_json(TypeInfo, RecordName, Json);
from_json(TypeInfo, {record_ref, RecordName}, Json) ->
    record_from_json(TypeInfo, RecordName, Json);
from_json(TypeInfo, {user_type_ref, TypeName}, Json) ->
    type_from_json(TypeInfo, TypeName, Json);
from_json(_TypeInfo, {type, integer}, Json) ->
    Json;
from_json(_TypeInfo, {type, string}, Json) ->
    Json;
from_json(TypeInfo, {type, TypeName}, Json) ->
    type_from_json(TypeInfo, TypeName, Json).

type_from_json(TypeInfo, TypeName, Json) ->
    case maps:get({type, TypeName}, TypeInfo) of
        {map, MapFieldType} ->
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
                     RecordFieldData = maps:get(atom_to_binary(FieldName), Json),
                     from_json(TypeInfo, FieldType, RecordFieldData)
                  end,
                  RecordInfo),
    list_to_tuple([RecordName | Fields]).

parse_transform(Forms, _Options) ->
    io:format("Inspecting records at compile-time...~n"),
    RecordInfo =
        maps:from_list(
            lists:filtermap(fun type_in_form/1, Forms)),
    io:format("Record information:~n~p~n", [RecordInfo]),
    Forms.

type_in_form({attribute, _, record, {RecordName, Fields}}) ->
    FieldInfos = lists:map(fun record_field_info/1, Fields),
    {true, {{record, RecordName}, #a_rec{name = RecordName, fields = FieldInfos}}};
type_in_form({attribute, _, type, {TypeName, {type, _, record, Attrs}, []}}) ->
    [{atom, _, RecordName} | FieldInfo] = Attrs,
    FieldTypes =
        lists:map(fun({type, _, field_type, [{atom, _, FieldName}, {type, _, FieldType, []}]}) ->
                     {FieldName, {type, FieldType}}
                  end,
                  FieldInfo),
    {true, {{type, TypeName}, #a_rec{name = RecordName, fields = FieldTypes}}};
type_in_form({attribute, _, type, {TypeName, {type, _, map, Attrs}, []}}) ->
    FieldInfos = lists:flatmap(fun map_field_info/1, Attrs),
    {true, {{type, TypeName}, #a_map{fields = FieldInfos}}};
type_in_form({attribute, _, type, {TypeName, {type, _, Type, Attrs}, []}}) ->
    FieldInfos = lists:flatmap(fun type_field_info/1, Attrs),
    {true, {{type, TypeName}, {Type, FieldInfos}}};
type_in_form(_) ->
    false.

-spec type_field_info(term()) -> [a_type()].
type_field_info({ann_type, _, [{var, _, _VarName}, {type, _, _TypeAnnType, []}]}) ->
    [];
type_field_info({TypeOfType, _, Type, TypeAttrs}) ->
    case {TypeOfType, Type} of
        {type, record} ->
            [{atom, _, SubTypeRecordName}] = TypeAttrs,
            [{record_ref, SubTypeRecordName}];
        {user_type, Type} ->
            [{user_type_ref, Type}];
        {type, field_type} ->
            [{atom, _, FieldName}, {type, _, FieldType, []}] = TypeAttrs,
            [{FieldName, FieldType}];
        {type, map} ->
            MapFields = lists:flatmap(fun map_field_info/1, TypeAttrs),
            [#a_map{fields = MapFields}];
        {type, tuple} ->
            TupleFields = lists:flatmap(fun type_field_info/1, TypeAttrs),
            [{tuple, TupleFields}];
        {type, string} ->
            [{type, string}];
        {type, integer} ->
            [{type, integer}];
        {type, union} ->
            UnionFields =
                lists:map(fun ({UTypeOfType, _, UnionType, []}) ->
                                  {UTypeOfType, UnionType};
                              ({UTypeOfType, _, UnionType}) ->
                                  {UTypeOfType, UnionType}
                          end,
                          TypeAttrs),
            [{union, UnionFields}]
    end.

map_field_info({TypeOfType, _, Type, TypeAttrs}) ->
    case {TypeOfType, Type} of
        {type, map_field_assoc} ->
            [{atom, _, MapFieldName}, {type, _, MapFieldType, []}] = TypeAttrs,
            [{map_field_assoc, MapFieldName, MapFieldType}];
        {type, map_field_exact} ->
            [{atom, _, MapFieldName}, {type, _, MapFieldType, []}] = TypeAttrs,
            [{map_field_exact, MapFieldName, MapFieldType}]
    end.

-spec record_field_info(term()) -> #a_field{}.
record_field_info({typed_record_field, {record_field, _, {atom, _, FieldName}}, Type}) ->
    [TypeInfo] = type_field_info(Type),
    true = is_atom(FieldName),
    #a_field{name = FieldName, type = TypeInfo}.
