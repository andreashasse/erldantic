-module(record_type_introspect).

-export([parse_transform/2, record_from_json/3, test_address/0, test_name_t/0,
         test_person/0]).

type_info() ->
    % FIXME: I copy the output I get when compiling person.erl here
    #{{record, address} => [{street, {type, string}}, {city, {type, string}}],
      {record, person} =>
          [{name, {user_type_ref, name_t}}, {age, {type, integer}}, {home, {record_ref, address}}],
      {type, name_t} =>
          {map, [{map_field_assoc, first, string}, {map_field_exact, last, string}]}}.

test_person() ->
    Json =
        json:decode(<<"{\"name\": {\"first\": \"Andreas\", \"last\": \"Hasselberg\"}, "
                      "\"age\": 22, \"home\": {\"street\": \"mojs\", \"city\": \"sollentuna\""
                      "}}"/utf8>>),
    {person, Name, Age, Home} = Person = record_from_json(type_info(), person, Json),
    #{first := <<"Andreas">>, last := <<"Hasselberg">>} = Name,
    22 = Age,
    {address, <<"mojs">>, <<"sollentuna">>} = Home,
    Person.

test_address() ->
    Json = json:decode(<<"{\"street\": \"mojs\", \"city\": \"sollentuna\"}"/utf8>>),
    {address, <<"mojs">>, <<"sollentuna">>} = from_json(type_info(), {record, address}, Json).

test_name_t() ->
    Json =
        json:decode(<<"{\"first\": \"Andreas\", \"last\": \"Hasselberg\", \"not_present\": "
                      "22}"/utf8>>),
    #{first := <<"Andreas">>, last := <<"Hasselberg">>} =
        M = from_json(type_info(), {type, name_t}, Json),
    [first, last] = maps:keys(M),
    M.

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

record_from_json(TypeInfo, RecordName, Json) ->
    RecordInfo = maps:get({record, RecordName}, TypeInfo),
    Fields =
        lists:map(fun({FieldName, FieldType}) ->
                     RecordFieldData = maps:get(atom_to_binary(FieldName), Json),
                     from_json(TypeInfo, FieldType, RecordFieldData)
                  end,
                  RecordInfo),
    list_to_tuple([RecordName | Fields]).

type_from_json(Data, TypeName, Json) ->
    case maps:get({type, TypeName}, Data) of
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
            maps:from_list(Fields)
    end.

parse_transform(Forms, _Options) ->
    io:format("Inspecting records at compile-time...~n"),
    RecordInfo =
        maps:from_list(
            lists:filtermap(fun type_in_form/1, Forms)),
    io:format("Record information:~n~p~n", [RecordInfo]),
    Forms.

type_in_form({attribute, _, record, {RecordName, Fields}}) ->
    FieldInfos = lists:map(fun record_field_info/1, Fields),
    {true, {{record, RecordName}, FieldInfos}};
type_in_form({attribute, _, type, {TypeName, {type, _, Type, Attrs}, []}} = A) ->
    io:format("Type: ~p~n", [A]),
    FieldInfos = lists:flatmap(fun type_field_info/1, Attrs),
    {true, {{type, TypeName}, {Type, FieldInfos}}};
type_in_form(_) ->
    false.

-spec type_field_info(any()) -> list().
type_field_info({ann_type, _, [{var, _, _VarName}, {type, _, _TypeAnnType, []}]}) ->
    [];
type_field_info({TypeOfType, _, Type, TypeAttrs}) ->
    case {TypeOfType, Type} of
        {type, record} ->
            [{atom, _, SubTypeRecordName}] = TypeAttrs,
            [{record_ref, SubTypeRecordName}];
        {user_type, Type} ->
            [{user_type_ref, Type}];
        {type, map_field_assoc} ->
            [{atom, _, MapFieldName}, {type, _, MapFieldType, []}] = TypeAttrs,
            [{map_field_assoc, MapFieldName, MapFieldType}];
        {type, map_field_exact} ->
            [{atom, _, MapFieldName}, {type, _, MapFieldType, []}] = TypeAttrs,
            [{map_field_exact, MapFieldName, MapFieldType}];
        {type, map} ->
            MapFields = lists:flatmap(fun type_field_info/1, TypeAttrs),
            [{map, MapFields}];
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

record_field_info({typed_record_field, {record_field, _, {atom, _, FieldName}}, Type}) ->
    [TypeInfo] = type_field_info(Type),
    {FieldName, TypeInfo}.
