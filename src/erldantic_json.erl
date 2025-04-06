-module(erldantic_json).

-export([from_json/3]).

-include("../include/record_type_introspect.hrl").

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
        #a_map{fields = MapFieldType} ->
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
