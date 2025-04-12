-module(erldantic_json).

-export([from_json/3, to_json/3]).

-include("../include/record_type_introspect.hrl").

% FIXME: User can get 'skip' as return value.
-spec to_json(TypeInfo :: map(), Type :: term(), Data :: term()) ->
                 {ok, term()} | {error, list()} | skip.
to_json(TypeInfo, {record, RecordName}, Record) when is_atom(RecordName) ->
    record_to_json(TypeInfo, RecordName, Record);
to_json(TypeInfo, {record_ref, RecordName}, Record) when is_atom(RecordName) ->
    record_to_json(TypeInfo, RecordName, Record);
to_json(TypeInfo, {user_type_ref, TypeName}, Data) when is_atom(TypeName) ->
    data_to_json(TypeInfo, TypeName, Data);
to_json(_TypeInfo, {type, PrimaryType}, Value) when ?is_primary_type(PrimaryType) ->
    case check_type(PrimaryType, Value) of
        true ->
            {ok, Value};
        false ->
            {error, [{type_mismatch, PrimaryType, Value}]}
    end;
to_json(_TypeInfo, {literal, undefined}, undefined) ->
    skip;
to_json(_TypeInfo, {literal, Literal}, Literal) ->
    {ok, Literal};
to_json(TypeInfo, {union, Types}, Data) ->
    first(fun to_json/3, TypeInfo, Types, Data);
to_json(TypeInfo, {type, TypeName}, Data) when is_atom(TypeName) ->
    data_to_json(TypeInfo, TypeName, Data).

data_to_json(TypeInfo, TypeName, Data) ->
    case maps:get({type, TypeName}, TypeInfo) of
        #a_rec{name = RecordName, fields = _RecordInfo} ->
            record_to_json(TypeInfo, RecordName, Data);
        #a_map{fields = MapFieldTypes} ->
            map_to_json(MapFieldTypes, Data)
    end.

map_to_json(MapFieldTypes, Data) ->
    MapFields =
        lists:zf(fun ({map_field_assoc, FieldName, _FieldType}) ->
                         case Data of
                             #{FieldName := FieldData} ->
                                 {true, {FieldName, FieldData}};
                             _ ->
                                 false
                         end;
                     ({map_field_exact, FieldName, _FieldType}) ->
                         FieldData = maps:get(FieldName, Data),
                         {true, {FieldName, FieldData}}
                 end,
                 MapFieldTypes),
    %% FIXME: Handle missing fields
    {ok, maps:from_list(MapFields)}.

record_to_json(TypeInfo, RecordName, Record) when is_tuple(Record) ->
    io:format("recorc name = ~p~n", [RecordName]),
    [RecordName | FieldsData] = tuple_to_list(Record),
    #a_rec{name = RecordName, fields = RecordInfo} = maps:get({record, RecordName}, TypeInfo),
    Mojs = lists:zip(RecordInfo, FieldsData),
    {Fields, Errors} =
        lists:foldl(fun({{FieldName, FieldType}, RecordFieldData}, {FieldsAcc, ErrorsAcc})
                           when is_atom(FieldName) ->
                       case to_json(TypeInfo, FieldType, RecordFieldData) of
                           {ok, FieldJson} ->
                               {FieldsAcc ++ [{FieldName, FieldJson}], ErrorsAcc};
                           skip ->
                               {FieldsAcc, ErrorsAcc};
                           {error, Err} ->
                               {FieldsAcc, ErrorsAcc ++ [{Err, FieldName}]}
                       end
                    end,
                    {[], []},
                    Mojs),
    case Errors of
        [] ->
            {ok, maps:from_list(Fields)};
        _ ->
            {error, Errors}
    end.

-spec from_json(TypeInfo :: map(), Type :: term(), Json :: map() | undefined) ->
                   {ok, term()} | {error, list()}.
from_json(TypeInfo, {record, RecordName}, Json) when is_atom(RecordName) ->
    record_from_json(TypeInfo, RecordName, Json);
from_json(TypeInfo, {record_ref, RecordName}, Json) when is_atom(RecordName) ->
    record_from_json(TypeInfo, RecordName, Json);
from_json(TypeInfo, #a_map{fields = MapFieldTypes}, Json) ->
    map_from_json(TypeInfo, MapFieldTypes, Json);
from_json(TypeInfo, {user_type_ref, TypeName}, Json) when is_atom(TypeName) ->
    type_from_json(TypeInfo, TypeName, Json);
from_json(_TypeInfo, {type, PrimaryType}, Json) when ?is_primary_type(PrimaryType) ->
    case check_type(PrimaryType, Json) of
        true ->
            {ok, Json};
        false ->
            {error, [{type_mismatch, PrimaryType, Json}]}
    end;
from_json(_TypeInfo, {literal, Literal}, Literal) ->
    {ok, Literal};
from_json(_TypeInfo, {type, TypeName}, undefined) ->
    {error, [{missing_type, TypeName}]};
from_json(TypeInfo, {type, TypeName}, Json) when is_atom(TypeName) ->
    type_from_json(TypeInfo, TypeName, Json);
from_json(TypeInfo, {union, Types}, Json) ->
    first(fun from_json/3, TypeInfo, Types, Json);
from_json(_TypeInfo, {range, integer, Min, Max}, Value) when Min =< Value, Value =< Max ->
    {ok, Value}.

check_type(integer, Json) when is_integer(Json) ->
    true;
check_type(string, Json) when is_binary(Json) ->
    true;
check_type(boolean, Json) when is_boolean(Json) ->
    true;
check_type(_Type, _Json) ->
    false.

first(_F, _TypeInfo, [], _Json) ->
    {error, no_match};
first(F, TypeInfo, [Type | Rest], Json) ->
    case F(TypeInfo, Type, Json) of
        skip ->
            skip;
        {ok, Result} ->
            {ok, Result};
        {error, _} ->
            first(F, TypeInfo, Rest, Json)
    end.

-spec type_from_json(TypeInfo :: map(), TypeName :: atom(), Json :: map()) ->
                        {ok, term()} | {error, list()}.
type_from_json(TypeInfo, TypeName, Json) ->
    case maps:get({type, TypeName}, TypeInfo) of
        #a_map{fields = MapFieldType} ->
            map_from_json(TypeInfo, MapFieldType, Json);
        #a_rec{name = RecordName, fields = RecordInfo} ->
            do_record_from_json(TypeInfo, RecordName, RecordInfo, Json)
    end.

map_from_json(TypeInfo, MapFieldType, Json) ->
    Fields =
        lists:zf(fun ({map_field_assoc, FieldName, FieldType}) ->
                         case maps:find(atom_to_binary(FieldName), Json) of
                             {ok, FieldData} ->
                                 case from_json(TypeInfo, FieldType, FieldData) of
                                     {ok, FieldJson} ->
                                         {true, {FieldName, FieldJson}};
                                     skip ->
                                         false
                                 end;
                             _ ->
                                 false
                         end;
                     ({map_field_exact, FieldName, FieldType}) ->
                         FieldData = maps:get(atom_to_binary(FieldName), Json),
                         {ok, FieldJson} = from_json(TypeInfo, FieldType, FieldData),
                         {true, {FieldName, FieldJson}}
                 end,
                 MapFieldType),
    %% FIXME: Handle missing fields
    {ok, maps:from_list(Fields)}.

-spec record_from_json(TypeInfo :: map(),
                       RecordName :: atom(),
                       Json :: map() | undefined) ->
                          {ok, term()} | {error, list()}.
record_from_json(TypeInfo, RecordName, Json) ->
    #a_rec{name = RecordName, fields = RecordInfo} = maps:get({record, RecordName}, TypeInfo),
    do_record_from_json(TypeInfo, RecordName, RecordInfo, Json).

-spec do_record_from_json(TypeInfo :: map(),
                          RecordName :: atom(),
                          RecordInfo :: list(),
                          Json :: map() | undefined) ->
                             {ok, term()} | {error, list()}.
do_record_from_json(_TypeInfo, RecordName, _RecordInfo, undefined) ->
    {error, [missing, RecordName]};
do_record_from_json(TypeInfo, RecordName, RecordInfo, Json) ->
    {Fields, Errors} =
        lists:foldl(fun({FieldName, FieldType}, {FieldsAcc, ErrorsAcc}) when is_atom(FieldName) ->
                       RecordFieldData = maps:get(atom_to_binary(FieldName), Json, undefined),
                       case from_json(TypeInfo, FieldType, RecordFieldData) of
                           {ok, FieldJson} ->
                               {FieldsAcc ++ [FieldJson], ErrorsAcc};
                           {error, Err} ->
                               {FieldsAcc, ErrorsAcc ++ [{Err, FieldName}]}
                       end
                    end,
                    {[], []},
                    RecordInfo),
    case Errors of
        [] ->
            {ok, list_to_tuple([RecordName | Fields])};
        _ ->
            {error, Errors}
    end.
