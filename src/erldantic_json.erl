-module(erldantic_json).

-export([from_json/3]).

-include("../include/record_type_introspect.hrl").

-spec from_json(TypeInfo :: map(), Type :: term(), Json :: map() | undefined) ->
                   {ok, term()} | {error, list()}.
from_json(TypeInfo, {record, RecordName}, Json) when is_atom(RecordName) ->
    record_from_json(TypeInfo, RecordName, Json);
from_json(TypeInfo, {record_ref, RecordName}, Json) when is_atom(RecordName) ->
    record_from_json(TypeInfo, RecordName, Json);
from_json(TypeInfo, {user_type_ref, TypeName}, Json) when is_atom(TypeName) ->
    type_from_json(TypeInfo, TypeName, Json);
from_json(_TypeInfo, {type, PrimaryType}, Json) when ?is_primary_type(PrimaryType) ->
    case check_type(PrimaryType, Json) of
        true ->
            {ok, Json};
        false ->
            {error, [{type_mismatch, PrimaryType}]}
    end;
from_json(_TypeInfo, {literal, Literal}, Literal) ->
    {ok, Literal};
from_json(_TypeInfo, {type, TypeName}, undefined) ->
    {error, [{missing_type, TypeName}]};
from_json(TypeInfo, {type, TypeName}, Json) when is_atom(TypeName) ->
    type_from_json(TypeInfo, TypeName, Json);
from_json(TypeInfo, {union, Types}, Json) ->
    first(TypeInfo, Types, Json).

check_type(integer, Json) when is_integer(Json) ->
    true;
check_type(string, Json) when is_binary(Json) ->
    true;
check_type(boolean, Json) when is_boolean(Json) ->
    true;
check_type(_Type, _Json) ->
    false.

first(_TypeInfo, [], _Json) ->
    {error, no_match};
first(TypeInfo, [Type | Rest], Json) ->
    case from_json(TypeInfo, Type, Json) of
        {ok, Result} ->
            {ok, Result};
        {error, _} ->
            first(TypeInfo, Rest, Json)
    end.

-spec type_from_json(TypeInfo :: map(), TypeName :: atom(), Json :: map()) ->
                        {ok, term()} | {error, list()}.
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
            %% FIXME: Handle missing fields
            {ok, maps:from_list(Fields)};
        #a_rec{name = RecordName, fields = RecordInfo} ->
            do_record_from_json(TypeInfo, RecordName, RecordInfo, Json)
    end.

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
                           {error, _} ->
                               {FieldsAcc, ErrorsAcc ++ [{unknown, FieldName}]}
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
