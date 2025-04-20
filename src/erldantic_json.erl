-module(erldantic_json).

-export([from_json/3, to_json/3]).

-include("../include/record_type_introspect.hrl").

-type key() :: any(). %% fixme
-type json__encode_value() :: term(). %% Should be json:encode_value()
-type json() :: json:decode_value().

% FIXME: User can get 'skip' as return value.
-spec to_json(TypeInfo :: #{key() => record_type_introspect:a_type()},
              Type :: record_type_introspect:a_type_or_ref(),
              Data :: term()) ->
                 {ok, json__encode_value()} | {error, [#ed_error{}]} | skip.
to_json(TypeInfo, {record, RecordName}, Record) when is_atom(RecordName) ->
    record_to_json(TypeInfo, RecordName, Record);
to_json(TypeInfo, {record_ref, RecordName}, Record) when is_atom(RecordName) ->
    record_to_json(TypeInfo, RecordName, Record);
to_json(TypeInfo, {user_type_ref, TypeName}, Data) when is_atom(TypeName) ->
    data_to_json(TypeInfo, TypeName, Data);
to_json(_TypeInfo, {type, Type} = T, Value)
    when ?is_primary_type(Type) orelse ?is_predefined_int_range(Type) ->
    case check_type(Type, Value) of
        true ->
            {ok, Value};
        false ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => T, value => Value}}]}
    end;
to_json(_TypeInfo, {range, integer, Min, Max}, Value)
    when is_integer(Value) andalso Min =< Value, Value =< Max ->
    {ok, Value};
to_json(_TypeInfo, {literal, undefined}, undefined) ->
    skip;
to_json(_TypeInfo, {literal, Literal}, Literal) ->
    {ok, Literal};
to_json(TypeInfo, {union, Types}, Data) ->
    first(fun to_json/3, TypeInfo, Types, Data);
to_json(TypeInfo, {list, Type}, Data) when is_list(Data) ->
    list_to_json(TypeInfo, Type, Data);
to_json(TypeInfo, {type, TypeName}, Data) when is_atom(TypeName) ->
    data_to_json(TypeInfo, TypeName, Data);
to_json(TypeInfo, #a_map{fields = MapFieldTypes}, Data) ->
    map_to_json(TypeInfo, MapFieldTypes, Data);
to_json(_TypeInfo, T, OtherValue) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => T, value => OtherValue}}]}.

-spec list_to_json(TypeInfo :: map(),
                   Type :: record_type_introspect:a_type_or_ref(),
                   Data :: [term()]) ->
                      {ok, [json__encode_value()]} | {error, [#ed_error{}]}.
list_to_json(TypeInfo, Type, Data) when is_list(Data) ->
    JsonRes = lists:map(fun(Item) -> to_json(TypeInfo, Type, Item) end, Data),
    {AllOk, Errors} =
        lists:partition(fun ({ok, _}) ->
                                true;
                            (_) ->
                                false
                        end,
                        JsonRes),
    case Errors of
        [] ->
            {ok, lists:map(fun({ok, Json}) -> Json end, AllOk)};
        _ ->
            {error, lists:flatmap(fun({error, Errs}) -> Errs end, Errors)}
    end.

data_to_json(TypeInfo, TypeName, Data) ->
    case TypeInfo of
        #{{type, TypeName} := Mojs} ->
            to_json(TypeInfo, Mojs, Data);
        #{} ->
            {error, [#ed_error{type = missing_type, location = [TypeName]}]}
    end.

map_to_json(TypeInfo, MapFieldTypes, Data) ->
    {MapFields, Errors} =
        lists:foldl(fun ({map_field_assoc, FieldName, FieldType}, {FieldsAcc, ErrorsAcc}) ->
                            case Data of
                                #{FieldName := FieldData} ->
                                    %% ADD test case for bad and good data in map
                                    case to_json(TypeInfo, FieldType, FieldData) of
                                        {ok, FieldJson} ->
                                            {FieldsAcc ++ [{FieldName, FieldJson}], ErrorsAcc};
                                        {error, Errs} ->
                                            Errs2 =
                                                lists:map(fun(Err) ->
                                                             err_append_location(Err, FieldName)
                                                          end,
                                                          Errs),
                                            {FieldsAcc, ErrorsAcc ++ Errs2}
                                    end;
                                _ ->
                                    {FieldsAcc, ErrorsAcc}
                            end;
                        ({map_field_exact, FieldName, FieldType}, {FieldsAcc, ErrorsAcc}) ->
                            case Data of
                                #{FieldName := FieldData} ->
                                    %% ADD test case for bad and good data in map
                                    case to_json(TypeInfo, FieldType, FieldData) of
                                        {ok, FieldJson} ->
                                            {FieldsAcc ++ [{FieldName, FieldJson}], ErrorsAcc};
                                        {error, Errs} ->
                                            Errs2 =
                                                lists:map(fun(Err) ->
                                                             err_append_location(Err, FieldName)
                                                          end,
                                                          Errs),
                                            {FieldsAcc, ErrorsAcc ++ Errs2}
                                    end;
                                #{} ->
                                    case can_be_undefined(FieldType) of
                                        true ->
                                            %% ADD test case
                                            {FieldsAcc ++ [{FieldName, undefined}], ErrorsAcc};
                                        false ->
                                            %% ADD test case
                                            {FieldsAcc,
                                             ErrorsAcc
                                             ++ [#ed_error{type = missing_data,
                                                           location = [FieldName]}]}
                                    end
                            end
                    end,
                    {[], []},
                    MapFieldTypes),
    case Errors of
        [] ->
            {ok, maps:from_list(MapFields)};
        _ ->
            {error, Errors}
    end.

-spec record_to_json(TypeInfo :: map(), RecordName :: atom(), Record :: term()) ->
                        {ok, #{atom() => json__encode_value()}} | {error, [#ed_error{}]}.
record_to_json(TypeInfo, RecordName, Record) when is_tuple(Record) ->
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
                           {error, Errs} ->
                               Errs2 =
                                   lists:map(fun(Err) -> err_append_location(Err, FieldName) end,
                                             Errs),
                               {FieldsAcc, ErrorsAcc ++ Errs2}
                       end
                    end,
                    {[], []},
                    Mojs),
    case Errors of
        [] ->
            {ok, maps:from_list(Fields)};
        _ ->
            {error, Errors}
    end;
record_to_json(_TypeInfo, RecordName, Record) ->
    {error,
     [#ed_error{type = record_type_mismatch,
                location = [RecordName],
                ctx = #{record_name => RecordName, record => Record}}]}.

err_append_location(Err, FieldName) ->
    Err#ed_error{location = [FieldName | Err#ed_error.location]}.

-spec from_json(TypeInfo :: map(), Type :: term(), Json :: json()) ->
                   {ok, term()} | {error, [#ed_error{}]}.
from_json(TypeInfo, {record, RecordName}, Json) when is_atom(RecordName) ->
    record_from_json(TypeInfo, RecordName, Json);
from_json(TypeInfo, {record_ref, RecordName}, Json) when is_atom(RecordName) ->
    record_from_json(TypeInfo, RecordName, Json);
from_json(TypeInfo, #a_map{fields = MapFieldTypes}, Json) ->
    map_from_json(TypeInfo, MapFieldTypes, Json);
from_json(TypeInfo, {user_type_ref, TypeName}, Json) when is_atom(TypeName) ->
    type_from_json(TypeInfo, TypeName, Json);
from_json(TypeInfo, {list, Type}, Data) ->
    list_from_json(TypeInfo, Type, Data);
from_json(_TypeInfo, {type, PrimaryType} = T, Json)
    when ?is_primary_type(PrimaryType) orelse ?is_predefined_int_range(PrimaryType) ->
    case check_type(PrimaryType, Json) of
        true ->
            {ok, Json};
        false ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => T, value => Json}}]}
    end;
from_json(_TypeInfo, {literal, Literal}, Literal) ->
    {ok, Literal};
from_json(_TypeInfo, {literal, Literal} = T, Value) ->
    case try_convert_to_literal(Literal, Value) of
        {ok, Literal} ->
            {ok, Literal};
        false ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => T, value => Value}}]}
    end;
from_json(TypeInfo, {type, TypeName}, Json) when is_atom(TypeName) ->
    type_from_json(TypeInfo, TypeName, Json);
from_json(TypeInfo, {union, Types}, Json) ->
    first(fun from_json/3, TypeInfo, Types, Json);
from_json(_TypeInfo, {range, integer, Min, Max}, Value) when Min =< Value, Value =< Max ->
    {ok, Value};
from_json(_TypeInfo, {range, integer, Min, Max}, Value) when is_integer(Value) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => {range, integer, Min, Max}, value => Value}}]}.

try_convert_to_literal(Literal, Value) when is_atom(Literal) andalso is_binary(Value) ->
    try binary_to_existing_atom(Value, utf8) of
        Literal when is_atom(Literal) ->
            {ok, Literal};
        _ ->
            false
    catch
        error:badarg ->
            false
    end;
try_convert_to_literal(_Literal, _Value) ->
    false.

list_from_json(TypeInfo, Type, Data) ->
    JsonRes =
        lists:map(fun({Nr, Item}) ->
                     case from_json(TypeInfo, Type, Item) of
                         {ok, Json} ->
                             {ok, Json};
                         {error, Errs} ->
                             Errs2 = lists:map(fun(Err) -> err_append_location(Err, Nr) end, Errs),
                             {error, Errs2}
                     end
                  end,
                  lists:enumerate(Data)),
    {AllOk, Errors} =
        lists:partition(fun ({ok, _}) ->
                                true;
                            (_) ->
                                false
                        end,
                        JsonRes),
    case Errors of
        [] ->
            {ok, lists:map(fun({ok, Json}) -> Json end, AllOk)};
        _ ->
            {error, lists:flatmap(fun({error, EdErrors}) -> EdErrors end, Errors)}
    end.

check_type(integer, Json) when is_integer(Json) ->
    true;
check_type(string, Json) when is_binary(Json) ->
    true;
check_type(boolean, Json) when is_boolean(Json) ->
    true;
check_type(float, Json) when is_float(Json) ->
    true;
check_type(non_neg_integer, Json) when is_integer(Json) andalso Json >= 0 ->
    true;
check_type(pos_integer, Json) when is_integer(Json) andalso Json > 0 ->
    true;
check_type(neg_integer, Json) when is_integer(Json) andalso Json < 0 ->
    true;
check_type(atom, Json) when is_atom(Json) ->
    true;
check_type(_Type, _Json) ->
    false.

first(F, TypeInfo, Types, Json) ->
    case do_first(F, TypeInfo, Types, Json) of
        {error, no_match} ->
            {error,
             [#ed_error{type = no_match,
                        location = [],
                        ctx = #{type => Types, value => Json}}]};
        Result ->
            Result
    end.

do_first(_F, _TypeInfo, [], _Json) ->
    {error, no_match};
do_first(F, TypeInfo, [Type | Rest], Json) ->
    case F(TypeInfo, Type, Json) of
        skip ->
            skip;
        {ok, Result} ->
            {ok, Result};
        {error, _} ->
            do_first(F, TypeInfo, Rest, Json)
    end.

-spec type_from_json(TypeInfo :: map(), TypeName :: atom(), Json :: json()) ->
                        {ok, term()} | {error, [#ed_error{}]}.
type_from_json(TypeInfo, TypeName, Json) ->
    case TypeInfo of
        #{{type, TypeName} := Type} ->
            from_json(TypeInfo, Type, Json);
        #{} ->
            {error, [#ed_error{type = missing_type, location = [TypeName]}]}
    end.

map_from_json(TypeInfo, MapFieldType, Json) ->
    {Fields, Errors} =
        lists:foldl(fun ({map_field_assoc, FieldName, FieldType}, {FieldsAcc, ErrAcc}) ->
                            case maps:find(atom_to_binary(FieldName), Json) of
                                {ok, FieldData} ->
                                    case from_json(TypeInfo, FieldType, FieldData) of
                                        {ok, FieldJson} ->
                                            {FieldsAcc ++ [{FieldName, FieldJson}], ErrAcc};
                                        {error, Errs} ->
                                            Errs2 =
                                                lists:map(fun(Err) ->
                                                             err_append_location(Err, FieldName)
                                                          end,
                                                          Errs),
                                            {FieldsAcc, ErrAcc ++ Errs2}
                                    end;
                                error ->
                                    {FieldsAcc, ErrAcc}
                            end;
                        ({map_field_exact, FieldName, FieldType}, {FieldsAcc, ErrAcc}) ->
                            case maps:find(atom_to_binary(FieldName), Json) of
                                {ok, FieldData} ->
                                    case from_json(TypeInfo, FieldType, FieldData) of
                                        {ok, FieldJson} ->
                                            {FieldsAcc ++ [{FieldName, FieldJson}], ErrAcc};
                                        {error, Errs} ->
                                            Errs2 =
                                                lists:map(fun(Err) ->
                                                             err_append_location(Err, FieldName)
                                                          end,
                                                          Errs),
                                            {FieldsAcc, ErrAcc ++ Errs2}
                                    end;
                                error ->
                                    %% Should check if can be undefined?
                                    {FieldsAcc,
                                     ErrAcc
                                     ++ [#ed_error{type = missing_data, location = [FieldName]}]}
                            end
                    end,
                    {[], []},
                    MapFieldType),
    %% FIXME: Handle missing fields
    if Errors =:= [] ->
           {ok, maps:from_list(Fields)};
       true ->
           {error, Errors}
    end.

-spec record_from_json(TypeInfo :: map(), RecordName :: atom(), Json :: json()) ->
                          {ok, term()} | {error, list()}.
record_from_json(TypeInfo, RecordName, Json) ->
    #a_rec{name = RecordName, fields = RecordInfo} = maps:get({record, RecordName}, TypeInfo),
    do_record_from_json(TypeInfo, RecordName, RecordInfo, Json).

-spec do_record_from_json(TypeInfo :: map(),
                          RecordName :: atom(),
                          RecordInfo :: list(),
                          Json :: json()) ->
                             {ok, term()} | {error, list()}.
do_record_from_json(TypeInfo, RecordName, RecordInfo, Json) when is_map(Json) ->
    {Fields, Errors} =
        lists:foldl(fun({FieldName, FieldType}, {FieldsAcc, ErrorsAcc}) when is_atom(FieldName) ->
                       case maps:find(atom_to_binary(FieldName), Json) of
                           {ok, RecordFieldData} ->
                               case from_json(TypeInfo, FieldType, RecordFieldData) of
                                   {ok, FieldJson} ->
                                       {FieldsAcc ++ [FieldJson], ErrorsAcc};
                                   {error, Errs} ->
                                       Errs2 =
                                           lists:map(fun(Err) -> err_append_location(Err, FieldName)
                                                     end,
                                                     Errs),
                                       {FieldsAcc, ErrorsAcc ++ Errs2}
                               end;
                           error ->
                               case can_be_undefined(FieldType) of
                                   true ->
                                       {FieldsAcc ++ [undefined], ErrorsAcc};
                                   false ->
                                       {FieldsAcc,
                                        ErrorsAcc
                                        ++ [#ed_error{type = missing_data, location = [FieldName]}]}
                               end
                       end
                    end,
                    {[], []},
                    RecordInfo),
    case Errors of
        [] ->
            {ok, list_to_tuple([RecordName | Fields])};
        _ ->
            {error, Errors}
    end;
do_record_from_json(_TypeInfo, RecordName, _RecordInfo, Json) ->
    {error,
     [#ed_error{type = json_type_mismatch,
                location = [RecordName],
                ctx = #{record_name => RecordName, record => Json}}]}.

can_be_undefined(Type) ->
    case Type of
        {union, Types} ->
            lists:member({literal, undefined}, Types);
        {literal, undefined} ->
            true;
        _ ->
            false
    end.
