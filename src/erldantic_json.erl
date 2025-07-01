-module(erldantic_json).

-export([type_to_json/4, type_from_json/4, record_to_json/3, record_from_json/3]).

-ignore_xref([{erldantic_json, type_to_json, 4},
              {erldantic_json, type_from_json, 4},
              {erldantic_json, record_to_json, 3},
              {erldantic_json, record_from_json, 3}]).

-include("../include/record_type_introspect.hrl").

%% API
-spec type_to_json(Module :: module(),
                   TypeName :: atom(),
                   TypeArity :: integer(),
                   Value :: dynamic()) ->
                      {ok, json:encode_value()} | {error, [erldantic:error()]}.
type_to_json(Module, TypeName, TypeArity, Value)
    when is_atom(Module) andalso is_atom(TypeName) andalso is_integer(TypeArity) ->
    TypeRef = {type, TypeName, TypeArity},
    to_json_no_pt(Module, TypeRef, Value).

-spec type_from_json(Module :: module(),
                     TypeName :: atom(),
                     TypeArity :: integer(),
                     Json :: json:decode_value()) ->
                        {ok, dynamic()} | {error, [erldantic:error()]}.
type_from_json(Module, TypeName, TypeArity, Json)
    when is_atom(Module) andalso is_atom(TypeName) andalso is_integer(TypeArity) ->
    TypeRef = {type, TypeName, TypeArity},
    from_json_no_pt(Module, TypeRef, Json).

-spec record_to_json(Module :: module(), RecordName :: atom(), Value :: dynamic()) ->
                        {ok, json:encode_value()} | {error, [erldantic:error()]}.
record_to_json(Module, RecordName, Value)
    when is_atom(Module) andalso is_atom(RecordName) ->
    to_json_no_pt(Module, {record, RecordName}, Value).

-spec record_from_json(Module :: module(),
                       RecordName :: atom(),
                       Json :: json:decode_value()) ->
                          {ok, dynamic()} | {error, [erldantic:error()]}.
record_from_json(Module, RecordName, Json)
    when is_atom(Module) andalso is_atom(RecordName) ->
    from_json_no_pt(Module, {record, RecordName}, Json).

%% INTERNAL

-spec to_json_no_pt(Module :: module(),
                    TypeRef :: erldantic:a_type_or_ref(),
                    Data :: dynamic()) ->
                       {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json_no_pt(Module, TypeRef, Data) ->
    case erldantic_module_types:get(Module) of
        {ok, TypeInfo} ->
            to_json(TypeInfo, TypeRef, Data);
        {error, _} = Err ->
            Err
    end.

to_json(TypeInfo, Type, Data) ->
    case do_to_json(TypeInfo, Type, Data) of
        {ok, Json} ->
            {ok, Json};
        skip ->
            {ok, undefined};
        {error, Errs} ->
            {error, Errs}
    end.

-spec from_json_no_pt(Module :: module(),
                      TypeOrRecord :: erldantic:a_type_or_ref(),
                      Json :: json:decode_value()) ->
                         {ok, dynamic()} | {error, [erldantic:error()]}.
from_json_no_pt(Module, TypeRef, Json) ->
    case erldantic_module_types:get(Module) of
        {ok, TypeInfo} ->
            from_json(TypeInfo, TypeRef, Json);
        {error, _} = Err ->
            Err
    end.

% FIXME: User can get 'skip' as return value.
-spec do_to_json(TypeInfo :: erldantic:type_info(),
                 Type :: erldantic:a_type_or_ref(),
                 Data :: term()) ->
                    {ok, json:encode_value()} | {error, [erldantic:error()]} | skip.
do_to_json(TypeInfo, {record, RecordName}, Record) when is_atom(RecordName) ->
    record_to_json(TypeInfo, RecordName, Record, []);
do_to_json(TypeInfo, #a_rec{fields = Fields}, Record) when is_tuple(Record) ->
    [_RecordName | Values] = tuple_to_list(Record),
    Mojs = lists:zip(Fields, Values),
    do_record_to_json(TypeInfo, Mojs);
do_to_json(TypeInfo, {record_ref, RecordName, TypeArgs}, Record)
    when is_atom(RecordName) ->
    record_to_json(TypeInfo, RecordName, Record, TypeArgs);
do_to_json(TypeInfo, {user_type_ref, TypeName, TypeArgs}, Data) when is_atom(TypeName) ->
    TypeArity = length(TypeArgs),
    case TypeInfo of
        #{{type, TypeName, TypeArity} := Type} ->
            TypeWithoutVars = apply_args(TypeInfo, Type, TypeArgs),
            do_to_json(TypeInfo, TypeWithoutVars, Data);
        #{} ->
            {error, [#ed_error{type = missing_type, location = []}]}
    end;
do_to_json(_TypeInfo, {type, Type} = T, Value)
    when ?is_primary_type(Type) orelse ?is_predefined_int_range(Type) ->
    prim_type_to_json(T, Value);
do_to_json(_TypeInfo, {range, integer, Min, Max}, Value)
    when is_integer(Value) andalso Min =< Value, Value =< Max ->
    {ok, Value};
do_to_json(_TypeInfo, {literal, undefined}, undefined) ->
    skip;
do_to_json(_TypeInfo, {literal, Value}, Value) ->
    literal_to_json(Value);
do_to_json(TypeInfo, {union, _} = T, Data) ->
    union(fun do_to_json/3, TypeInfo, T, Data);
do_to_json(TypeInfo, {nonempty_list, Type}, Data) ->
    nonempty_list_to_json(TypeInfo, Type, Data);
do_to_json(TypeInfo, {list, Type}, Data) when is_list(Data) ->
    list_to_json(TypeInfo, Type, Data);
do_to_json(TypeInfo, {type, TypeName, TypeArity}, Data) when is_atom(TypeName) ->
    %% FIXME: For simple types without arity, default to 0
    data_to_json(TypeInfo, TypeName, TypeArity, Data);
do_to_json(TypeInfo, #a_type{type = Type, vars = _ArgsNames}, Data) ->
    do_to_json(TypeInfo, Type, Data);
do_to_json(TypeInfo, #a_map{} = Map, Data) ->
    map_to_json(TypeInfo, Map, Data);
do_to_json(_TypeInfo, #a_tuple{} = T, _Data) ->
    {error,
     [#ed_error{type = type_not_supported,
                location = [],
                ctx = #{type => T}}]};
do_to_json(_TypeInfo, #remote_type{mfargs = {Module, TypeName, Args}}, Data) ->
    case erldantic_module_types:get(Module) of
        {ok, TypeInfo} ->
            TypeArity = length(Args),
            case TypeInfo of
                #{{type, TypeName, TypeArity} := Type} ->
                    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
                    do_to_json(TypeInfo, TypeWithoutVars, Data);
                #{} ->
                    {error, [#ed_error{type = missing_type, location = []}]}
            end;
        {error, _} = Err ->
            Err
    end;
do_to_json(_TypeInfo, T, OtherValue) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => T, value => OtherValue}}]}.

-spec literal_to_json(Value :: term()) ->
                         {ok, json:encode_value()} | {error, [erldantic:error()]}.
%% FIXME: Handle maps, records, list (strings?).
literal_to_json(Term)
    when is_integer(Term) orelse is_float(Term) orelse is_binary(Term) orelse is_atom(Term) ->
    {ok, Term};
literal_to_json(Term) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => {literal, Term}, value => Term}}]}.

-spec prim_type_to_json(Type :: erldantic:a_type_or_ref(), Value :: term()) ->
                           {ok, json:encode_value()} | {error, [erldantic:error()]}.
prim_type_to_json({type, Type} = T, Value) ->
    case check_type_to_json(Type, Value) of
        {true, NewValue} ->
            {ok, NewValue};
        false ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => T, value => Value}}]}
    end.

nonempty_list_to_json(TypeInfo, Type, Data) when is_list(Data) andalso Data =/= [] ->
    list_to_json(TypeInfo, Type, Data);
nonempty_list_to_json(_TypeInfo, Type, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => {nonempty_list, Type}, value => Data}}]}.

-spec list_to_json(TypeInfo :: map(),
                   Type :: erldantic:a_type_or_ref(),
                   Data :: [term()]) ->
                      {ok, [json:encode_value()]} | {error, [erldantic:error()]}.
list_to_json(TypeInfo, Type, Data) when is_list(Data) ->
    JsonRes =
        lists:map(fun({Nr, Item}) ->
                     case do_to_json(TypeInfo, Type, Item) of
                         {ok, Json} ->
                             {ok, Json};
                         skip ->
                             {ok, undefined};
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
            {error, lists:flatmap(fun({error, Errs}) -> Errs end, Errors)}
    end.

data_to_json(TypeInfo, TypeName, TypeArity, Data) ->
    case TypeInfo of
        #{{type, TypeName, TypeArity} := Type} ->
            do_to_json(TypeInfo, Type, Data);
        #{} ->
            {error, [#ed_error{type = missing_type, location = [TypeName]}]}
    end.

map_to_json(TypeInfo, #a_map{fields = Fields}, Data) when is_map(Data) ->
    case map_fields(TypeInfo, Fields, Data) of
        {ok, MapFields} ->
            {ok, maps:from_list(MapFields)};
        {error, Errors} ->
            {error, Errors}
    end;
map_to_json(_TypeInfo, _MapFieldTypes, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => {type, map}, value => Data}}]}.

map_fields(TypeInfo, MapFieldTypes, Data) ->
    {MapFields, Errors, FinalData} =
        lists:foldl(fun ({map_field_assoc, FieldName, FieldType},
                         {FieldsAcc, ErrorsAcc, DataAcc}) ->
                            case maps:take(FieldName, DataAcc) of
                                {FieldData, NewDataAcc} ->
                                    case do_to_json(TypeInfo, FieldType, FieldData) of
                                        {ok, FieldJson} ->
                                            {FieldsAcc ++ [{FieldName, FieldJson}],
                                             ErrorsAcc,
                                             maps:remove(FieldName, DataAcc)};
                                        skip ->
                                            {FieldsAcc, ErrorsAcc, NewDataAcc};
                                        {error, Errs} ->
                                            Errs2 =
                                                lists:map(fun(Err) ->
                                                             err_append_location(Err, FieldName)
                                                          end,
                                                          Errs),
                                            {FieldsAcc, ErrorsAcc ++ Errs2, NewDataAcc}
                                    end;
                                error ->
                                    {FieldsAcc, ErrorsAcc, DataAcc}
                            end;
                        ({map_field_type_assoc, KeyType, ValueType},
                         {FieldsAcc, ErrorsAcc, DataAcc}) ->
                            {NewFields, NewErrors, NewDataAcc} =
                                map_field_type(TypeInfo, KeyType, ValueType, DataAcc),
                            {NewFields ++ FieldsAcc, NewErrors ++ ErrorsAcc, NewDataAcc};
                        ({map_field_type_exact, KeyType, ValueType},
                         {FieldsAcc, ErrorsAcc, DataAcc}) ->
                            {NewFields, NewErrors, NewDataAcc} =
                                map_field_type(TypeInfo, KeyType, ValueType, DataAcc),
                            case NewFields of
                                [] ->
                                    NoExactMatch =
                                        #ed_error{type = not_matched_fields,
                                                  location = [],
                                                  ctx =
                                                      #{type =>
                                                            {map_field_type_exact,
                                                             KeyType,
                                                             ValueType}}},
                                    {NewFields ++ FieldsAcc,
                                     [NoExactMatch] ++ ErrorsAcc ++ ErrorsAcc,
                                     NewDataAcc};
                                _ ->
                                    {NewFields ++ FieldsAcc, NewErrors ++ ErrorsAcc, NewDataAcc}
                            end;
                        ({map_field_exact, FieldName, FieldType},
                         {FieldsAcc, ErrorsAcc, DataAcc}) ->
                            case maps:take(FieldName, DataAcc) of
                                {FieldData, NewDataAcc} ->
                                    case do_to_json(TypeInfo, FieldType, FieldData) of
                                        {ok, FieldJson} ->
                                            {FieldsAcc ++ [{FieldName, FieldJson}],
                                             ErrorsAcc,
                                             NewDataAcc};
                                        skip ->
                                            %% FIXME: Warn about weird type def??
                                            {FieldsAcc, ErrorsAcc, NewDataAcc};
                                        {error, Errs} ->
                                            Errs2 =
                                                lists:map(fun(Err) ->
                                                             err_append_location(Err, FieldName)
                                                          end,
                                                          Errs),
                                            {FieldsAcc, ErrorsAcc ++ Errs2, NewDataAcc}
                                    end;
                                error ->
                                    case can_be_undefined(TypeInfo, FieldType) of
                                        true ->
                                            {FieldsAcc, ErrorsAcc, DataAcc};
                                        false ->
                                            %% ADD test case
                                            {FieldsAcc,
                                             ErrorsAcc
                                             ++ [#ed_error{type = missing_data,
                                                           location = [FieldName]}],
                                             DataAcc}
                                    end
                            end
                    end,
                    {[], [], Data},
                    MapFieldTypes),
    UnMappedErrors =
        case maps:to_list(FinalData) of
            [] ->
                [];
            _ ->
                lists:map(fun({Key, Value}) ->
                             #ed_error{type = not_matched_fields,
                                       location = [],
                                       ctx = #{key => Key, value => Value}}
                          end,
                          maps:to_list(FinalData))
        end,
    case {Errors, UnMappedErrors} of
        {[], []} ->
            {ok, MapFields};
        _ ->
            {error, Errors ++ UnMappedErrors}
    end.

map_field_type(TypeInfo, KeyType, ValueType, Data) ->
    lists:foldl(fun({Key, Value}, {FieldsAcc, ErrorsAcc, DataAcc}) ->
                   case do_to_json(TypeInfo, KeyType, Key) of
                       {ok, KeyJson} ->
                           case do_to_json(TypeInfo, ValueType, Value) of
                               {ok, ValueJson} ->
                                   {FieldsAcc ++ [{KeyJson, ValueJson}],
                                    ErrorsAcc,
                                    maps:remove(Key, DataAcc)};
                               skip ->
                                   {FieldsAcc, ErrorsAcc, DataAcc};
                               {error, Errs} ->
                                   Errs2 =
                                       lists:map(fun(Err) -> err_append_location(Err, Key) end,
                                                 Errs),
                                   {FieldsAcc, ErrorsAcc ++ Errs2, DataAcc}
                           end;
                       {error, _Errs} ->
                           {FieldsAcc, ErrorsAcc, DataAcc}
                   end
                end,
                {[], [], Data},
                maps:to_list(Data)).

-spec record_to_json(TypeInfo :: map(),
                     RecordName :: atom(),
                     Record :: term(),
                     TypeArgs :: [{atom(), erldantic:a_type()}]) ->
                        {ok, #{atom() => json:encode_value()}} | {error, [erldantic:error()]}.
record_to_json(TypeInfo, RecordName, Record, TypeArgs) when is_tuple(Record) ->
    [RecordName | FieldsData] = tuple_to_list(Record),
    #a_rec{name = RecordName, fields = RecordInfo} = maps:get({record, RecordName}, TypeInfo),
    RecordInfoWithVars = apply_record_arg_types(RecordInfo, TypeArgs),
    Mojs = lists:zip(RecordInfoWithVars, FieldsData),
    do_record_to_json(TypeInfo, Mojs);
record_to_json(_TypeInfo, RecordName, Record, TypeArgs) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx =
                    #{record_name => RecordName,
                      record => Record,
                      type_args => TypeArgs}}]}.

apply_record_arg_types(RecordInfo, TypeArgs) ->
    lists:foldl(fun({Field, Type}, Fields) ->
                   lists:keyreplace(Field, 1, Fields, {Field, Type})
                end,
                RecordInfo,
                TypeArgs).

do_record_to_json(TypeInfo, Mojs) ->
    {Fields, Errors} =
        lists:foldl(fun({{FieldName, FieldType}, RecordFieldData}, {FieldsAcc, ErrorsAcc})
                           when is_atom(FieldName) ->
                       case do_to_json(TypeInfo, FieldType, RecordFieldData) of
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
    end.

err_append_location(Err, FieldName) ->
    Err#ed_error{location = [FieldName | Err#ed_error.location]}.

-spec from_json(TypeInfo :: map(),
                Type :: erldantic:a_type_or_ref(),
                Json :: json:decode_value()) ->
                   {ok, term()} | {error, [erldantic:error()]}.
%% why {record, atom()}?
from_json(TypeInfo, {record, RecordName}, Json) when is_atom(RecordName) ->
    record_from_json(TypeInfo, RecordName, Json, []);
from_json(TypeInfo, #a_rec{name = RecordName, fields = RecordInfo}, Json) ->
    do_record_from_json(TypeInfo, RecordName, RecordInfo, Json);
from_json(_TypeInfo, #remote_type{mfargs = {Module, TypeName, TypeArgs}}, Json) ->
    case erldantic_module_types:get(Module) of
        {ok, TypeInfo} ->
            TypeArity = length(TypeArgs),
            case TypeInfo of
                #{{type, TypeName, TypeArity} := Type} ->
                    TypeWithoutVars = apply_args(TypeInfo, Type, TypeArgs),
                    from_json(TypeInfo, TypeWithoutVars, Json);
                #{} ->
                    {error, [#ed_error{type = missing_type, location = []}]}
            end;
        {error, _} = Err ->
            Err
    end;
from_json(TypeInfo, {record_ref, RecordName, TypeArgs}, Json) when is_atom(RecordName) ->
    record_from_json(TypeInfo, RecordName, Json, TypeArgs);
from_json(TypeInfo, #a_map{fields = Fields}, Json) ->
    map_from_json(TypeInfo, Fields, Json);
from_json(TypeInfo, #a_type{type = Type, vars = []}, Json) ->
    from_json(TypeInfo, Type, Json);
from_json(TypeInfo, {user_type_ref, TypeName, TypeArgs}, Json) when is_atom(TypeName) ->
    type_from_json(TypeInfo, TypeName, length(TypeArgs), TypeArgs, Json);
from_json(TypeInfo, {nonempty_list, Type}, Data) ->
    nonempty_list_from_json(TypeInfo, Type, Data);
from_json(TypeInfo, {list, Type}, Data) ->
    list_from_json(TypeInfo, Type, Data);
from_json(_TypeInfo, {type, PrimaryType} = T, Json)
    when ?is_primary_type(PrimaryType) orelse ?is_predefined_int_range(PrimaryType) ->
    case check_type_from_json(PrimaryType, Json) of
        {true, NewValue} ->
            {ok, NewValue};
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
    %% FIXME: For simple types without arity, default to 0
    type_from_json(TypeInfo, TypeName, 0, [], Json);
from_json(TypeInfo, {type, TypeName, TypeArity}, Json) when is_atom(TypeName) ->
    type_from_json(TypeInfo, TypeName, TypeArity, [], Json);
from_json(TypeInfo, {union, _} = T, Json) ->
    union(fun from_json/3, TypeInfo, T, Json);
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

nonempty_list_from_json(TypeInfo, Type, Data) when is_list(Data) andalso Data =/= [] ->
    list_from_json(TypeInfo, Type, Data);
nonempty_list_from_json(_TypeInfo, Type, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => {nonempty_list, Type}, value => Data}}]}.

list_from_json(TypeInfo, Type, Data) when is_list(Data) ->
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
    end;
list_from_json(_TypeInfo, Type, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => {list, Type}, value => Data}}]}.

check_type_from_json(string, Json) when is_binary(Json) ->
    {true, binary_to_list(Json)};
check_type_from_json(nonempty_string, Json) when is_binary(Json), byte_size(Json) > 0 ->
    {true, binary_to_list(Json)};
check_type_from_json(atom, Json) when is_binary(Json) ->
    try
        {true, binary_to_existing_atom(Json, utf8)}
    catch
        error:badarg ->
            false
    end;
check_type_from_json(Type, Json) ->
    check_type(Type, Json).

check_type_to_json(string, Json) when is_list(Json) ->
    {true, list_to_binary(Json)};
check_type_to_json(nonempty_string, Json) when is_list(Json), Json =/= [] ->
    case io_lib:printable_list(Json) of
        true ->
            {true, list_to_binary(Json)};
        false ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx =
                            #{type => {type, string},
                              value => Json,
                              comment => "non printable"}}]}
    end;
check_type_to_json(Type, Json) ->
    check_type(Type, Json).

check_type(integer, Json) when is_integer(Json) ->
    {true, Json};
check_type(string, Json) when is_list(Json) ->
    %% All characters should be printable ASCII or it's probably not intended as a string
    %% FIXME: Document this.
    case io_lib:printable_list(Json) of
        true ->
            {true, Json};
        false ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx =
                            #{type => {type, string},
                              value => Json,
                              comment => "non printable"}}]}
    end;
check_type(boolean, Json) when is_boolean(Json) ->
    {true, Json};
check_type(float, Json) when is_float(Json) ->
    {true, Json};
check_type(number, Json) when is_integer(Json) orelse is_float(Json) ->
    {true, Json};
check_type(non_neg_integer, Json) when is_integer(Json) andalso Json >= 0 ->
    {true, Json};
check_type(pos_integer, Json) when is_integer(Json) andalso Json > 0 ->
    {true, Json};
check_type(neg_integer, Json) when is_integer(Json) andalso Json < 0 ->
    {true, Json};
check_type(binary, Json) when is_binary(Json) ->
    {true, Json};
check_type(atom, Json) when is_atom(Json) ->
    {true, Json};
check_type(term, Json) ->
    {true, Json};
check_type(_Type, _Json) ->
    false.

union(F, TypeInfo, {union, Types} = T, Json) ->
    case do_first(F, TypeInfo, Types, Json) of
        {error, no_match} ->
            {error,
             [#ed_error{type = no_match,
                        location = [],
                        ctx = #{type => T, value => Json}}]};
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

-spec type_from_json(TypeInfo :: erldantic:type_info(),
                     TypeName :: atom(),
                     TypeArity :: non_neg_integer(),
                     TypeArgs :: [erldantic:a_type()],
                     Json :: json:decode_value()) ->
                        {ok, term()} | {error, [erldantic:error()]}.
type_from_json(TypeInfo, TypeName, TypeArity, TypeArgs, Json) ->
    case TypeInfo of
        #{{type, TypeName, TypeArity} := Type} ->
            TypeWithoutVars = apply_args(TypeInfo, Type, TypeArgs),
            from_json(TypeInfo, TypeWithoutVars, Json);
        #{} ->
            {error, [#ed_error{type = missing_type, location = [TypeName]}]}
    end.

apply_args(TypeInfo, Type, TypeArgs) when is_list(TypeArgs) ->
    ArgNames = arg_names(Type),
    NamedTypes =
        maps:from_list(
            lists:zip(ArgNames, TypeArgs)),
    type_replace_vars(TypeInfo, Type, NamedTypes).

arg_names(#a_type{vars = Args}) ->
    Args;
arg_names(_) ->
    [].

-spec type_replace_vars(TypeInfo :: erldantic:type_info(),
                        Type :: erldantic:a_type(),
                        NamedTypes :: #{atom() => erldantic:a_type()}) ->
                           erldantic:a_type().
type_replace_vars(_TypeInfo, {var, Name}, NamedTypes) ->
    maps:get(Name, NamedTypes, {type, term});
type_replace_vars(TypeInfo, #a_type{type = Type, vars = _Vars}, NamedTypes) ->
    case Type of
        %% FIXME: lists and ranges?
        {union, Types} ->
            {union, lists:map(fun(T) -> type_replace_vars(TypeInfo, T, NamedTypes) end, Types)};
        #a_map{fields = Fields} ->
            #a_map{fields =
                       lists:map(fun ({map_field_assoc, FieldName, FieldType}) ->
                                         {map_field_assoc,
                                          FieldName,
                                          type_replace_vars(TypeInfo, FieldType, NamedTypes)};
                                     ({map_field_exact, FieldName, FieldType}) ->
                                         {map_field_exact,
                                          FieldName,
                                          type_replace_vars(TypeInfo, FieldType, NamedTypes)};
                                     ({map_field_type_assoc, KeyType, ValueType}) ->
                                         %% ADD TESTS
                                         {map_field_type_assoc,
                                          type_replace_vars(TypeInfo, KeyType, NamedTypes),
                                          type_replace_vars(TypeInfo, ValueType, NamedTypes)};
                                     ({map_field_type_exact, KeyType, ValueType}) ->
                                         %% ADD TESTS
                                         {map_field_type_exact,
                                          type_replace_vars(TypeInfo, KeyType, NamedTypes),
                                          type_replace_vars(TypeInfo, ValueType, NamedTypes)}
                                 end,
                                 Fields)};
        {record_ref, RecordName, TypeArgs} ->
            case TypeInfo of
                #{{record, RecordName} := #a_rec{fields = Fields} = Rec} ->
                    NewFields =
                        lists:foldl(fun({Name, NType}, FieldsAcc) ->
                                       lists:keyreplace(Name, 1, FieldsAcc, {Name, NType})
                                    end,
                                    Fields,
                                    TypeArgs),
                    NewRec = Rec#a_rec{fields = NewFields},
                    Mojs = type_replace_vars(TypeInfo, NewRec, NamedTypes),
                    Mojs;
                #{} ->
                    erlang:error({missing_type, {record, RecordName}})
            end;
        _ ->
            Type
    end;
type_replace_vars(_TypeInfo, #a_rec{fields = Fields} = Rec, NamedTypes) ->
    Rec#a_rec{fields =
                  lists:map(fun({Name, NType}) ->
                               {Name, type_replace_vars(_TypeInfo, NType, NamedTypes)}
                            end,
                            Fields)};
type_replace_vars(_TypeInfo, Type, _NamedTypes) ->
    Type.

map_from_json(TypeInfo, MapFieldType, Json) when is_map(Json) ->
    {Fields, Errors, NotMapped} =
        lists:foldl(fun ({map_field_assoc, FieldName, FieldType}, {FieldsAcc, ErrAcc, JsonAcc}) ->
                            case maps:take(atom_to_binary(FieldName), JsonAcc) of
                                {FieldData, NewJsonAcc} ->
                                    case from_json(TypeInfo, FieldType, FieldData) of
                                        {ok, FieldJson} ->
                                            {FieldsAcc ++ [{FieldName, FieldJson}],
                                             ErrAcc,
                                             NewJsonAcc};
                                        {error, Errs} ->
                                            Errs2 =
                                                lists:map(fun(Err) ->
                                                             err_append_location(Err, FieldName)
                                                          end,
                                                          Errs),
                                            {FieldsAcc, ErrAcc ++ Errs2, NewJsonAcc}
                                    end;
                                error ->
                                    {FieldsAcc, ErrAcc, JsonAcc}
                            end;
                        ({map_field_exact, FieldName, FieldType}, {FieldsAcc, ErrAcc, JsonAcc}) ->
                            case maps:take(atom_to_binary(FieldName), JsonAcc) of
                                {FieldData, NewJsonAcc} ->
                                    case from_json(TypeInfo, FieldType, FieldData) of
                                        {ok, FieldJson} ->
                                            {FieldsAcc ++ [{FieldName, FieldJson}],
                                             ErrAcc,
                                             NewJsonAcc};
                                        {error, Errs} ->
                                            Errs2 =
                                                lists:map(fun(Err) ->
                                                             err_append_location(Err, FieldName)
                                                          end,
                                                          Errs),
                                            {FieldsAcc, ErrAcc ++ Errs2, NewJsonAcc}
                                    end;
                                error ->
                                    case can_be_undefined(TypeInfo, FieldType) of
                                        true ->
                                            {FieldsAcc ++ [{FieldName, undefined}],
                                             ErrAcc,
                                             JsonAcc};
                                        false ->
                                            {FieldsAcc,
                                             ErrAcc
                                             ++ [#ed_error{type = missing_data,
                                                           location = [FieldName]}],
                                             JsonAcc}
                                    end
                            end;
                        ({map_field_type_assoc, KeyType, ValueType},
                         {FieldsAcc, ErrAcc, JsonAcc}) ->
                            {NewFields, NewErrors, NewJsonAcc} =
                                map_field_type_from_json(TypeInfo, KeyType, ValueType, JsonAcc),
                            {FieldsAcc ++ NewFields, ErrAcc ++ NewErrors, NewJsonAcc};
                        ({map_field_type_exact, KeyType, ValueType},
                         {FieldsAcc, ErrAcc, JsonAcc}) ->
                            {NewFields, NewErrors, NewJsonAcc} =
                                map_field_type_from_json(TypeInfo, KeyType, ValueType, JsonAcc),
                            case NewFields of
                                [] ->
                                    NoExactMatch =
                                        #ed_error{type = not_matched_fields,
                                                  location = [],
                                                  ctx =
                                                      #{type =>
                                                            {map_field_type_exact,
                                                             KeyType,
                                                             ValueType}}},
                                    {NewFields ++ FieldsAcc,
                                     [NoExactMatch] ++ ErrAcc ++ NewErrors,
                                     NewJsonAcc};
                                _ ->
                                    {NewFields ++ FieldsAcc, ErrAcc ++ NewErrors, NewJsonAcc}
                            end
                    end,
                    {[], [], Json},
                    MapFieldType),
    case Errors of
        [] ->
            case maps:size(NotMapped) of
                0 ->
                    {ok, maps:from_list(Fields)};
                _ ->
                    {error,
                     lists:map(fun({Key, Value}) ->
                                  #ed_error{type = not_matched_fields,
                                            location = [],
                                            ctx = #{key => Key, value => Value}}
                               end,
                               maps:to_list(NotMapped))}
            end;
        _ ->
            {error, Errors}
    end;
map_from_json(_TypeInfo, _MapFieldType, Json) ->
    %% Return error when Json is not a map
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => {type, map}, value => Json}}]}.

map_field_type_from_json(TypeInfo, KeyType, ValueType, Json) ->
    lists:foldl(fun({Key, Value}, {FieldsAcc, ErrAcc, JsonAcc}) ->
                   case from_json(TypeInfo, KeyType, Key) of
                       {ok, KeyJson} ->
                           case from_json(TypeInfo, ValueType, Value) of
                               {ok, ValueJson} ->
                                   {FieldsAcc ++ [{KeyJson, ValueJson}],
                                    ErrAcc,
                                    maps:remove(Key, JsonAcc)};
                               {error, Errs} ->
                                   Errs2 =
                                       lists:map(fun(Err) -> err_append_location(Err, Key) end,
                                                 Errs),
                                   {FieldsAcc, ErrAcc ++ Errs2, maps:remove(Key, JsonAcc)}
                           end;
                       {error, _Errs} ->
                           {FieldsAcc, ErrAcc, JsonAcc}
                   end
                end,
                {[], [], Json},
                maps:to_list(Json)).

-spec record_from_json(TypeInfo :: map(),
                       RecordName :: atom(),
                       Json :: json:decode_value(),
                       TypeArgs :: [erldantic:record_field()]) ->
                          {ok, term()} | {error, list()}.
record_from_json(TypeInfo, RecordName, Json, TypeArgs) ->
    ARec = maps:get({record, RecordName}, TypeInfo),
    NewARec = apply_args(TypeInfo, ARec, TypeArgs),
    #a_rec{name = RecordName, fields = RecordInfo} = NewARec,
    do_record_from_json(TypeInfo, RecordName, RecordInfo, Json).

-spec do_record_from_json(TypeInfo :: map(),
                          RecordName :: atom(),
                          RecordInfo :: list(),
                          Json :: json:decode_value()) ->
                             {ok, term()} | {error, list()}.
do_record_from_json(TypeInfo, RecordName, RecordInfo, Json) when is_map(Json) ->
    %% FIXME: Apply type args?
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
                               case can_be_undefined(TypeInfo, FieldType) of
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
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{record_name => RecordName, record => Json}}]}.

-spec can_be_undefined(TypeInfo :: erldantic:type_info(), Type :: erldantic:a_type()) ->
                          boolean().
can_be_undefined(TypeInfo, Type) ->
    case Type of
        #a_type{type = Type2} ->
            can_be_undefined(TypeInfo, Type2);
        {union, Types} ->
            lists:member({literal, undefined}, Types);
        {literal, undefined} ->
            true;
        {user_type_ref, TypeName, TypeArgs} ->
            TypeArity = length(TypeArgs),
            case TypeInfo of
                #{{type, TypeName, TypeArity} := Type2} ->
                    %% infinite recursion?
                    can_be_undefined(TypeInfo, Type2);
                #{} ->
                    %% error?
                    false
            end;
        _ ->
            false
    end.
