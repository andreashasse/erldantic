-module(erldantic_json).

-export([to_json/3, from_json/3]).

-ignore_xref([to_json/3, from_json/3]).

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

%% API

-spec to_json(erldantic:type_info() | module(),
              erldantic:ed_type_or_ref(),
              Data :: dynamic()) ->
                 {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json(Module, TypeRef, Data) when is_atom(Module) ->
    TypeInfo = erldantic_module_types:get(Module),
    to_json(TypeInfo, TypeRef, Data);
to_json(TypeInfo, Type, Data) ->
    case do_to_json(TypeInfo, Type, Data) of
        {ok, Json} ->
            {ok, Json};
        skip ->
            {ok, undefined};
        {error, Errs} ->
            {error, Errs}
    end.

%% INTERNAL
-spec do_to_json(TypeInfo :: erldantic:type_info(),
                 Type :: erldantic:ed_type_or_ref(),
                 Data :: dynamic()) ->
                    {ok, json:encode_value()} | {error, [erldantic:error()]} | skip.
do_to_json(TypeInfo, {record, RecordName}, Record) when is_atom(RecordName) ->
    record_to_json(TypeInfo, RecordName, Record, []);
do_to_json(TypeInfo, #ed_rec{} = RecordInfo, Record) when is_tuple(Record) ->
    record_to_json(TypeInfo, RecordInfo, Record, []);
do_to_json(TypeInfo,
           #ed_rec_ref{record_name = RecordName, field_types = TypeArgs},
           Record)
    when is_atom(RecordName) ->
    record_to_json(TypeInfo, RecordName, Record, TypeArgs);
do_to_json(TypeInfo, #ed_user_type_ref{type_name = TypeName, variables = TypeArgs}, Data)
    when is_atom(TypeName) ->
    TypeArity = length(TypeArgs),
    {ok, Type} = erldantic_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, TypeArgs),
    do_to_json(TypeInfo, TypeWithoutVars, Data);
do_to_json(_TypeInfo, #ed_simple_type{type = NotSupported} = Type, _Data)
    when NotSupported =:= pid
         orelse NotSupported =:= port
         orelse NotSupported =:= reference
         orelse NotSupported =:= bitstring
         orelse NotSupported =:= nonempty_bitstring
         orelse NotSupported =:= none ->
    erlang:error({type_not_supported, Type});
do_to_json(_TypeInfo, #ed_simple_type{} = Type, Value) ->
    prim_type_to_json(Type, Value);
do_to_json(_TypeInfo,
           #ed_range{type = integer,
                     lower_bound = Min,
                     upper_bound = Max},
           Value)
    when is_integer(Value) andalso Min =< Value, Value =< Max ->
    {ok, Value};
do_to_json(_TypeInfo, #ed_literal{value = undefined}, undefined) ->
    skip;
do_to_json(_TypeInfo, #ed_literal{value = Value}, Value) ->
    {ok, Value};
do_to_json(TypeInfo, #ed_union{} = Type, Data) ->
    union(fun do_to_json/3, TypeInfo, Type, Data);
do_to_json(TypeInfo, #ed_nonempty_list{type = Type}, Data) ->
    nonempty_list_to_json(TypeInfo, Type, Data);
do_to_json(TypeInfo, #ed_list{type = Type}, Data) when is_list(Data) ->
    list_to_json(TypeInfo, Type, Data);
do_to_json(TypeInfo, {type, TypeName, TypeArity}, Data) when is_atom(TypeName) ->
    %% FIXME: For simple types without arity, default to 0
    {ok, Type} = erldantic_type_info:get_type(TypeInfo, TypeName, TypeArity),
    do_to_json(TypeInfo, Type, Data);
do_to_json(TypeInfo, #ed_map{struct_name = StructName} = Map, Data) ->
    case StructName of
        undefined ->
            map_to_json(TypeInfo, Map, Data);
        _ ->
            %% For Elixir structs, we expect the data to have the __struct__ field
            case maps:get('__struct__', Data, undefined) of
                StructName ->
                    map_to_json(TypeInfo, Map, Data);
                _ ->
                    {error,
                     [#ed_error{type = type_mismatch,
                                location = [],
                                ctx = #{expected_struct => StructName, value => Data}}]}
            end
    end;
do_to_json(_TypeInfo, #ed_remote_type{mfargs = {Module, TypeName, Args}}, Data) ->
    TypeInfo = erldantic_module_types:get(Module),
    TypeArity = length(Args),
    {ok, Type} = erldantic_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
    do_to_json(TypeInfo, TypeWithoutVars, Data);
do_to_json(_TypeInfo, #ed_maybe_improper_list{} = Type, _Data) ->
    erlang:error({type_not_implemented, Type});
do_to_json(_TypeInfo, #ed_nonempty_improper_list{} = Type, _Data) ->
    erlang:error({type_not_implemented, Type});
%% Not supported types
do_to_json(_TypeInfo, #ed_tuple{} = Type, _Data) ->
    erlang:error({type_not_supported, Type});
do_to_json(_TypeInfo, #ed_function{} = Type, _Data) ->
    erlang:error({type_not_supported, Type});
do_to_json(_TypeInfo, Type, OtherValue) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => Type, value => OtherValue}}]}.

-spec prim_type_to_json(Type :: erldantic:ed_type(), Value :: term()) ->
                           {ok, json:encode_value()} | {error, [erldantic:error()]}.
prim_type_to_json(#ed_simple_type{type = Type} = T, Value) ->
    case check_type_to_json(Type, Value) of
        {true, NewValue} ->
            {ok, NewValue};
        {error, Reason} ->
            {error, Reason};
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

-spec list_to_json(TypeInfo :: erldantic:type_info(),
                   Type :: erldantic:ed_type_or_ref(),
                   Data :: [term()]) ->
                      {ok, [json:encode_value()]} | {error, [erldantic:error()]}.
list_to_json(TypeInfo, Type, Data) when is_list(Data) ->
    erldantic_util:map_until_error(fun({Nr, Item}) ->
                                      case do_to_json(TypeInfo, Type, Item) of
                                          {ok, Json} ->
                                              {ok, Json};
                                          skip ->
                                              {ok, undefined};
                                          {error, Errs} ->
                                              Errs2 =
                                                  lists:map(fun(Err) -> err_append_location(Err, Nr)
                                                            end,
                                                            Errs),
                                              {error, Errs2}
                                      end
                                   end,
                                   lists:enumerate(Data)).

map_to_json(TypeInfo, #ed_map{fields = Fields}, Data) when is_map(Data) ->
    %% Check if this is an Elixir struct and remove __struct__ field for JSON serialization
    DataWithoutStruct =
        case maps:take('__struct__', Data) of
            {_StructName, CleanData} ->
                CleanData;
            error ->
                Data
        end,
    case map_fields_to_json(TypeInfo, Fields, DataWithoutStruct) of
        {ok, MapFields} ->
            {ok, maps:from_list(MapFields)};
        {error, Errors} ->
            {error, Errors}
    end;
map_to_json(_TypeInfo, _MapFieldTypes, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = map}, value => Data}}]}.

map_fields_to_json(TypeInfo, MapFieldTypes, Data) ->
    Fun = fun ({map_field_assoc, FieldName, FieldType}, {FieldsAcc, DataAcc}) ->
                  case maps:take(FieldName, DataAcc) of
                      {FieldData, NewDataAcc} ->
                          case do_to_json(TypeInfo, FieldType, FieldData) of
                              {ok, FieldJson} ->
                                  {ok,
                                   {[{FieldName, FieldJson}] ++ FieldsAcc,
                                    maps:remove(FieldName, DataAcc)}};
                              skip ->
                                  {ok, {FieldsAcc, NewDataAcc}};
                              {error, Errs} ->
                                  Errs2 =
                                      lists:map(fun(Err) -> err_append_location(Err, FieldName) end,
                                                Errs),
                                  {error, Errs2}
                          end;
                      error ->
                          {ok, {FieldsAcc, DataAcc}}
                  end;
              ({map_field_type_assoc, KeyType, ValueType}, {FieldsAcc, DataAcc}) ->
                  case map_field_type(TypeInfo, KeyType, ValueType, DataAcc) of
                      {ok, {NewFields, NewDataAcc}} ->
                          {ok, {NewFields ++ FieldsAcc, NewDataAcc}};
                      {error, _} = Err ->
                          Err
                  end;
              ({map_field_type_exact, KeyType, ValueType}, {FieldsAcc, DataAcc}) ->
                  case map_field_type(TypeInfo, KeyType, ValueType, DataAcc) of
                      {ok, {[], _}} ->
                          NoExactMatch =
                              #ed_error{type = not_matched_fields,
                                        location = [],
                                        ctx =
                                            #{type => {map_field_type_exact, KeyType, ValueType}}},
                          {error, [NoExactMatch]};
                      {ok, {NewFields, NewDataAcc}} ->
                          {ok, {NewFields ++ FieldsAcc, NewDataAcc}};
                      {error, _} = Err ->
                          Err
                  end;
              ({map_field_exact, FieldName, FieldType}, {FieldsAcc, DataAcc}) ->
                  case maps:take(FieldName, DataAcc) of
                      {FieldData, NewDataAcc} ->
                          case do_to_json(TypeInfo, FieldType, FieldData) of
                              {ok, FieldJson} ->
                                  {ok, {[{FieldName, FieldJson}] ++ FieldsAcc, NewDataAcc}};
                              skip ->
                                  %% FIXME: Warn about weird type def??
                                  {ok, {FieldsAcc, NewDataAcc}};
                              {error, Errs} ->
                                  Errs2 =
                                      lists:map(fun(Err) -> err_append_location(Err, FieldName) end,
                                                Errs),
                                  {error, Errs2}
                          end;
                      error ->
                          case erldantic_type:can_be_undefined(TypeInfo, FieldType) of
                              true ->
                                  {ok, {FieldsAcc, DataAcc}};
                              false ->
                                  {error, [#ed_error{type = missing_data, location = [FieldName]}]}
                          end
                  end
          end,
    case erldantic_util:fold_until_error(Fun, {[], Data}, MapFieldTypes) of
        {ok, {MapFields, FinalData}} ->
            case maps:to_list(FinalData) of
                [] ->
                    {ok, MapFields};
                L ->
                    {error,
                     lists:map(fun({Key, Value}) ->
                                  #ed_error{type = not_matched_fields,
                                            location = [],
                                            ctx = #{key => Key, value => Value}}
                               end,
                               L)}
            end;
        {error, _} = Err ->
            Err
    end.

-spec map_field_type(TypeInfo :: erldantic:type_info(),
                     KeyType :: erldantic:ed_type(),
                     ValueType :: erldantic:ed_type(),
                     Data :: map()) ->
                        {ok, {[{json:encode_value(), json:encode_value()}], map()}} |
                        {error, [erldantic:error()]}.
map_field_type(TypeInfo, KeyType, ValueType, Data) ->
    Fun = fun({Key, Value}, {FieldsAcc, DataAcc}) ->
             case do_to_json(TypeInfo, KeyType, Key) of
                 {ok, KeyJson} ->
                     case do_to_json(TypeInfo, ValueType, Value) of
                         {ok, ValueJson} ->
                             {ok, {FieldsAcc ++ [{KeyJson, ValueJson}], maps:remove(Key, DataAcc)}};
                         skip ->
                             {ok, {FieldsAcc, DataAcc}};
                         {error, Errs} ->
                             Errs2 = lists:map(fun(Err) -> err_append_location(Err, Key) end, Errs),
                             {error, Errs2}
                     end;
                 {error, _Errs} ->
                     {ok, {FieldsAcc, DataAcc}}
             end
          end,
    erldantic_util:fold_until_error(Fun, {[], Data}, maps:to_list(Data)).

-spec record_to_json(TypeInfo :: erldantic:type_info(),
                     RecordName :: atom() | #ed_rec{},
                     Record :: term(),
                     TypeArgs :: [{atom(), erldantic:ed_type()}]) ->
                        {ok, #{atom() => json:encode_value()}} | {error, [erldantic:error()]}.
record_to_json(TypeInfo, RecordName, Record, TypeArgs) when is_atom(RecordName) ->
    {ok, RecordInfo} = erldantic_type_info:get_record(TypeInfo, RecordName),
    record_to_json(TypeInfo, RecordInfo, Record, TypeArgs);
record_to_json(TypeInfo,
               #ed_rec{name = RecordName,
                       fields = Fields,
                       arity = Arity},
               Record,
               TypeArgs)
    when is_tuple(Record)
         andalso element(1, Record) =:= RecordName
         andalso tuple_size(Record) =:= Arity ->
    [RecordName | FieldsData] = tuple_to_list(Record),
    RecFieldTypes = record_replace_vars(Fields, TypeArgs),
    RecFieldTypesWithData = lists:zip(RecFieldTypes, FieldsData),
    do_record_to_json(TypeInfo, RecFieldTypesWithData);
record_to_json(_TypeInfo, RecordName, Record, TypeArgs) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx =
                    #{record_name => RecordName,
                      record => Record,
                      type_args => TypeArgs}}]}.

record_replace_vars(RecordInfo, TypeArgs) ->
    lists:foldl(fun({Field, Type}, Fields) ->
                   lists:keyreplace(Field, 1, Fields, {Field, Type})
                end,
                RecordInfo,
                TypeArgs).

-spec do_record_to_json(erldantic:type_info(),
                        [{{atom(), erldantic:ed_type()}, term()}]) ->
                           {ok, #{atom() => json}} | {error, [erldantic:error()]}.
do_record_to_json(TypeInfo, RecFieldTypesWithData) ->
    Fun = fun({{FieldName, FieldType}, RecordFieldData}, FieldsAcc) when is_atom(FieldName) ->
             case do_to_json(TypeInfo, FieldType, RecordFieldData) of
                 {ok, FieldJson} ->
                     {ok, [{FieldName, FieldJson}] ++ FieldsAcc};
                 skip ->
                     {ok, FieldsAcc};
                 {error, Errors} ->
                     {error,
                      lists:map(fun(Error) -> err_append_location(Error, FieldName) end, Errors)}
             end
          end,

    case erldantic_util:fold_until_error(Fun, [], RecFieldTypesWithData) of
        {ok, Fields} ->
            {ok, maps:from_list(Fields)};
        {error, _} = Err ->
            Err
    end.

err_append_location(Err, FieldName) ->
    Err#ed_error{location = [FieldName | Err#ed_error.location]}.

-spec from_json(TypeInfo :: erldantic:type_info() | module(),
                Type :: erldantic:ed_type_or_ref(),
                Json :: json:decode_value()) ->
                   {ok, term()} | {error, [erldantic:error()]}.
from_json(Module, Type, Json) when is_atom(Module) ->
    TypeInfo = erldantic_module_types:get(Module),
    do_from_json(TypeInfo, Type, Json);
from_json(TypeInfo, Type, Json) ->
    do_from_json(TypeInfo, Type, Json).

-spec do_from_json(TypeInfo :: erldantic:type_info(),
                   Type :: erldantic:ed_type_or_ref(),
                   Json :: json:decode_value()) ->
                      {ok, term()} | {error, [erldantic:error()]}.
do_from_json(TypeInfo, {record, RecordName}, Json) when is_atom(RecordName) ->
    record_from_json(TypeInfo, RecordName, Json, []);
do_from_json(TypeInfo, #ed_rec{} = Rec, Json) ->
    record_from_json(TypeInfo, Rec, Json, []);
do_from_json(_TypeInfo, #ed_remote_type{mfargs = {Module, TypeName, Args}}, Data) ->
    TypeInfo = erldantic_module_types:get(Module),
    TypeArity = length(Args),
    {ok, Type} = erldantic_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
    do_from_json(TypeInfo, TypeWithoutVars, Data);
do_from_json(TypeInfo,
             #ed_rec_ref{record_name = RecordName, field_types = TypeArgs},
             Json)
    when is_atom(RecordName) ->
    record_from_json(TypeInfo, RecordName, Json, TypeArgs);
do_from_json(TypeInfo, #ed_map{fields = Fields, struct_name = StructName}, Json) ->
    case map_from_json(TypeInfo, Fields, Json) of
        {ok, MapResult} when StructName =/= undefined ->
            %% Add back the __struct__ field for Elixir structs
            {ok, maps:put('__struct__', StructName, MapResult)};
        Result ->
            Result
    end;
do_from_json(TypeInfo,
             #ed_user_type_ref{type_name = TypeName, variables = TypeArgs},
             Json)
    when is_atom(TypeName) ->
    type_from_json(TypeInfo, TypeName, length(TypeArgs), TypeArgs, Json);
do_from_json(TypeInfo, #ed_nonempty_list{type = Type}, Data) ->
    nonempty_list_from_json(TypeInfo, Type, Data);
do_from_json(TypeInfo, #ed_list{type = Type}, Data) ->
    list_from_json(TypeInfo, Type, Data);
do_from_json(_TypeInfo, #ed_simple_type{type = NotSupported} = T, _Value)
    when NotSupported =:= pid
         orelse NotSupported =:= port
         orelse NotSupported =:= reference
         orelse NotSupported =:= bitstring
         orelse NotSupported =:= nonempty_bitstring
         orelse NotSupported =:= none ->
    erlang:error({type_not_supported, T});
do_from_json(_TypeInfo, #ed_simple_type{type = PrimaryType} = T, Json) ->
    case check_type_from_json(PrimaryType, Json) of
        {true, NewValue} ->
            {ok, NewValue};
        {error, Reason} ->
            {error, Reason};
        false ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => T, value => Json}}]}
    end;
do_from_json(_TypeInfo, #ed_literal{value = Literal}, Literal) ->
    {ok, Literal};
do_from_json(_TypeInfo, #ed_literal{value = Literal} = Type, Value) ->
    case try_convert_to_literal(Literal, Value) of
        {ok, Literal} ->
            {ok, Literal};
        false ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => Type, value => Value}}]}
    end;
do_from_json(TypeInfo, {type, TypeName, TypeArity}, Json) when is_atom(TypeName) ->
    type_from_json(TypeInfo, TypeName, TypeArity, [], Json);
do_from_json(TypeInfo, #ed_union{} = Type, Json) ->
    union(fun do_from_json/3, TypeInfo, Type, Json);
do_from_json(_TypeInfo,
             #ed_range{type = integer,
                       lower_bound = Min,
                       upper_bound = Max},
             Value)
    when Min =< Value, Value =< Max, is_integer(Value) ->
    {ok, Value};
do_from_json(_TypeInfo,
             #ed_range{type = integer,
                       lower_bound = _Min,
                       upper_bound = _Max} =
                 Range,
             Value)
    when is_integer(Value) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => Range, value => Value}}]};
do_from_json(_TypeInfo, #ed_maybe_improper_list{} = Type, _Value) ->
    erlang:error({type_not_implemented, Type});
do_from_json(_TypeInfo, #ed_nonempty_improper_list{} = Type, _Value) ->
    erlang:error({type_not_implemented, Type});
do_from_json(_TypeInfo, #ed_function{} = Type, _Value) ->
    erlang:error({type_not_supported, Type});
do_from_json(_TypeInfo, #ed_tuple{} = Type, _Value) ->
    erlang:error({type_not_supported, Type});
do_from_json(_TypeInfo, Type, Value) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => Type, value => Value}}]}.

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
    Fun = fun({Nr, Item}) ->
             case do_from_json(TypeInfo, Type, Item) of
                 {ok, Json} ->
                     {ok, Json};
                 {error, Errs} ->
                     Errs2 = lists:map(fun(Err) -> err_append_location(Err, Nr) end, Errs),

                     {error, Errs2}
             end
          end,
    erldantic_util:map_until_error(Fun, lists:enumerate(Data));
list_from_json(_TypeInfo, Type, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => {list, Type}, value => Data}}]}.

string_from_json(Type, Json) ->
    StringValue = unicode:characters_to_list(Json),
    case io_lib:printable_list(StringValue) of
        true ->
            {true, StringValue};
        false ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx =
                            #{type => #ed_simple_type{type = Type},
                              value => Json,
                              comment => "non printable"}}]}
    end.

check_type_from_json(string, Json) when is_binary(Json) ->
    string_from_json(string, Json);
check_type_from_json(nonempty_string, Json) when is_binary(Json), byte_size(Json) > 0 ->
    string_from_json(nonempty_string, Json);
check_type_from_json(iodata, Json) when is_binary(Json) ->
    {true, Json};
check_type_from_json(iolist, Json) when is_binary(Json) ->
    {true, [Json]};
check_type_from_json(atom, Json) when is_binary(Json) ->
    try
        {true, binary_to_existing_atom(Json, utf8)}
    catch
        error:badarg ->
            false
    end;
check_type_from_json(Type, Json) ->
    check_type(Type, Json).

check_type_to_json(iodata, Json) when is_binary(Json) ->
    {true, Json};
check_type_to_json(iodata, Json) when is_list(Json) ->
    {true, iolist_to_binary(Json)};
check_type_to_json(iolist, Json) when is_list(Json) ->
    {true, iolist_to_binary(Json)};
check_type_to_json(nonempty_string, Json) when is_list(Json), Json =/= [] ->
    case unicode:characters_to_binary(Json) of
        {Err, _, _} when Err =:= error orelse Err =:= incomplete ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx =
                            #{type => #ed_simple_type{type = string},
                              value => Json,
                              comment => "non printable"}}]};
        Bin when is_binary(Bin) ->
            {true, Bin}
    end;
check_type_to_json(string, Json) when is_list(Json) ->
    case unicode:characters_to_binary(Json) of
        {Err, _, _} when Err =:= error orelse Err =:= incomplete ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx =
                            #{type => #ed_simple_type{type = string},
                              value => Json,
                              comment => "non printable"}}]};
        Bin when is_binary(Bin) ->
            {true, Bin}
    end;
check_type_to_json(Type, Json) ->
    check_type(Type, Json).

check_type(integer, Json) when is_integer(Json) ->
    {true, Json};
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
check_type(nonempty_binary, Json) when is_binary(Json), byte_size(Json) > 0 ->
    {true, Json};
check_type(atom, Json) when is_atom(Json) ->
    {true, Json};
check_type(term, Json) ->
    {true, Json};
check_type(_Type, _Json) ->
    false.

union(Fun, TypeInfo, #ed_union{types = Types} = T, Json) ->
    case do_first(Fun, TypeInfo, Types, Json) of
        {error, no_match} ->
            {error,
             [#ed_error{type = no_match,
                        location = [],
                        ctx = #{type => T, value => Json}}]};
        Result ->
            Result
    end.

do_first(_Fun, _TypeInfo, [], _Json) ->
    {error, no_match};
do_first(Fun, TypeInfo, [Type | Rest], Json) ->
    case Fun(TypeInfo, Type, Json) of
        skip ->
            skip;
        {ok, Result} ->
            {ok, Result};
        {error, _} ->
            do_first(Fun, TypeInfo, Rest, Json)
    end.

-spec type_from_json(TypeInfo :: erldantic:type_info(),
                     TypeName :: atom(),
                     TypeArity :: non_neg_integer(),
                     TypeArgs :: [erldantic:ed_type()],
                     Json :: json:decode_value()) ->
                        {ok, term()} | {error, [erldantic:error()]}.
type_from_json(TypeInfo, TypeName, TypeArity, TypeArgs, Json) ->
    {ok, Type} = erldantic_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, TypeArgs),
    do_from_json(TypeInfo, TypeWithoutVars, Json).

apply_args(TypeInfo, Type, TypeArgs) when is_list(TypeArgs) ->
    ArgNames = arg_names(Type),
    NamedTypes =
        maps:from_list(
            lists:zip(ArgNames, TypeArgs)),
    type_replace_vars(TypeInfo, Type, NamedTypes).

arg_names(#ed_type_with_variables{vars = Args}) ->
    Args;
arg_names(_) ->
    [].

-spec type_replace_vars(TypeInfo :: erldantic:type_info(),
                        Type :: erldantic:ed_type(),
                        NamedTypes :: #{atom() => erldantic:ed_type()}) ->
                           erldantic:ed_type().
type_replace_vars(_TypeInfo, #ed_var{name = Name}, NamedTypes) ->
    maps:get(Name, NamedTypes, #ed_simple_type{type = term});
type_replace_vars(TypeInfo, #ed_type_with_variables{type = Type}, NamedTypes) ->
    case Type of
        #ed_union{types = UnionTypes} ->
            #ed_union{types =
                          lists:map(fun(UnionType) ->
                                       type_replace_vars(TypeInfo, UnionType, NamedTypes)
                                    end,
                                    UnionTypes)};
        #ed_map{fields = Fields, struct_name = StructName} ->
            #ed_map{fields =
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
                                  Fields),
                    struct_name = StructName};
        #ed_rec_ref{record_name = RecordName, field_types = RefFieldTypes} ->
            case erldantic_type_info:get_record(TypeInfo, RecordName) of
                {ok, #ed_rec{fields = Fields} = Rec} ->
                    NewRec = Rec#ed_rec{fields = record_replace_vars(Fields, RefFieldTypes)},
                    type_replace_vars(TypeInfo, NewRec, NamedTypes);
                error ->
                    erlang:error({missing_type, {record, RecordName}})
            end;
        #ed_remote_type{mfargs = {Module, TypeName, Args}} ->
            case erldantic_module_types:get(Module) of
                {ok, TypeInfo} ->
                    TypeArity = length(Args),
                    case erldantic_type_info:get_type(TypeInfo, TypeName, TypeArity) of
                        {ok, Type} ->
                            type_replace_vars(TypeInfo, Type, NamedTypes);
                        error ->
                            erlang:error({missing_type, TypeName})
                    end;
                {error, _} = Err ->
                    erlang:error(Err)
            end;
        #ed_list{type = ListType} ->
            #ed_list{type = type_replace_vars(TypeInfo, ListType, NamedTypes)}
    end;
type_replace_vars(_TypeInfo, #ed_rec{fields = Fields} = Rec, NamedTypes) ->
    Rec#ed_rec{fields =
                   lists:map(fun({Name, NType}) ->
                                {Name, type_replace_vars(_TypeInfo, NType, NamedTypes)}
                             end,
                             Fields)};
type_replace_vars(_TypeInfo, Type, _NamedTypes) ->
    Type.

-spec map_from_json(erldantic:type_info(),
                    [erldantic:map_field()],
                    json:decode_value()) ->
                       {ok, #{json:encode_value() => json:encode_value()}} |
                       {error, [erldantic:error()]}.
map_from_json(TypeInfo, MapFieldType, Json) when is_map(Json) ->
    Fun = fun ({map_field_assoc, FieldName, FieldType}, {FieldsAcc, JsonAcc}) ->
                  case maps:take(atom_to_binary(FieldName), JsonAcc) of
                      {FieldData, NewJsonAcc} ->
                          case do_from_json(TypeInfo, FieldType, FieldData) of
                              {ok, FieldJson} ->
                                  {ok, {[{FieldName, FieldJson}] ++ FieldsAcc, NewJsonAcc}};
                              {error, Errs} ->
                                  Errs2 =
                                      lists:map(fun(Err) -> err_append_location(Err, FieldName) end,
                                                Errs),
                                  {error, Errs2}
                          end;
                      error ->
                          {ok, {FieldsAcc, JsonAcc}}
                  end;
              ({map_field_exact, FieldName, FieldType}, {FieldsAcc, JsonAcc}) ->
                  case maps:take(atom_to_binary(FieldName), JsonAcc) of
                      {FieldData, NewJsonAcc} ->
                          case do_from_json(TypeInfo, FieldType, FieldData) of
                              {ok, FieldJson} ->
                                  {ok, {[{FieldName, FieldJson}] ++ FieldsAcc, NewJsonAcc}};
                              {error, Errs} ->
                                  Errs2 =
                                      lists:map(fun(Err) -> err_append_location(Err, FieldName) end,
                                                Errs),
                                  {error, Errs2}
                          end;
                      error ->
                          case erldantic_type:can_be_undefined(TypeInfo, FieldType) of
                              true ->
                                  {ok, {[{FieldName, undefined}] ++ FieldsAcc, JsonAcc}};
                              false ->
                                  {error, [#ed_error{type = missing_data, location = [FieldName]}]}
                          end
                  end;
              ({map_field_type_assoc, KeyType, ValueType}, {FieldsAcc, JsonAcc}) ->
                  case map_field_type_from_json(TypeInfo, KeyType, ValueType, JsonAcc) of
                      {ok, {NewFields, NewJsonAcc}} ->
                          {ok, {NewFields ++ FieldsAcc, NewJsonAcc}};
                      {error, Reason} ->
                          {error, Reason}
                  end;
              ({map_field_type_exact, KeyType, ValueType}, {FieldsAcc, JsonAcc}) ->
                  case map_field_type_from_json(TypeInfo, KeyType, ValueType, JsonAcc) of
                      {ok, {NewFields, NewJsonAcc}} ->
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
                                  {error, [NoExactMatch]};
                              _ ->
                                  {ok, {NewFields ++ FieldsAcc, NewJsonAcc}}
                          end;
                      {error, _} = Err ->
                          Err
                  end
          end,

    case erldantic_util:fold_until_error(Fun, {[], Json}, MapFieldType) of
        {ok, {Fields, NotMapped}} ->
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
        {error, _} = Err ->
            Err
    end;
map_from_json(_TypeInfo, _MapFieldType, Json) ->
    %% Return error when Json is not a map
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = map}, value => Json}}]}.

map_field_type_from_json(TypeInfo, KeyType, ValueType, Json) ->
    erldantic_util:fold_until_error(fun({Key, Value}, {FieldsAcc, JsonAcc}) ->
                                       case do_from_json(TypeInfo, KeyType, Key) of
                                           {ok, KeyJson} ->
                                               case do_from_json(TypeInfo, ValueType, Value) of
                                                   {ok, ValueJson} ->
                                                       {ok,
                                                        {FieldsAcc ++ [{KeyJson, ValueJson}],
                                                         maps:remove(Key, JsonAcc)}};
                                                   {error, Errs} ->
                                                       Errs2 =
                                                           lists:map(fun(Err) ->
                                                                        err_append_location(Err,
                                                                                            Key)
                                                                     end,
                                                                     Errs),
                                                       {error, Errs2}
                                               end;
                                           {error, _Errs} ->
                                               {ok, {FieldsAcc, JsonAcc}}
                                       end
                                    end,
                                    {[], Json},
                                    maps:to_list(Json)).

-spec record_from_json(TypeInfo :: erldantic:type_info(),
                       RecordName :: atom() | #ed_rec{},
                       Json :: json:decode_value(),
                       TypeArgs :: [erldantic:record_field()]) ->
                          {ok, term()} | {error, list()}.
record_from_json(TypeInfo, RecordName, Json, TypeArgs) when is_atom(RecordName) ->
    {ok, Record} = erldantic_type_info:get_record(TypeInfo, RecordName),
    record_from_json(TypeInfo, Record, Json, TypeArgs);
record_from_json(TypeInfo, #ed_rec{name = RecordName} = ARec, Json, TypeArgs) ->
    RecordInfo = record_replace_vars(ARec#ed_rec.fields, TypeArgs),
    do_record_from_json(TypeInfo, RecordName, RecordInfo, Json).

-spec do_record_from_json(TypeInfo :: erldantic:type_info(),
                          RecordName :: atom(),
                          RecordInfo :: list(),
                          Json :: json:decode_value()) ->
                             {ok, term()} | {error, list()}.
do_record_from_json(TypeInfo, RecordName, RecordInfo, Json) when is_map(Json) ->
    Fun = fun({FieldName, FieldType}) when is_atom(FieldName) ->
             case maps:find(atom_to_binary(FieldName), Json) of
                 {ok, RecordFieldData} ->
                     case do_from_json(TypeInfo, FieldType, RecordFieldData) of
                         {ok, FieldJson} ->
                             {ok, FieldJson};
                         {error, Errs} ->
                             Errs2 =
                                 lists:map(fun(Err) -> err_append_location(Err, FieldName) end,
                                           Errs),
                             {error, Errs2}
                     end;
                 error ->
                     case erldantic_type:can_be_undefined(TypeInfo, FieldType) of
                         true ->
                             {ok, undefined};
                         false ->
                             {error, [#ed_error{type = missing_data, location = [FieldName]}]}
                     end
             end
          end,
    case erldantic_util:map_until_error(Fun, RecordInfo) of
        {ok, Fields} ->
            {ok, list_to_tuple([RecordName | Fields])};
        {error, Errs} ->
            {error, Errs}
    end;
do_record_from_json(_TypeInfo, RecordName, _RecordInfo, Json) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{record_name => RecordName, record => Json}}]}.
