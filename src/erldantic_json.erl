-module(erldantic_json).

-export([type_to_json/3, type_from_json/3, record_to_json/3, record_from_json/3]).

-ignore_xref([{erldantic_json, type_to_json, 3},
              {erldantic_json, type_from_json, 3},
              {erldantic_json, record_to_json, 3},
              {erldantic_json, record_from_json, 3}]).

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

%% API

-doc("Converts an Erlang value to JSON format based on a type specification.\nThis function validates the given value against the specified type definition\nfrom the module and converts it to a JSON-encodable format.\nThe type must be of arity 0.\n\n### Returns\n{ok, JsonValue} if conversion succeeds, or {error, Errors} if validation fails").
-doc(#{params =>
           #{"Module" => "The module containing the type definition",
             "TypeName" => "The name of the type to validate against",
             "Value" => "The Erlang value to convert to JSON"}}).

-spec type_to_json(Module :: module(), TypeName :: atom(), Value :: dynamic()) ->
                      {ok, json:encode_value()} | {error, [erldantic:error()]}.
type_to_json(Module, TypeName, Value) when is_atom(Module) andalso is_atom(TypeName) ->
    TypeArity = 0,
    TypeRef = {type, TypeName, TypeArity},
    to_json_no_pt(Module, TypeRef, Value).

-doc("Converts a JSON value to an Erlang value based on a type specification.\nThis function validates the given JSON value against the specified type definition\nfrom the module and converts it to the corresponding Erlang value. The type must be\nof arity 0.\n\n### Returns\n{ok, ErlangValue} if conversion succeeds, or {error, Errors} if validation fails").
-doc(#{params =>
           #{"Json" => "The JSON value to convert to Erlang format",
             "Module" => "The module containing the type definition",
             "TypeName" => "The name of the type to validate against"}}).

-spec type_from_json(Module :: module(),
                     TypeName :: atom(),
                     Json :: json:decode_value()) ->
                        {ok, dynamic()} | {error, [erldantic:error()]}.
type_from_json(Module, TypeName, Json) when is_atom(Module) andalso is_atom(TypeName) ->
    TypeArity = 0,
    TypeRef = {type, TypeName, TypeArity},
    from_json_no_pt(Module, TypeRef, Json).

-doc("Converts an Erlang record to JSON format based on a record specification.\nThis function validates the given record value against the specified record definition\nfrom the module and converts it to a JSON object.\n\n### Returns\n{ok, JsonObject} if conversion succeeds, or {error, Errors} if validation fails").
-doc(#{params =>
           #{"Module" => "The module containing the record definition",
             "RecordName" => "The name of the record to validate against",
             "Value" => "The Erlang record value to convert to JSON"}}).

-spec record_to_json(Module :: module(), RecordName :: atom(), Value :: dynamic()) ->
                        {ok, json:encode_value()} | {error, [erldantic:error()]}.
record_to_json(Module, RecordName, Value)
    when is_atom(Module) andalso is_atom(RecordName) ->
    to_json_no_pt(Module, {record, RecordName}, Value).

-doc("Converts a JSON value to an Erlang record based on a record specification.\nThis function validates the given JSON value against the specified record definition\nfrom the module and converts it to the corresponding Erlang record.\n\n### Returns\n{ok, ErlangRecord} if conversion succeeds, or {error, Errors} if validation fails").
-doc(#{params =>
           #{"Json" => "The JSON value to convert to Erlang record format",
             "Module" => "The module containing the record definition",
             "RecordName" => "The name of the record to validate against"}}).

-spec record_from_json(Module :: module(),
                       RecordName :: atom(),
                       Json :: json:decode_value()) ->
                          {ok, dynamic()} | {error, [erldantic:error()]}.
record_from_json(Module, RecordName, Json)
    when is_atom(Module) andalso is_atom(RecordName) ->
    from_json_no_pt(Module, {record, RecordName}, Json).

%% INTERNAL

-spec to_json_no_pt(Module :: module(),
                    TypeRef :: erldantic:ed_type_reference(),
                    Data :: dynamic()) ->
                       {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json_no_pt(Module, TypeRef, Data) ->
    case erldantic_module_types:get(Module) of
        {ok, TypeInfo} ->
            to_json(TypeInfo, TypeRef, Data);
        {error, _} = Err ->
            Err
    end.

-spec to_json(erldantic:type_info(), erldantic:ed_type_or_ref(), Data :: dynamic()) ->
                 {ok, json:encode_value()} | {error, [erldantic:error()]}.
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
                      TypeOrRecord :: erldantic:ed_type_or_ref(),
                      Json :: json:decode_value()) ->
                         {ok, dynamic()} | {error, [erldantic:error()]}.
from_json_no_pt(Module, TypeRef, Json) ->
    case erldantic_module_types:get(Module) of
        {ok, TypeInfo} ->
            from_json(TypeInfo, TypeRef, Json);
        {error, _} = Err ->
            Err
    end.

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
    case TypeInfo of
        #{{type, TypeName, TypeArity} := Type} ->
            TypeWithoutVars = apply_args(TypeInfo, Type, TypeArgs),
            do_to_json(TypeInfo, TypeWithoutVars, Data);
        #{} ->
            {error, [#ed_error{type = missing_type, location = []}]}
    end;
do_to_json(_TypeInfo, #ed_simple_type{type = NotSupported} = Type, _Data)
    when NotSupported =:= pid
         orelse NotSupported =:= port
         orelse NotSupported =:= reference
         orelse NotSupported =:= bitstring
         orelse NotSupported =:= nonempty_bitstring
         orelse NotSupported =:= none ->
    {error,
     [#ed_error{type = type_not_supported,
                location = [],
                ctx = #{type => Type}}]};
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
    data_to_json(TypeInfo, TypeName, TypeArity, Data);
do_to_json(TypeInfo, #ed_map{} = Map, Data) ->
    map_to_json(TypeInfo, Map, Data);
do_to_json(_TypeInfo, #ed_remote_type{mfargs = {Module, TypeName, Args}}, Data) ->
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
do_to_json(_TypeInfo, #ed_maybe_improper_list{} = Type, Data) ->
    {error,
     [#ed_error{type = not_implemented,
                location = [],
                ctx = #{type => Type, value => Data}}]};
do_to_json(_TypeInfo, #ed_nonempty_improper_list{} = Type, Data) ->
    {error,
     [#ed_error{type = not_implemented,
                location = [],
                ctx = #{type => Type, value => Data}}]};
%% Not supported types
do_to_json(_TypeInfo, #ed_tuple{} = Type, _Data) ->
    {error,
     [#ed_error{type = type_not_supported,
                location = [],
                ctx = #{type => Type}}]};
do_to_json(_TypeInfo, #ed_function{} = Type, _Data) ->
    {error,
     [#ed_error{type = type_not_supported,
                location = [],
                ctx = #{type => Type}}]};
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

-spec data_to_json(erldantic:type_info(), atom(), arity(), Data :: dynamic()) ->
                      {ok, json:encode_value()} | {error, [erldantic:error()]} | skip.
data_to_json(TypeInfo, TypeName, TypeArity, Data) ->
    case TypeInfo of
        #{{type, TypeName, TypeArity} := Type} ->
            do_to_json(TypeInfo, Type, Data);
        #{} ->
            {error, [#ed_error{type = missing_type, location = [TypeName]}]}
    end.

map_to_json(TypeInfo, #ed_map{fields = Fields}, Data) when is_map(Data) ->
    case map_fields_to_json(TypeInfo, Fields, Data) of
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
                          case can_be_undefined(TypeInfo, FieldType) of
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

-spec record_to_json(TypeInfo :: map(),
                     RecordName :: atom() | #ed_rec{},
                     Record :: term(),
                     TypeArgs :: [{atom(), erldantic:ed_type()}]) ->
                        {ok, #{atom() => json:encode_value()}} | {error, [erldantic:error()]}.
record_to_json(TypeInfo, RecordName, Record, TypeArgs) when is_atom(RecordName) ->
    RecordInfo = maps:get({record, RecordName}, TypeInfo),
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
    RecFieldTypes = apply_record_arg_types(Fields, TypeArgs),
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

apply_record_arg_types(RecordInfo, TypeArgs) ->
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

-spec from_json(TypeInfo :: map(),
                Type :: erldantic:ed_type_or_ref(),
                Json :: json:decode_value()) ->
                   {ok, term()} | {error, [erldantic:error()]}.
from_json(TypeInfo, {record, RecordName}, Json) when is_atom(RecordName) ->
    record_from_json(TypeInfo, RecordName, Json, []);
from_json(TypeInfo, #ed_rec{} = Rec, Json) ->
    record_from_json(TypeInfo, Rec, Json, []);
from_json(_TypeInfo, #ed_remote_type{mfargs = {Module, TypeName, TypeArgs}}, Json) ->
    case erldantic_module_types:get(Module) of
        {ok, TypeInfo} ->
            TypeArity = length(TypeArgs),
            type_from_json(TypeInfo, TypeName, TypeArity, TypeArgs, Json);
        {error, _} = Err ->
            Err
    end;
from_json(TypeInfo, #ed_rec_ref{record_name = RecordName, field_types = TypeArgs}, Json)
    when is_atom(RecordName) ->
    record_from_json(TypeInfo, RecordName, Json, TypeArgs);
from_json(TypeInfo, #ed_map{fields = Fields}, Json) ->
    map_from_json(TypeInfo, Fields, Json);
from_json(TypeInfo, #ed_user_type_ref{type_name = TypeName, variables = TypeArgs}, Json)
    when is_atom(TypeName) ->
    type_from_json(TypeInfo, TypeName, length(TypeArgs), TypeArgs, Json);
from_json(TypeInfo, #ed_nonempty_list{type = Type}, Data) ->
    nonempty_list_from_json(TypeInfo, Type, Data);
from_json(TypeInfo, #ed_list{type = Type}, Data) ->
    list_from_json(TypeInfo, Type, Data);
from_json(_TypeInfo, #ed_simple_type{type = NotSupported} = T, Value)
    when NotSupported =:= pid
         orelse NotSupported =:= port
         orelse NotSupported =:= reference
         orelse NotSupported =:= bitstring
         orelse NotSupported =:= nonempty_bitstring
         orelse NotSupported =:= none ->
    {error,
     [#ed_error{type = type_not_supported,
                location = [],
                ctx = #{type => T, value => Value}}]};
from_json(_TypeInfo, #ed_simple_type{type = PrimaryType} = T, Json) ->
    case check_type_from_json(PrimaryType, Json) of
        {true, NewValue} ->
            {ok, NewValue};
        false ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => T, value => Json}}]}
    end;
from_json(_TypeInfo, #ed_literal{value = Literal}, Literal) ->
    {ok, Literal};
from_json(_TypeInfo, #ed_literal{value = Literal} = Type, Value) ->
    case try_convert_to_literal(Literal, Value) of
        {ok, Literal} ->
            {ok, Literal};
        false ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => Type, value => Value}}]}
    end;
from_json(TypeInfo, {type, TypeName, TypeArity}, Json) when is_atom(TypeName) ->
    type_from_json(TypeInfo, TypeName, TypeArity, [], Json);
from_json(TypeInfo, #ed_union{} = Type, Json) ->
    union(fun from_json/3, TypeInfo, Type, Json);
from_json(_TypeInfo,
          #ed_range{type = integer,
                    lower_bound = Min,
                    upper_bound = Max},
          Value)
    when Min =< Value, Value =< Max ->
    {ok, Value};
from_json(_TypeInfo,
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
from_json(_TypeInfo, #ed_maybe_improper_list{} = Type, Value) ->
    %% erlang:error(...) for not impolemented or supported stuff? It is not an error that should be handled by the user?
    {error,
     [#ed_error{type = not_implemented,
                location = [],
                ctx = #{type => Type, value => Value}}]};
from_json(_TypeInfo, #ed_nonempty_improper_list{} = Type, Value) ->
    %% erlang:error(...) for not impolemented or supported stuff? It is not an error that should be handled by the user?
    {error,
     [#ed_error{type = not_implemented,
                location = [],
                ctx = #{type => Type, value => Value}}]};
from_json(_TypeInfo, #ed_function{} = Type, Value) ->
    {error,
     [#ed_error{type = type_not_supported,
                location = [],
                ctx = #{type => Type, value => Value}}]};
from_json(_TypeInfo, #ed_tuple{} = Type, Value) ->
    {error,
     [#ed_error{type = type_not_supported,
                location = [],
                ctx = #{type => Type, value => Value}}]};
from_json(_TypeInfo, Type, Value) ->
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
             case from_json(TypeInfo, Type, Item) of
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

check_type_from_json(string, Json) when is_binary(Json) ->
    {true, unicode:characters_to_list(Json)};
check_type_from_json(nonempty_string, Json) when is_binary(Json), byte_size(Json) > 0 ->
    {true, unicode:characters_to_list(Json)};
check_type_from_json(iodata, Json) when is_binary(Json) ->
    {true, Json};
check_type_from_json(iolist, Json) when is_binary(Json) ->
    {true, Json};
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
    {true, unicode:characters_to_binary(Json)};
check_type_to_json(iodata, Json) when is_binary(Json) ->
    {true, Json};
check_type_to_json(iodata, Json) when is_list(Json) ->
    {true, iolist_to_binary(Json)};
check_type_to_json(iolist, Json) when is_list(Json) ->
    {true, iolist_to_binary(Json)};
check_type_to_json(nonempty_string, Json) when is_list(Json), Json =/= [] ->
    case io_lib:printable_list(Json) of
        true ->
            {true, unicode:characters_to_binary(Json)};
        false ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx =
                            #{type => #ed_simple_type{type = string},
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
                            #{type => #ed_simple_type{type = string},
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
        #ed_map{fields = Fields} ->
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
                                  Fields)};
        #ed_rec_ref{record_name = RecordName, field_types = TypeArgs} ->
            case TypeInfo of
                #{{record, RecordName} := #ed_rec{fields = Fields} = Rec} ->
                    NewFields = apply_record_arg_types(Fields, TypeArgs),
                    NewRec = Rec#ed_rec{fields = NewFields},
                    type_replace_vars(TypeInfo, NewRec, NamedTypes);
                #{} ->
                    erlang:error({missing_type, {record, RecordName}})
            end;
        #ed_remote_type{mfargs = {Module, TypeName, Args}} ->
            case erldantic_module_types:get(Module) of
                {ok, TypeInfo} ->
                    TypeArity = length(Args),
                    case TypeInfo of
                        #{{type, TypeName, TypeArity} := Type} ->
                            type_replace_vars(TypeInfo, Type, NamedTypes);
                        #{} ->
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
                       {ok, #{json:encode_value() => json:encode_value()}} | {error, [#ed_error{}]}.
map_from_json(TypeInfo, MapFieldType, Json) when is_map(Json) ->
    Fun = fun ({map_field_assoc, FieldName, FieldType}, {FieldsAcc, JsonAcc}) ->
                  case maps:take(atom_to_binary(FieldName), JsonAcc) of
                      {FieldData, NewJsonAcc} ->
                          case from_json(TypeInfo, FieldType, FieldData) of
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
                          case from_json(TypeInfo, FieldType, FieldData) of
                              {ok, FieldJson} ->
                                  {ok, {[{FieldName, FieldJson}] ++ FieldsAcc, NewJsonAcc}};
                              {error, Errs} ->
                                  Errs2 =
                                      lists:map(fun(Err) -> err_append_location(Err, FieldName) end,
                                                Errs),
                                  {error, Errs2}
                          end;
                      error ->
                          case can_be_undefined(TypeInfo, FieldType) of
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
                                       case from_json(TypeInfo, KeyType, Key) of
                                           {ok, KeyJson} ->
                                               case from_json(TypeInfo, ValueType, Value) of
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

-spec record_from_json(TypeInfo :: map(),
                       RecordName :: atom() | #ed_rec{},
                       Json :: json:decode_value(),
                       TypeArgs :: [erldantic:record_field()]) ->
                          {ok, term()} | {error, list()}.
record_from_json(TypeInfo, RecordName, Json, TypeArgs) when is_atom(RecordName) ->
    ARec = maps:get({record, RecordName}, TypeInfo),
    record_from_json(TypeInfo, ARec, Json, TypeArgs);
record_from_json(TypeInfo, #ed_rec{name = RecordName} = ARec, Json, TypeArgs) ->
    RecordInfo = apply_record_arg_types(ARec#ed_rec.fields, TypeArgs),
    do_record_from_json(TypeInfo, RecordName, RecordInfo, Json).

-spec do_record_from_json(TypeInfo :: map(),
                          RecordName :: atom(),
                          RecordInfo :: list(),
                          Json :: json:decode_value()) ->
                             {ok, term()} | {error, list()}.
do_record_from_json(TypeInfo, RecordName, RecordInfo, Json) when is_map(Json) ->
    Fun = fun({FieldName, FieldType}) when is_atom(FieldName) ->
             case maps:find(atom_to_binary(FieldName), Json) of
                 {ok, RecordFieldData} ->
                     case from_json(TypeInfo, FieldType, RecordFieldData) of
                         {ok, FieldJson} ->
                             {ok, FieldJson};
                         {error, Errs} ->
                             Errs2 =
                                 lists:map(fun(Err) -> err_append_location(Err, FieldName) end,
                                           Errs),
                             {error, Errs2}
                     end;
                 error ->
                     case can_be_undefined(TypeInfo, FieldType) of
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

-spec can_be_undefined(TypeInfo :: erldantic:type_info(), Type :: erldantic:ed_type()) ->
                          boolean().
can_be_undefined(TypeInfo, Type) ->
    case Type of
        #ed_type_with_variables{type = Type2} ->
            can_be_undefined(TypeInfo, Type2);
        #ed_union{types = Types} ->
            lists:member(#ed_literal{value = undefined}, Types);
        #ed_literal{value = undefined} ->
            true;
        #ed_user_type_ref{type_name = TypeName, variables = TypeArgs} ->
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
