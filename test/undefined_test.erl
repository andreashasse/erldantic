-module(undefined_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/record_type_introspect.hrl").

%% Type definitions
-type maybe_string() :: string() | undefined.
-type maybe_integer() :: integer() | undefined.
-type int_list() :: [integer()].
-type maybe_list() :: int_list() | undefined.
-type string_map() :: #{atom() => string()}.
-type maybe_map() :: string_map() | undefined.

%% Record definition with typed fields
-record(undefined_test_record,
        {string_field :: maybe_string(),
         int_field :: maybe_integer(),
         list_field :: maybe_list(),
         map_field :: maybe_map()}).

%% Define map type
-type test_map() ::
    #{key1 := string(),
      key2 := maybe_string(),
      key3 := integer()}.
%% Define list type that can contain undefined values
-type maybe_int_list() :: [integer() | undefined].

%% Test conversion of records with undefined fields
undefined_record_test() ->
    % Create record with all fields set to undefined
    Record1 =
        #undefined_test_record{string_field = undefined,
                               int_field = undefined,
                               list_field = undefined,
                               map_field = undefined},

    % Convert to JSON
    {ok, Json1} = to_json(Record1),

    % Verify JSON contains only record keys with undefined values
    ?assertEqual(#{}, Json1),

    % Create record with some fields defined and some undefined
    Record2 =
        #undefined_test_record{string_field = "hello",
                               int_field = undefined,
                               list_field = [1, 2, 3],
                               map_field = undefined},

    % Convert to JSON
    {ok, Json2} = to_json(Record2),

    % Verify JSON contains only defined fields
    ?assertEqual(#{string_field => <<"hello">>, list_field => [1, 2, 3]}, Json2),

    % Test from_json with undefined fields
    {ok, RecordFromJson1} =
        from_json(#{<<"string_field">> => <<"test">>, <<"int_field">> => 42}),

    % Verify fields were properly set, and unspecified fields are undefined
    ?assertEqual("test", RecordFromJson1#undefined_test_record.string_field),
    ?assertEqual(42, RecordFromJson1#undefined_test_record.int_field),
    ?assertEqual(undefined, RecordFromJson1#undefined_test_record.list_field),
    ?assertEqual(undefined, RecordFromJson1#undefined_test_record.map_field).

%% Test with map containing undefined values
undefined_map_test() ->
    % Create map with undefined value
    MapWithUndefined =
        #{key1 => "value1",
          key2 => undefined,
          key3 => 123},

    % Convert to JSON using map type
    {ok, JsonMap} = to_json_map(MapWithUndefined),

    % Verify undefined values are skipped in JSON
    ?assertEqual(#{key1 => <<"value1">>, key3 => 123}, JsonMap),

    % Test from_json with missing keys
    {ok, MapFromJson} = from_json_map(#{<<"key1">> => <<"value1">>, <<"key3">> => 123}),

    % Verify missing keys are set to undefined
    ?assertEqual(#{key1 => "value1",
                   key2 => undefined,
                   key3 => 123},
                 MapFromJson).

%% Test with lists containing undefined values
undefined_list_test() ->
    % Create list with some undefined values
    ListWithUndefined = [1, undefined, 3, undefined, 5],

    % Convert to JSON using list type
    {ok, JsonList} = to_json_list(ListWithUndefined),

    % Verify undefined values are preserved in the JSON list
    ?assertEqual([1, undefined, 3, undefined, 5], JsonList),

    % Test from_json with a list containing undefined
    {ok, ListFromJson} = from_json_list([1, undefined, 3, undefined, 6]),

    % Verify list structure is preserved
    ?assertEqual([1, undefined, 3, undefined, 6], ListFromJson).

%% Test with simple scalar types
undefined_scalar_test() ->
    % Test maybe_string type
    {ok, JsonUndefined} = to_json_maybe_string(undefined),
    {ok, JsonString} = to_json_maybe_string("test string"),

    % Verify conversions
    ?assertEqual(undefined, JsonUndefined),
    ?assertEqual(<<"test string">>, JsonString),

    % Test from_json with undefined and string
    {ok, UndefinedFromJson} = from_json_maybe_string(undefined),
    {ok, StringFromJson} = from_json_maybe_string(<<"another string">>),

    % Verify conversions
    ?assertEqual(undefined, UndefinedFromJson),
    ?assertEqual("another string", StringFromJson).

%% Helper functions for record type
-spec to_json(#undefined_test_record{}) -> {ok, map()} | {error, [#ed_error{}]}.
to_json(Record) ->
    erldantic_json:to_json_no_pt({?MODULE, {record, undefined_test_record}, []}, Record).

-spec from_json(map()) -> {ok, #undefined_test_record{}} | {error, [#ed_error{}]}.
from_json(Json) ->
    erldantic_json:from_json_no_pt({?MODULE, {record, undefined_test_record}, []}, Json).

%% Helper functions for map type
-spec to_json_map(test_map()) -> {ok, map()} | {error, [#ed_error{}]}.
to_json_map(Map) ->
    erldantic_json:to_json_no_pt({?MODULE, {type, test_map}, []}, Map).

-spec from_json_map(map()) -> {ok, test_map()} | {error, [#ed_error{}]}.
from_json_map(Json) ->
    erldantic_json:from_json_no_pt({?MODULE, {type, test_map}, []}, Json).

%% Helper functions for list type
-spec to_json_list(maybe_int_list()) -> {ok, list()} | {error, [#ed_error{}]}.
to_json_list(List) ->
    erldantic_json:to_json_no_pt({?MODULE, {type, maybe_int_list}, []}, List).

-spec from_json_list(list()) -> {ok, maybe_int_list()} | {error, [#ed_error{}]}.
from_json_list(Json) ->
    erldantic_json:from_json_no_pt({?MODULE, {type, maybe_int_list}, []}, Json).

%% Helper functions for maybe_string type
-spec to_json_maybe_string(maybe_string()) ->
                              {ok, binary() | undefined} | {error, [#ed_error{}]}.
to_json_maybe_string(MaybeString) ->
    erldantic_json:to_json_no_pt({?MODULE, {type, maybe_string}, []}, MaybeString).

-spec from_json_maybe_string(binary() | undefined) ->
                                {ok, maybe_string()} | {error, [#ed_error{}]}.
from_json_maybe_string(Json) ->
    erldantic_json:from_json_no_pt({?MODULE, {type, maybe_string}, []}, Json).
