-module(term_field_test).

-include_lib("eunit/include/eunit.hrl").

-record(data_with_term, {id :: string(), value :: term()}).

%% Test function to validate term field handling
validate_term_field_test() ->
    % Create test records with different term types
    RecordWithString = #data_with_term{id = "record1", value = "string value"},
    RecordWithInteger = #data_with_term{id = "record2", value = 42},
    RecordWithMap = #data_with_term{id = "record3", value = #{a => 1, b => 2}},
    RecordWithList = #data_with_term{id = "record4", value = [1, 2, 3]},
    RecordWithTuple = #data_with_term{id = "record5", value = {a, b, c}},

    % Test to_json conversion
    {ok, JsonWithString} = to_json(RecordWithString),
    {ok, JsonWithInteger} = to_json(RecordWithInteger),
    {ok, JsonWithMap} = to_json(RecordWithMap),
    {ok, JsonWithList} = to_json(RecordWithList),
    {ok, JsonWithTuple} = to_json(RecordWithTuple),

    % Check conversion results
    ?assertEqual(#{id => <<"record1">>, value => "string value"}, JsonWithString),
    ?assertEqual(#{id => <<"record2">>, value => 42}, JsonWithInteger),
    ?assertEqual(#{id => <<"record3">>, value => #{a => 1, b => 2}}, JsonWithMap),
    ?assertEqual(#{id => <<"record4">>, value => [1, 2, 3]}, JsonWithList),
    ?assertEqual(#{id => <<"record5">>, value => {a, b, c}}, JsonWithTuple),

    % Test from_json conversion
    {ok, RecordFromString} =
        from_json(#{<<"id">> => <<"record1">>, <<"value">> => <<"string value">>}),
    {ok, RecordFromInteger} = from_json(#{<<"id">> => <<"record2">>, <<"value">> => 42}),
    {ok, RecordFromMap} =
        from_json(#{<<"id">> => <<"record3">>, <<"value">> => #{<<"a">> => 1, <<"b">> => 2}}),
    {ok, RecordFromList} = from_json(#{<<"id">> => <<"record4">>, <<"value">> => [1, 2, 3]}),
    {ok, RecordFromTuple} = from_json(#{<<"id">> => <<"record5">>, <<"value">> => [a, b, c]}),

    % Check conversion results
    ?assertEqual("record1", RecordFromString#data_with_term.id),
    ?assertEqual(<<"string value">>, RecordFromString#data_with_term.value),

    ?assertEqual("record2", RecordFromInteger#data_with_term.id),
    ?assertEqual(42, RecordFromInteger#data_with_term.value),

    ?assertEqual("record3", RecordFromMap#data_with_term.id),
    ?assertMatch(#{<<"a">> := 1, <<"b">> := 2}, RecordFromMap#data_with_term.value),

    ?assertEqual("record4", RecordFromList#data_with_term.id),
    ?assertEqual([1, 2, 3], RecordFromList#data_with_term.value),

    ?assertEqual("record5", RecordFromTuple#data_with_term.id),
    ?assertEqual([a, b, c], RecordFromTuple#data_with_term.value).

-spec to_json(#data_with_term{}) ->
    {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json(Data) ->
    erldantic_json:to_json(?MODULE, {record, data_with_term}, Data).

-spec from_json(json:encode_value()) ->
    {ok, #data_with_term{}} | {error, [erldantic:error()]}.
from_json(Json) ->
    erldantic_json:from_json(?MODULE, {record, data_with_term}, Json).
