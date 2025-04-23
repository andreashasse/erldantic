-module(erldantic_no_pt_test).

-include_lib("eunit/include/eunit.hrl").

-include("record_type_introspect.hrl").

active_test() ->
    {ok, no_parse_trans} = c:c("test/no_parse_trans.erl", [debug_info]),
    Json = json:decode(<<"{\"first\":\"John\",\"last\":\"Doe\"}">>),
    ?assertEqual({ok, #{first => "John", last => "Doe"}},
                 no_parse_trans:name_from_json(Json)).

active_to_json_test() ->
    {ok, no_parse_trans} = c:c("test/no_parse_trans.erl", [debug_info]),
    Name = #{first => "John", last => "Doe"},
    {ok, Json} = no_parse_trans:name_to_json(Name),
    ?assertEqual(#{first => <<"John">>, last => <<"Doe">>}, Json).

my_weird_union_to_json_test() ->
    Json =
        json:decode(<<"{\"city\": \"sollentuna\", \"score\": {\"value\": 5, "
                      "\"comment\": {\"lang\": \"en\", \"text\": \"ok\"}}}"/utf8>>),
    ?assertEqual({ok,
                  #{city => "sollentuna",
                    score => #{comment => #{lang => "en", text => "ok"}, value => 5}}},
                 person:my_weird_union_from_json(Json)).

invalid_types_with_name_map_test() ->
    {ok, no_parse_trans} = c:c("test/no_parse_trans.erl", [debug_info]),
    % Test with integer instead of map
    ?assertEqual({error,
                  [#ed_error{type = type_mismatch,
                             location = [],
                             ctx = #{type => {type, map}, value => 42}}]},
                 no_parse_trans:name_to_json(42)),

    % Test with list instead of map
    ?assertEqual({error,
                  [#ed_error{type = type_mismatch,
                             location = [],
                             ctx = #{type => {type, map}, value => ["John", "Doe"]}}]},
                 no_parse_trans:name_to_json(["John", "Doe"])),

    % Test with atom instead of map
    ?assertEqual({error,
                  [#ed_error{type = type_mismatch,
                             location = [],
                             ctx = #{type => {type, map}, value => name}}]},
                 no_parse_trans:name_to_json(name)),

    % Test with map missing required field
    ?assertEqual({error, [#ed_error{type = missing_data, location = [last]}]},
                 no_parse_trans:name_to_json(#{first => "John"})),

    % Test with map containing wrong type for field
    ?assertEqual({error,
                  [#ed_error{type = type_mismatch,
                             location = [first],
                             ctx = #{type => {type, string}, value => 123}}]},
                 no_parse_trans:name_to_json(#{first => 123, last => "Doe"})),

    ?assertEqual({error,
                  [#ed_error{type = type_mismatch,
                             location = [last],
                             ctx = #{type => {type, string}, value => 456}}]},
                 no_parse_trans:name_to_json(#{first => "John", last => 456})).
