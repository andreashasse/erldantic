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
    ?assertEqual({ok, #{first => <<"John">>, last => <<"Doe">>}},
                 no_parse_trans:name_from_json(Name)).

my_weird_union_to_json_test() ->
    Json =
        json:decode(<<"{\"city\": \"sollentuna\", \"score\": {\"value\": 5, "
                      "\"comment\": {\"lang\": \"en\", \"text\": \"ok\"}}}"/utf8>>),
    ?assertEqual({ok,
                  #{city => <<"sollentuna">>,
                    score => #{comment => #{lang => <<"en">>, text => <<"ok">>}, value => 5}}},
                 person:my_weird_union_from_json(Json)).
