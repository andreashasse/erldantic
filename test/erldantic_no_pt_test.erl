-module(erldantic_no_pt_test).

-include_lib("eunit/include/eunit.hrl").

-include("record_type_introspect.hrl").

active_test() ->
    {ok, no_parse_trans} = c:c("test/no_parse_trans.erl", [debug_info]),
    Json = json:decode(<<"{\"first\":\"John\",\"last\":\"Doe\"}">>),
    ?assertEqual({ok, #{first => <<"John">>, last => <<"Doe">>}},
                 no_parse_trans:name_from_json(Json)).
