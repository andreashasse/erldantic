-module(range_test).

-include_lib("eunit/include/eunit.hrl").

-type base64() :: $A..$Z | $a..$z | $0..$9 | $+ | $/ | $- | $_ | $=.
-type my_fancy_string() :: [$a..$z].

char_test() ->
    % Convert to JSON
    Val1 = $=,
    Val2 = $B,
    BadVal = 10000,
    ?assertEqual({ok, <<"61">>}, to_json(Val1)),
    ?assertEqual({ok, Val1}, from_json(<<"61">>)),
    ?assertEqual({ok, <<"66">>}, to_json(Val2)),
    ?assertEqual({ok, Val2}, from_json(<<"66">>)),
    ?assertMatch({error, _}, to_json(BadVal)).

-spec to_json(base64()) -> {ok, json:json()} | {error, [erldantic:ed_error()]}.
to_json(Value) ->
    erldantic:encode(json, ?MODULE, base64, Value).

-spec from_json(binary()) -> {ok, base64()} | {error, [erldantic:ed_error()]}.
from_json(Json) ->
    erldantic:decode(json, ?MODULE, base64, Json).

fancy_string_test() ->
    %% This test documents that erldantic currently doesn't make a difference between char and int.
    OkVal = "hello",
    BadVal = "Hello",
    ?assertEqual({ok, <<"[104,101,108,108,111]">>}, to_fancy_string_json(OkVal)),
    ?assertEqual({ok, OkVal}, from_fancy_json(<<"[104,101,108,108,111]">>)),
    ?assertMatch({error, _}, to_fancy_string_json(BadVal)).

-spec to_fancy_string_json(my_fancy_string()) ->
    {ok, json:json()} | {error, [erldantic:ed_error()]}.
to_fancy_string_json(Value) ->
    case erldantic:encode(json, ?MODULE, my_fancy_string, Value) of
        {ok, Json} ->
            {ok, iolist_to_binary(Json)};
        {error, Errors} ->
            {error, Errors}
    end.

-spec from_fancy_json(binary()) -> {ok, my_fancy_string()} | {error, [erldantic:ed_error()]}.
from_fancy_json(Json) ->
    erldantic:decode(json, ?MODULE, my_fancy_string, Json).
