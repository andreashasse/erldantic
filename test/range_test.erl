-module(range_test).

-include_lib("eunit/include/eunit.hrl").

-type base64() :: $A..$Z | $a..$z | $0..$9 | $+ | $/ | $- | $_ | $=.

undefined_record_test() ->
    % Convert to JSON
    Val1 = $=,
    Val2 = $B,
    ?assertEqual({ok, <<"61">>}, to_json(Val1)),
    ?assertEqual({ok, Val1}, from_json(<<"61">>)),
    ?assertEqual({ok, <<"66">>}, to_json(Val2)),
    ?assertEqual({ok, Val2}, from_json(<<"66">>)).

-spec to_json(base64()) -> {ok, json:json()} | {error, [erldantic:ed_error()]}.
to_json(Value) ->
    erldantic:encode(json, ?MODULE, base64, Value).

-spec from_json(binary()) -> {ok, base64()} | {error, [erldantic:ed_error()]}.
from_json(Json) ->
    erldantic:decode(json, ?MODULE, base64, Json).
