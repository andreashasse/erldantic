-module(string_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

-type my_string() :: string().

string_test() ->
    %% not printable string
    %% FIXME: match more specific
    ?assertMatch({error, _},
                 impala_json:to_json(?MODULE, {type, my_string, 0}, [1655379, 100, 210, 81])),
    ?assertMatch({error, _},
                 impala_json:from_json(?MODULE,
                                       my_string,
                                       <<240, 144, 128, 128, 100, 195, 146, 81>>)).
