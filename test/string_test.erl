-module(string_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

-type my_string() :: string().

string_test() ->
    %% not printable string
    %% FIXME: match more specific
    ?assertMatch({error, _},
                 erldantic_json:type_to_json(?MODULE, my_string, [65536, 100, 210, 81])),
    ?assertMatch({error, _},
                 erldantic_json:type_from_json(?MODULE,
                                               my_string,
                                               <<240, 144, 128, 128, 100, 195, 146, 81>>)).
