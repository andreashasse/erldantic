-module(simple_types_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-compile(nowarn_unused_type).

-type my_arity() :: arity().
-type my_byte() :: byte().
-type my_char() :: char().
-type my_mfa() :: mfa().
-type my_any() :: any().
-type my_timeout() :: timeout().
-type my_pid() :: pid().
-type my_iodata() :: iodata().
-type my_iolist() :: iolist().
-type my_port() :: port().
-type my_reference() :: reference().
-type my_node() :: node().
-type my_identifier() :: identifier().
-type my_literal() :: 1.
-type my_list() :: list().
-type my_term() :: term().
-type my_nonempty_list() :: nonempty_list().
-type my_nil() :: []. %% My code formater re-writes nil to [].

missing_test() ->
    {ok, Types} = erldantic_abstract_code:types_in_module(?MODULE),

    %% arity
    ?assertEqual({range, integer, 0, 255}, maps:get({type, my_arity, 0}, Types)),
    ?assertEqual({ok, 42}, erldantic_json:type_to_json(?MODULE, my_arity, 42)),
    ?assertEqual({ok, 42}, erldantic_json:type_from_json(?MODULE, my_arity, 42)),

    %% byte
    ?assertEqual({range, integer, 0, 255}, maps:get({type, my_byte, 0}, Types)),
    ?assertEqual({ok, 42}, erldantic_json:type_to_json(?MODULE, my_byte, 42)),
    ?assertEqual({ok, 42}, erldantic_json:type_from_json(?MODULE, my_byte, 42)),

    %% char
    ?assertEqual({range, integer, 0, 1114111}, maps:get({type, my_char, 0}, Types)),
    ?assertEqual({ok, 42}, erldantic_json:type_to_json(?MODULE, my_char, 42)),
    ?assertEqual({ok, 42}, erldantic_json:type_from_json(?MODULE, my_char, 42)),

    %% mfa
    ?assertEqual(#a_tuple{fields = [{type, atom}, {type, atom}, {range, integer, 0, 255}]},
                 maps:get({type, my_mfa, 0}, Types)),
    ?assertMatch({error, [#ed_error{type = type_not_supported}]},
                 erldantic_json:type_to_json(?MODULE, my_mfa, {module, function, 42})),
    ?assertMatch({error, [#ed_error{type = type_not_supported}]},
                 erldantic_json:type_from_json(?MODULE, my_mfa, {module, function, 42})),

    %% any
    ?assertEqual({type, term}, maps:get({type, my_any, 0}, Types)),
    ?assertEqual({ok, 42}, erldantic_json:type_to_json(?MODULE, my_any, 42)),
    ?assertEqual({ok, 42}, erldantic_json:type_from_json(?MODULE, my_any, 42)),

    %% timeout
    ?assertEqual({union, [{type, non_neg_integer}, {literal, infinity}]},
                 maps:get({type, my_timeout, 0}, Types)),
    ?assertMatch({error, [#ed_error{type = no_match}]},
                 erldantic_json:type_to_json(?MODULE, my_timeout, <<"infinity">>)),
    ?assertEqual({ok, infinity}, erldantic_json:type_to_json(?MODULE, my_timeout, infinity)),
    ?assertEqual({ok, 42}, erldantic_json:type_to_json(?MODULE, my_timeout, 42)),
    ?assertEqual({ok, infinity},
                 erldantic_json:type_from_json(?MODULE, my_timeout, infinity)),
    ?assertEqual({ok, 1000}, erldantic_json:type_from_json(?MODULE, my_timeout, 1000)),

    %% pid
    ?assertEqual({type, pid}, maps:get({type, my_pid, 0}, Types)),
    ?assertMatch({error, [#ed_error{type = type_not_supported}]},
                 erldantic_json:type_to_json(?MODULE, my_pid, self())),
    ?assertMatch({error, [#ed_error{type = type_not_supported}]},
                 erldantic_json:type_from_json(?MODULE, my_pid, <<"not_a_pid">>)),

    %% iodata
    IoList1 = [<<"hello">>, <<"world">>],
    IoList2 = [<<"hello">> | <<"world">>],
    IoList3 = [104, <<"ello">>, [<<"wo">>, 114 | <<"l">>] | <<"d">>],

    ?assertEqual({type, iodata}, maps:get({type, my_iodata, 0}, Types)),
    ?assertEqual({ok, <<"helloworld">>},
                 erldantic_json:type_to_json(?MODULE, my_iodata, IoList1)),
    ?assertEqual({ok, <<"helloworld">>},
                 erldantic_json:type_to_json(?MODULE, my_iodata, IoList2)),
    ?assertEqual({ok, <<"helloworld">>},
                 erldantic_json:type_to_json(?MODULE, my_iodata, IoList3)),
    ?assertEqual({ok, <<"helloworld">>},
                 erldantic_json:type_to_json(?MODULE, my_iodata, <<"helloworld">>)),
    ?assertEqual({ok, <<"helloworld">>},
                 erldantic_json:type_from_json(?MODULE, my_iodata, <<"helloworld">>)),

    ?assertEqual({type, iolist}, maps:get({type, my_iolist, 0}, Types)),
    ?assertEqual({ok, <<"helloworld">>},
                 erldantic_json:type_to_json(?MODULE, my_iolist, IoList1)),
    ?assertEqual({ok, <<"helloworld">>},
                 erldantic_json:type_to_json(?MODULE, my_iolist, IoList2)),
    ?assertEqual({ok, <<"helloworld">>},
                 erldantic_json:type_to_json(?MODULE, my_iolist, IoList3)),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_json:type_to_json(?MODULE, my_iolist, <<"helloworld">>)),
    ?assertEqual({ok, <<"helloworld">>},
                 erldantic_json:type_from_json(?MODULE, my_iolist, <<"helloworld">>)),

    %% port
    ?assertEqual({type, port}, maps:get({type, my_port, 0}, Types)),
    ?assertMatch({error, [#ed_error{type = type_not_supported}]},
                 erldantic_json:type_to_json(?MODULE, my_port, not_a_port)),
    ?assertMatch({error, [#ed_error{type = type_not_supported}]},
                 erldantic_json:type_from_json(?MODULE, my_port, <<"not_a_port">>)),

    %% reference
    ?assertEqual({type, reference}, maps:get({type, my_reference, 0}, Types)),
    ?assertMatch({error, [#ed_error{type = type_not_supported}]},
                 erldantic_json:type_to_json(?MODULE, my_reference, make_ref())),
    ?assertMatch({error, [#ed_error{type = type_not_supported}]},
                 erldantic_json:type_from_json(?MODULE, my_reference, <<"not_a_reference">>)),

    %% node
    ?assertEqual({type, atom}, maps:get({type, my_node, 0}, Types)),
    ?assertEqual({ok, nonode@nohost},
                 erldantic_json:type_to_json(?MODULE, my_node, nonode@nohost)),
    ?assertEqual({ok, nonode@nohost},
                 erldantic_json:type_from_json(?MODULE, my_node, <<"nonode@nohost">>)),

    %% identifier
    ?assertEqual({union, [{type, pid}, {type, port}, {type, reference}]},
                 maps:get({type, my_identifier, 0}, Types)),
    ?assertMatch({error, [#ed_error{type = no_match}]},
                 erldantic_json:type_to_json(?MODULE, my_identifier, my_identifier)),
    ?assertMatch({error, [#ed_error{type = no_match}]},
                 erldantic_json:type_to_json(?MODULE, my_identifier, my_identifier)),

    %% literal
    ?assertEqual({literal, 1}, maps:get({type, my_literal, 0}, Types)),
    ?assertEqual({ok, 1}, erldantic_json:type_to_json(?MODULE, my_literal, 1)),
    ?assertEqual({ok, 1}, erldantic_json:type_from_json(?MODULE, my_literal, 1)),

    %% list
    ?assertEqual({list, {type, term}}, maps:get({type, my_list, 0}, Types)),
    ?assertEqual({ok, [1, 2, 3]}, erldantic_json:type_to_json(?MODULE, my_list, [1, 2, 3])),
    ?assertEqual({ok, [1, 2, 3]}, erldantic_json:type_from_json(?MODULE, my_list, [1, 2, 3])),

    %% term
    ?assertEqual({type, term}, maps:get({type, my_term, 0}, Types)),
    ?assertEqual({ok, 42}, erldantic_json:type_to_json(?MODULE, my_term, 42)),
    ?assertEqual({ok, 42}, erldantic_json:type_from_json(?MODULE, my_term, 42)),

    %% nonempty_list
    ?assertEqual({nonempty_list, {type, term}}, maps:get({type, my_nonempty_list, 0}, Types)),
    ?assertEqual({ok, [1, 2, 3]},
                 erldantic_json:type_to_json(?MODULE, my_nonempty_list, [1, 2, 3])),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_json:type_to_json(?MODULE, my_nonempty_list, [])),
    ?assertEqual({ok, [1, 2, 3]},
                 erldantic_json:type_from_json(?MODULE, my_nonempty_list, [1, 2, 3])),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_json:type_from_json(?MODULE, my_nonempty_list, [])),

    %% nil
    ?assertEqual({literal, []}, maps:get({type, my_nil, 0}, Types)),
    ?assertEqual({ok, []}, erldantic_json:type_to_json(?MODULE, my_nil, [])),
    ?assertEqual({ok, []}, erldantic_json:type_from_json(?MODULE, my_nil, [])).
