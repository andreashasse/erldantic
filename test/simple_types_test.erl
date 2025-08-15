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
-type my_dynamic() :: dynamic().
-type my_nonempty_binary() :: nonempty_binary().
-type my_bitstring() :: bitstring().
-type my_nonempty_bitstring() :: nonempty_bitstring().
-type my_no_return() :: no_return().
-type my_none() :: none().
-type my_range() :: -2..2.

missing_test() ->
    Types = erldantic_abstract_code:types_in_module(?MODULE),

    %% range
    %% FIXME: more specific matching
    ?assertMatch({error, _}, erldantic_json:type_to_json(?MODULE, my_range, -0.0)),

    %% arity
    ?assertEqual(#ed_range{type = integer,
                           lower_bound = 0,
                           upper_bound = 255},
                 maps:get({type, my_arity, 0}, Types)),
    ?assertEqual({ok, 42}, erldantic_json:type_to_json(?MODULE, my_arity, 42)),
    ?assertEqual({ok, 42}, erldantic_json:type_from_json(?MODULE, my_arity, 42)),

    %% byte
    ?assertEqual(#ed_range{type = integer,
                           lower_bound = 0,
                           upper_bound = 255},
                 maps:get({type, my_byte, 0}, Types)),
    ?assertEqual({ok, 42}, erldantic_json:type_to_json(?MODULE, my_byte, 42)),
    ?assertEqual({ok, 42}, erldantic_json:type_from_json(?MODULE, my_byte, 42)),

    %% char
    ?assertEqual(#ed_range{type = integer,
                           lower_bound = 0,
                           upper_bound = 1114111},
                 maps:get({type, my_char, 0}, Types)),
    ?assertEqual({ok, 42}, erldantic_json:type_to_json(?MODULE, my_char, 42)),
    ?assertEqual({ok, 42}, erldantic_json:type_from_json(?MODULE, my_char, 42)),

    %% mfa
    ?assertEqual(#ed_tuple{fields =
                               [#ed_simple_type{type = atom},
                                #ed_simple_type{type = atom},
                                #ed_range{type = integer,
                                          lower_bound = 0,
                                          upper_bound = 255}]},
                 maps:get({type, my_mfa, 0}, Types)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_to_json(?MODULE, my_mfa, {module, function, 42})),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_from_json(?MODULE, my_mfa, {module, function, 42})),

    %% any
    ?assertEqual(#ed_simple_type{type = term}, maps:get({type, my_term, 0}, Types)),
    ?assertEqual({ok, 42}, erldantic_json:type_to_json(?MODULE, my_term, 42)),
    ?assertEqual({ok, 42}, erldantic_json:type_from_json(?MODULE, my_term, 42)),

    %% timeout
    ?assertEqual(#ed_union{types =
                               [#ed_simple_type{type = non_neg_integer},
                                #ed_literal{value = infinity}]},
                 maps:get({type, my_timeout, 0}, Types)),
    ?assertMatch({error, [#ed_error{type = no_match}]},
                 erldantic_json:type_to_json(?MODULE, my_timeout, <<"infinity">>)),
    ?assertEqual({ok, infinity}, erldantic_json:type_to_json(?MODULE, my_timeout, infinity)),
    ?assertEqual({ok, 42}, erldantic_json:type_to_json(?MODULE, my_timeout, 42)),
    ?assertEqual({ok, infinity},
                 erldantic_json:type_from_json(?MODULE, my_timeout, infinity)),
    ?assertEqual({ok, 1000}, erldantic_json:type_from_json(?MODULE, my_timeout, 1000)),

    %% pid
    ?assertEqual(#ed_simple_type{type = pid}, maps:get({type, my_pid, 0}, Types)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_to_json(?MODULE, my_pid, self())),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_from_json(?MODULE, my_pid, <<"not_a_pid">>)),

    %% iodata
    IoList1 = [<<"hello">>, <<"world">>],
    IoList2 = [<<"hello">> | <<"world">>],
    IoList3 = [104, <<"ello">>, [<<"wo">>, 114 | <<"l">>] | <<"d">>],

    ?assertEqual(#ed_simple_type{type = iodata}, maps:get({type, my_iodata, 0}, Types)),
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

    ?assertEqual(#ed_simple_type{type = iolist}, maps:get({type, my_iolist, 0}, Types)),
    ?assertEqual({ok, <<"helloworld">>},
                 erldantic_json:type_to_json(?MODULE, my_iolist, IoList1)),
    ?assertEqual({ok, <<"helloworld">>},
                 erldantic_json:type_to_json(?MODULE, my_iolist, IoList2)),
    ?assertEqual({ok, <<"helloworld">>},
                 erldantic_json:type_to_json(?MODULE, my_iolist, IoList3)),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_json:type_to_json(?MODULE, my_iolist, <<"helloworld">>)),
    ?assertEqual({ok, [<<"helloworld">>]},
                 erldantic_json:type_from_json(?MODULE, my_iolist, <<"helloworld">>)),

    %% port
    ?assertEqual(#ed_simple_type{type = port}, maps:get({type, my_port, 0}, Types)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_to_json(?MODULE, my_port, not_a_port)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_from_json(?MODULE, my_port, <<"not_a_port">>)),

    %% reference
    ?assertEqual(#ed_simple_type{type = reference}, maps:get({type, my_reference, 0}, Types)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_to_json(?MODULE, my_reference, make_ref())),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_from_json(?MODULE, my_reference, <<"not_a_reference">>)),

    %% node
    ?assertEqual(#ed_simple_type{type = atom}, maps:get({type, my_node, 0}, Types)),
    ?assertEqual({ok, nonode@nohost},
                 erldantic_json:type_to_json(?MODULE, my_node, nonode@nohost)),
    ?assertEqual({ok, nonode@nohost},
                 erldantic_json:type_from_json(?MODULE, my_node, <<"nonode@nohost">>)),

    %% identifier
    ?assertEqual(#ed_union{types =
                               [#ed_simple_type{type = pid},
                                #ed_simple_type{type = port},
                                #ed_simple_type{type = reference}]},
                 maps:get({type, my_identifier, 0}, Types)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_to_json(?MODULE, my_identifier, my_identifier)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_to_json(?MODULE, my_identifier, my_identifier)),

    %% literal
    ?assertEqual(#ed_literal{value = 1}, maps:get({type, my_literal, 0}, Types)),
    ?assertEqual({ok, 1}, erldantic_json:type_to_json(?MODULE, my_literal, 1)),
    ?assertEqual({ok, 1}, erldantic_json:type_from_json(?MODULE, my_literal, 1)),

    %% list
    ?assertEqual(#ed_list{type = #ed_simple_type{type = term}},
                 maps:get({type, my_list, 0}, Types)),
    ?assertEqual({ok, [1, 2, 3]}, erldantic_json:type_to_json(?MODULE, my_list, [1, 2, 3])),
    ?assertEqual({ok, [1, 2, 3]}, erldantic_json:type_from_json(?MODULE, my_list, [1, 2, 3])),

    %% term
    ?assertEqual(#ed_simple_type{type = term}, maps:get({type, my_term, 0}, Types)),
    ?assertEqual({ok, 42}, erldantic_json:type_to_json(?MODULE, my_term, 42)),
    ?assertEqual({ok, 42}, erldantic_json:type_from_json(?MODULE, my_term, 42)),

    %% nonempty_list
    ?assertEqual(#ed_nonempty_list{type = #ed_simple_type{type = term}},
                 maps:get({type, my_nonempty_list, 0}, Types)),
    ?assertEqual({ok, [1, 2, 3]},
                 erldantic_json:type_to_json(?MODULE, my_nonempty_list, [1, 2, 3])),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_json:type_to_json(?MODULE, my_nonempty_list, [])),
    ?assertEqual({ok, [1, 2, 3]},
                 erldantic_json:type_from_json(?MODULE, my_nonempty_list, [1, 2, 3])),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_json:type_from_json(?MODULE, my_nonempty_list, [])),

    %% nil
    ?assertEqual(#ed_literal{value = []}, maps:get({type, my_nil, 0}, Types)),
    ?assertEqual({ok, []}, erldantic_json:type_to_json(?MODULE, my_nil, [])),
    ?assertEqual({ok, []}, erldantic_json:type_from_json(?MODULE, my_nil, [])),

    %% dynamic
    ?assertEqual(#ed_simple_type{type = term}, maps:get({type, my_dynamic, 0}, Types)),
    ?assertEqual({ok, 42}, erldantic_json:type_to_json(?MODULE, my_dynamic, 42)),
    ?assertEqual({ok, 42}, erldantic_json:type_from_json(?MODULE, my_dynamic, 42)),

    %% nonempty_binary
    ?assertEqual(#ed_simple_type{type = nonempty_binary},
                 maps:get({type, my_nonempty_binary, 0}, Types)),
    ?assertEqual({ok, <<"hello">>},
                 erldantic_json:type_to_json(?MODULE, my_nonempty_binary, <<"hello">>)),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_json:type_to_json(?MODULE, my_nonempty_binary, <<>>)),
    ?assertEqual({ok, <<"hello">>},
                 erldantic_json:type_from_json(?MODULE, my_nonempty_binary, <<"hello">>)),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_json:type_from_json(?MODULE, my_nonempty_binary, <<>>)),

    %% bitstring
    ?assertEqual(#ed_simple_type{type = bitstring}, maps:get({type, my_bitstring, 0}, Types)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_to_json(?MODULE, my_bitstring, <<1, 2, 3>>)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_from_json(?MODULE, my_bitstring, <<1, 2, 3>>)),

    %% nonempty_bitstring
    ?assertEqual(#ed_simple_type{type = nonempty_bitstring},
                 maps:get({type, my_nonempty_bitstring, 0}, Types)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_to_json(?MODULE, my_nonempty_bitstring, <<1, 2, 3>>)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_from_json(?MODULE, my_nonempty_bitstring, <<1, 2, 3>>)),

    %% no_return
    ?assertEqual(#ed_simple_type{type = none}, maps:get({type, my_no_return, 0}, Types)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_to_json(?MODULE, my_no_return, a)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_from_json(?MODULE, my_no_return, <<"not_a_no_return">>)),

    %% none
    ?assertEqual(#ed_simple_type{type = none}, maps:get({type, my_none, 0}, Types)),
    ?assertError({type_not_supported, _}, erldantic_json:type_to_json(?MODULE, my_none, a)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_from_json(?MODULE, my_none, <<"not_a_none">>)).
