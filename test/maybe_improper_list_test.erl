-module(maybe_improper_list_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-compile([nowarn_unused_type]).

-type empty_improper() :: maybe_improper_list().
-type iolist1() :: maybe_improper_list(string(), binary()).
-type iolist2() :: maybe_improper_list(string(), string()).
-type iolist3() :: maybe_improper_list(string(), maybe_improper_list(binary(), string())).
-type iolist4() :: maybe_improper_list(binary(), binary()).
-type iolist5() :: maybe_improper_list(binary(), maybe_improper_list(string(), binary())).

erl_abstract_code_parses_maybe_improper_list_types_test() ->
    {ok, TypeInfo} = erldantic_abstract_code:types_in_module(?MODULE),
    ?assertEqual(#maybe_improper_list{elements = {type, term}, tail = {type, term}},
                 maps:get({type, empty_improper, 0}, TypeInfo)),
    ?assertEqual(#maybe_improper_list{elements = {type, string}, tail = {type, binary}},
                 maps:get({type, iolist1, 0}, TypeInfo)),
    ?assertEqual(#maybe_improper_list{elements = {type, string}, tail = {type, string}},
                 maps:get({type, iolist2, 0}, TypeInfo)),
    ?assertEqual(#maybe_improper_list{elements = {type, string},
                                      tail =
                                          #maybe_improper_list{elements = {type, binary},
                                                               tail = {type, string}}},
                 maps:get({type, iolist3, 0}, TypeInfo)),
    ?assertEqual(#maybe_improper_list{elements = {type, binary}, tail = {type, binary}},
                 maps:get({type, iolist4, 0}, TypeInfo)),
    ?assertEqual(#maybe_improper_list{elements = {type, binary},
                                      tail =
                                          #maybe_improper_list{elements = {type, string},
                                                               tail = {type, binary}}},
                 maps:get({type, iolist5, 0}, TypeInfo)),
    ok.

erldantic_json_handles_maybe_improper_list_data_test() ->
    Iolist1 = ["hello", <<"world">>],
    ?assertMatch({error, [#ed_error{type = not_implemented}]},
                 erldantic_json:type_to_json(?MODULE, iolist1, Iolist1)),
    ok.

erldantic_json_handles_maybe_improper_list_data_from_json_test() ->
    Data = <<"[]">>,
    ?assertMatch({error, [#ed_error{type = not_implemented}]},
                 erldantic_json:type_from_json(?MODULE, iolist1, Data)),
    ok.
