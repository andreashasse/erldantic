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
-type non_empty_iolist1() :: nonempty_improper_list(string(), binary()).

erl_abstract_code_parses_maybe_improper_list_types_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),
    ?assertEqual(#ed_maybe_improper_list{elements = #ed_simple_type{type = term},
                                         tail = #ed_simple_type{type = term}},
                 maps:get({type, empty_improper, 0}, TypeInfo)),
    ?assertEqual(#ed_maybe_improper_list{elements = #ed_simple_type{type = string},
                                         tail = #ed_simple_type{type = binary}},
                 maps:get({type, iolist1, 0}, TypeInfo)),
    ?assertEqual(#ed_maybe_improper_list{elements = #ed_simple_type{type = string},
                                         tail = #ed_simple_type{type = string}},
                 maps:get({type, iolist2, 0}, TypeInfo)),
    ?assertEqual(#ed_maybe_improper_list{elements = #ed_simple_type{type = string},
                                         tail =
                                             #ed_maybe_improper_list{elements =
                                                                         #ed_simple_type{type =
                                                                                             binary},
                                                                     tail =
                                                                         #ed_simple_type{type =
                                                                                             string}}},
                 maps:get({type, iolist3, 0}, TypeInfo)),
    ?assertEqual(#ed_maybe_improper_list{elements = #ed_simple_type{type = binary},
                                         tail = #ed_simple_type{type = binary}},
                 maps:get({type, iolist4, 0}, TypeInfo)),
    ?assertEqual(#ed_maybe_improper_list{elements = #ed_simple_type{type = binary},
                                         tail =
                                             #ed_maybe_improper_list{elements =
                                                                         #ed_simple_type{type =
                                                                                             string},
                                                                     tail =
                                                                         #ed_simple_type{type =
                                                                                             binary}}},
                 maps:get({type, iolist5, 0}, TypeInfo)),
    ?assertEqual(#ed_nonempty_improper_list{elements = #ed_simple_type{type = string},
                                            tail = #ed_simple_type{type = binary}},
                 maps:get({type, non_empty_iolist1, 0}, TypeInfo)),
    ok.

erldantic_json_handles_maybe_improper_list_data_test() ->
    Iolist1 = ["hello", <<"world">>],
    ?assertError({type_not_implemented, _},
                 erldantic_json:type_to_json(?MODULE, iolist1, Iolist1)),
    ?assertError({type_not_implemented, _},
                 erldantic_json:type_to_json(?MODULE, non_empty_iolist1, Iolist1)),
    ok.

erldantic_json_handles_maybe_improper_list_data_from_json_test() ->
    Data = <<"[]">>,
    ?assertError({type_not_implemented, _},
                 erldantic_json:type_from_json(?MODULE, iolist1, Data)),
    ?assertError({type_not_implemented, _},
                 erldantic_json:type_from_json(?MODULE, non_empty_iolist1, Data)),
    ok.
