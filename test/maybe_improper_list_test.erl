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
    {ok, EmptyImproperType} = erldantic_type_info:get_type(TypeInfo, empty_improper, 0),
    ?assertEqual(#ed_maybe_improper_list{elements = #ed_simple_type{type = term},
                                         tail = #ed_simple_type{type = term}},
                 EmptyImproperType),
    {ok, Iolist1Type} = erldantic_type_info:get_type(TypeInfo, iolist1, 0),
    ?assertEqual(#ed_maybe_improper_list{elements = #ed_simple_type{type = string},
                                         tail = #ed_simple_type{type = binary}},
                 Iolist1Type),
    {ok, Iolist2Type} = erldantic_type_info:get_type(TypeInfo, iolist2, 0),
    ?assertEqual(#ed_maybe_improper_list{elements = #ed_simple_type{type = string},
                                         tail = #ed_simple_type{type = string}},
                 Iolist2Type),
    {ok, Iolist3Type} = erldantic_type_info:get_type(TypeInfo, iolist3, 0),
    ?assertEqual(#ed_maybe_improper_list{elements = #ed_simple_type{type = string},
                                         tail =
                                             #ed_maybe_improper_list{elements =
                                                                         #ed_simple_type{type =
                                                                                             binary},
                                                                     tail =
                                                                         #ed_simple_type{type =
                                                                                             string}}},
                 Iolist3Type),
    {ok, Iolist4Type} = erldantic_type_info:get_type(TypeInfo, iolist4, 0),
    ?assertEqual(#ed_maybe_improper_list{elements = #ed_simple_type{type = binary},
                                         tail = #ed_simple_type{type = binary}},
                 Iolist4Type),
    {ok, Iolist5Type} = erldantic_type_info:get_type(TypeInfo, iolist5, 0),
    ?assertEqual(#ed_maybe_improper_list{elements = #ed_simple_type{type = binary},
                                         tail =
                                             #ed_maybe_improper_list{elements =
                                                                         #ed_simple_type{type =
                                                                                             string},
                                                                     tail =
                                                                         #ed_simple_type{type =
                                                                                             binary}}},
                 Iolist5Type),
    {ok, NonEmptyIolist1Type} = erldantic_type_info:get_type(TypeInfo, non_empty_iolist1, 0),
    ?assertEqual(#ed_nonempty_improper_list{elements = #ed_simple_type{type = string},
                                            tail = #ed_simple_type{type = binary}},
                 NonEmptyIolist1Type),
    ok.

erldantic_json_handles_maybe_improper_list_data_test() ->
    Iolist1 = ["hello", <<"world">>],
    ?assertError({type_not_implemented, _},
                 erldantic_json:to_json(?MODULE, {type, iolist1, 0}, Iolist1)),
    ?assertError({type_not_implemented, _},
                 erldantic_json:to_json(?MODULE, {type, non_empty_iolist1, 0}, Iolist1)),
    ok.

erldantic_json_handles_maybe_improper_list_data_from_json_test() ->
    Data = <<"[]">>,
    ?assertError({type_not_implemented, _},
                 erldantic_json:from_json(?MODULE, {type, iolist1, 0}, Data)),
    ?assertError({type_not_implemented, _},
                 erldantic_json:from_json(?MODULE, {type, non_empty_iolist1, 0}, Data)),
    ok.
