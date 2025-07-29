-module(tuple_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-compile([nowarn_unused_type]).

-type empty_tuple() :: {}.
-type tuple2() :: {integer(), atom()}.
-type tuple3() :: tuple().

-record(with_tuple, {id :: integer(), data :: tuple()}).

erl_abstract_code_parses_tuple_types_test() ->
    {ok, TypeInfo} = erldantic_abstract_code:types_in_module(?MODULE),
    ?assertEqual(#ed_tuple{fields = []}, maps:get({type, empty_tuple, 0}, TypeInfo)),
    ?assertEqual(#ed_tuple{fields =
                               [#ed_simple_type{type = integer}, #ed_simple_type{type = atom}]},
                 maps:get({type, tuple2, 0}, TypeInfo)),
    ?assertEqual(#ed_tuple{fields = any}, maps:get({type, tuple3, 0}, TypeInfo)),
    ?assertEqual(#ed_rec{name = with_tuple,
                         fields =
                             [{id, #ed_simple_type{type = integer}},
                              {data, #ed_tuple{fields = any}}],
                         arity = 3},
                 maps:get({record, with_tuple}, TypeInfo)).

erldantic_json_handles_tuple_data_test() ->
    Tuple1 = {},
    Tuple2 = {42, hello},
    Tuple3 = {a},
    ?assertError({type_not_supported, _},
                 erldantic_json:type_to_json(?MODULE, empty_tuple, Tuple1)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_to_json(?MODULE, tuple2, Tuple2)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_to_json(?MODULE, tuple3, Tuple3)).

erldantic_json_handles_tuple_data_from_json_test() ->
    Data = <<"[]">>,
    ?assertError({type_not_supported, _},
                 erldantic_json:type_from_json(?MODULE, empty_tuple, Data)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_from_json(?MODULE, tuple2, Data)),
    ?assertError({type_not_supported, _},
                 erldantic_json:type_from_json(?MODULE, tuple3, Data)).

erldantic_json_error_on_record_with_tuple_field_test() ->
    Record = #with_tuple{id = 1, data = {}},
    ?assertError({type_not_supported, _},
                 erldantic_json:record_to_json(?MODULE, with_tuple, Record)).
