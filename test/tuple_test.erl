-module(tuple_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic_internal.hrl").

-compile([nowarn_unused_type]).

-type empty_tuple() :: {}.
-type tuple2() :: {integer(), atom()}.
-type tuple3() :: tuple().

-record(with_tuple, {id :: integer(), data :: tuple()}).

erl_abstract_code_parses_tuple_types_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),
    {ok, EmptyTupleType} = erldantic_type_info:get_type(TypeInfo, empty_tuple, 0),
    ?assertEqual(#ed_tuple{fields = []}, EmptyTupleType),
    {ok, Tuple2Type} = erldantic_type_info:get_type(TypeInfo, tuple2, 0),
    ?assertEqual(
        #ed_tuple{
            fields =
                [#ed_simple_type{type = integer}, #ed_simple_type{type = atom}]
        },
        Tuple2Type
    ),
    {ok, Tuple3Type} = erldantic_type_info:get_type(TypeInfo, tuple3, 0),
    ?assertEqual(#ed_tuple{fields = any}, Tuple3Type),
    {ok, WithTupleRecord} = erldantic_type_info:get_record(TypeInfo, with_tuple),
    ?assertEqual(
        #ed_rec{
            name = with_tuple,
            fields =
                [
                    {id, #ed_simple_type{type = integer}},
                    {data, #ed_tuple{fields = any}}
                ],
            arity = 3
        },
        WithTupleRecord
    ).

erldantic_json_handles_tuple_data_test() ->
    Tuple1 = {},
    Tuple2 = {42, hello},
    Tuple3 = {a},
    ?assertError(
        {type_not_supported, _},
        erldantic_json:to_json(?MODULE, {type, empty_tuple, 0}, Tuple1)
    ),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:to_json(?MODULE, {type, tuple2, 0}, Tuple2)
    ),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:to_json(?MODULE, {type, tuple3, 0}, Tuple3)
    ).

erldantic_json_handles_tuple_data_from_json_test() ->
    Data = <<"[]">>,
    ?assertError(
        {type_not_supported, _},
        erldantic_json:from_json(?MODULE, {type, empty_tuple, 0}, Data)
    ),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:from_json(?MODULE, {type, tuple2, 0}, Data)
    ),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:from_json(?MODULE, {type, tuple3, 0}, Data)
    ).

erldantic_json_error_on_record_with_tuple_field_test() ->
    Record = #with_tuple{id = 1, data = {}},
    ?assertError(
        {type_not_supported, _},
        erldantic_json:to_json(?MODULE, {record, with_tuple}, Record)
    ).
