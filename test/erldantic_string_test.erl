-module(erldantic_string_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-compile(nowarn_unused_type).

%% Test types
-type my_integer() :: integer().
-type my_float() :: float().
-type my_number() :: number().
-type my_boolean() :: boolean().
-type my_atom() :: atom().
-type my_string() :: string().
-type my_nonempty_string() :: nonempty_string().
-type my_binary() :: binary().
-type my_nonempty_binary() :: nonempty_binary().
-type my_non_neg_integer() :: non_neg_integer().
-type my_pos_integer() :: pos_integer().
-type my_neg_integer() :: neg_integer().
-type my_range() :: 1..10.
-type my_literal_atom() :: hello.
-type my_literal_integer() :: 42.
-type my_literal_boolean() :: true.
-type my_union() :: integer() | boolean().
-type my_complex_union() :: 1 | 2 | true | false.

%% Test ed_simple_type conversions
simple_types_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),

    %% integer
    ?assertEqual({ok, 42},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = integer}, "42")),
    ?assertEqual({ok, -42},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = integer}, "-42")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = integer},
                                              "not_a_number")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = integer}, "3.14")),

    %% float
    ?assertEqual({ok, 3.14},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = float}, "3.14")),
    ?assertEqual({ok, -3.14},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = float}, "-3.14")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = float},
                                              "not_a_number")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = float}, "42")),

    %% number (tries integer first, then float)
    ?assertEqual({ok, 42},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = number}, "42")),
    ?assertEqual({ok, 3.14},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = number}, "3.14")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = number},
                                              "not_a_number")),

    %% boolean
    ?assertEqual({ok, true},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = boolean}, "true")),
    ?assertEqual({ok, false},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = boolean}, "false")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = boolean}, "True")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = boolean}, "1")),

    %% atom
    ?assertEqual({ok, hello},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = atom}, "hello")),
    ?assertEqual({ok, 'hello world'},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = atom},
                                              "hello world")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = atom},
                                              "non_existing_atom_123456789")),

    %% string
    ?assertEqual({ok, "hello"},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = string}, "hello")),
    ?assertEqual({ok, ""},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = string}, "")),
    ?assertEqual({ok, "hello world 123!"},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = string},
                                              "hello world 123!")),

    %% nonempty_string
    ?assertEqual({ok, "hello"},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = nonempty_string},
                                              "hello")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = nonempty_string},
                                              "")),

    %% binary
    ?assertEqual({ok, <<"hello">>},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = binary}, "hello")),
    ?assertEqual({ok, <<"">>},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = binary}, "")),

    %% nonempty_binary
    ?assertEqual({ok, <<"hello">>},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = nonempty_binary},
                                              "hello")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = nonempty_binary},
                                              "")),

    %% non_neg_integer
    ?assertEqual({ok, 42},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = non_neg_integer},
                                              "42")),
    ?assertEqual({ok, 0},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = non_neg_integer},
                                              "0")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = non_neg_integer},
                                              "-1")),

    %% pos_integer
    ?assertEqual({ok, 42},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = pos_integer}, "42")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = pos_integer}, "0")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = pos_integer}, "-1")),

    %% neg_integer
    ?assertEqual({ok, -42},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = neg_integer},
                                              "-42")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = neg_integer}, "0")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = neg_integer}, "42")),

    ok.

%% Test ed_range conversions
range_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),
    Range =
        #ed_range{type = integer,
                  lower_bound = 1,
                  upper_bound = 10},

    %% Valid values in range
    ?assertEqual({ok, 1}, erldantic_string:from_string(TypeInfo, Range, "1")),
    ?assertEqual({ok, 5}, erldantic_string:from_string(TypeInfo, Range, "5")),
    ?assertEqual({ok, 10}, erldantic_string:from_string(TypeInfo, Range, "10")),

    %% Invalid values outside range
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, Range, "0")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, Range, "11")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, Range, "-5")),

    %% Invalid non-integer strings
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, Range, "not_a_number")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, Range, "5.5")),

    ok.

%% Test ed_literal conversions
literal_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),

    %% Literal atom
    AtomLiteral = #ed_literal{value = hello},
    ?assertEqual({ok, hello}, erldantic_string:from_string(TypeInfo, AtomLiteral, "hello")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, AtomLiteral, "world")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo,
                                              AtomLiteral,
                                              "non_existing_atom_123456789")),

    %% Literal integer
    IntegerLiteral = #ed_literal{value = 42},
    ?assertEqual({ok, 42}, erldantic_string:from_string(TypeInfo, IntegerLiteral, "42")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, IntegerLiteral, "43")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, IntegerLiteral, "not_a_number")),

    %% Literal boolean
    BooleanLiteral = #ed_literal{value = true},
    ?assertEqual({ok, true}, erldantic_string:from_string(TypeInfo, BooleanLiteral, "true")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, BooleanLiteral, "false")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, BooleanLiteral, "True")),

    ok.

%% Test ed_union conversions
union_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),

    %% Simple union: integer | boolean
    Union =
        #ed_union{types = [#ed_simple_type{type = integer}, #ed_simple_type{type = boolean}]},
    ?assertEqual({ok, 42}, erldantic_string:from_string(TypeInfo, Union, "42")),
    ?assertEqual({ok, true}, erldantic_string:from_string(TypeInfo, Union, "true")),
    ?assertEqual({ok, false}, erldantic_string:from_string(TypeInfo, Union, "false")),
    ?assertMatch({error, [#ed_error{type = no_match}]},
                 erldantic_string:from_string(TypeInfo, Union, "not_matching")),

    %% Complex union with literals: 1 | 2 | true | false
    ComplexUnion =
        #ed_union{types =
                      [#ed_literal{value = 1},
                       #ed_literal{value = 2},
                       #ed_literal{value = true},
                       #ed_literal{value = false}]},
    ?assertEqual({ok, 1}, erldantic_string:from_string(TypeInfo, ComplexUnion, "1")),
    ?assertEqual({ok, 2}, erldantic_string:from_string(TypeInfo, ComplexUnion, "2")),
    ?assertEqual({ok, true}, erldantic_string:from_string(TypeInfo, ComplexUnion, "true")),
    ?assertEqual({ok, false}, erldantic_string:from_string(TypeInfo, ComplexUnion, "false")),
    ?assertMatch({error, [#ed_error{type = no_match}]},
                 erldantic_string:from_string(TypeInfo, ComplexUnion, "3")),
    ?assertMatch({error, [#ed_error{type = no_match}]},
                 erldantic_string:from_string(TypeInfo, ComplexUnion, "maybe")),

    ok.

%% Test with type references
type_reference_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),

    %% Test various type references from the module
    ?assertEqual({ok, 42},
                 erldantic_string:from_string(TypeInfo, {type, my_integer, 0}, "42")),
    ?assertEqual({ok, 3.14},
                 erldantic_string:from_string(TypeInfo, {type, my_float, 0}, "3.14")),
    ?assertEqual({ok, 42},
                 erldantic_string:from_string(TypeInfo, {type, my_number, 0}, "42")),
    ?assertEqual({ok, 3.14},
                 erldantic_string:from_string(TypeInfo, {type, my_number, 0}, "3.14")),
    ?assertEqual({ok, true},
                 erldantic_string:from_string(TypeInfo, {type, my_boolean, 0}, "true")),
    ?assertEqual({ok, hello},
                 erldantic_string:from_string(TypeInfo, {type, my_atom, 0}, "hello")),
    ?assertEqual({ok, "test"},
                 erldantic_string:from_string(TypeInfo, {type, my_string, 0}, "test")),
    ?assertEqual({ok, <<"test">>},
                 erldantic_string:from_string(TypeInfo, {type, my_binary, 0}, "test")),

    %% Test range type
    ?assertEqual({ok, 5}, erldantic_string:from_string(TypeInfo, {type, my_range, 0}, "5")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, {type, my_range, 0}, "15")),

    %% Test literal types
    ?assertEqual({ok, hello},
                 erldantic_string:from_string(TypeInfo, {type, my_literal_atom, 0}, "hello")),
    ?assertEqual({ok, 42},
                 erldantic_string:from_string(TypeInfo, {type, my_literal_integer, 0}, "42")),
    ?assertEqual({ok, true},
                 erldantic_string:from_string(TypeInfo, {type, my_literal_boolean, 0}, "true")),

    %% Test union types
    ?assertEqual({ok, 42}, erldantic_string:from_string(TypeInfo, {type, my_union, 0}, "42")),
    ?assertEqual({ok, true},
                 erldantic_string:from_string(TypeInfo, {type, my_union, 0}, "true")),
    ?assertEqual({ok, 1},
                 erldantic_string:from_string(TypeInfo, {type, my_complex_union, 0}, "1")),
    ?assertEqual({ok, false},
                 erldantic_string:from_string(TypeInfo, {type, my_complex_union, 0}, "false")),

    ok.

%% Test unsupported types and operations
unsupported_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),

    %% Record types are not supported for string conversion
    ?assertMatch({error, [#ed_error{type = no_match}]},
                 erldantic_string:from_string(TypeInfo, {record, some_record}, "test")),

    %% Unsupported simple types should error
    ?assertError({type_not_supported, _},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = pid}, "test")),
    ?assertError({type_not_supported, _},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = port}, "test")),
    ?assertError({type_not_supported, _},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = reference}, "test")),
    ?assertError({type_not_supported, _},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = bitstring}, "test")),
    ?assertError({type_not_supported, _},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = nonempty_bitstring},
                                              "test")),
    ?assertError({type_not_supported, _},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = none}, "test")),

    %% Unknown type should give type mismatch error
    UnknownType = #ed_tuple{fields = any},
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, UnknownType, "test")),

    ok.

%% Test edge cases
edge_cases_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),

    %% Empty strings
    ?assertEqual({ok, ""},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = string}, "")),
    ?assertEqual({ok, <<"">>},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = binary}, "")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = nonempty_string},
                                              "")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = nonempty_binary},
                                              "")),

    %% Large numbers
    ?assertEqual({ok, 999999999999},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = integer},
                                              "999999999999")),
    ?assertEqual({ok, -999999999999},
                 erldantic_string:from_string(TypeInfo,
                                              #ed_simple_type{type = integer},
                                              "-999999999999")),

    %% Special float values
    ?assertEqual({ok, 0.0},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = float}, "0.0")),
    ?assertEqual({ok, -0.0},
                 erldantic_string:from_string(TypeInfo, #ed_simple_type{type = float}, "-0.0")),

    %% Boundary values for ranges
    Range =
        #ed_range{type = integer,
                  lower_bound = -5,
                  upper_bound = 5},
    ?assertEqual({ok, -5}, erldantic_string:from_string(TypeInfo, Range, "-5")),
    ?assertEqual({ok, 5}, erldantic_string:from_string(TypeInfo, Range, "5")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, Range, "-6")),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 erldantic_string:from_string(TypeInfo, Range, "6")),

    ok.
