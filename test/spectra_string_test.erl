-module(spectra_string_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

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
-type my_var_integer() :: T :: integer().

%% Test sp_simple_type conversions
simple_types_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% integer
    ?assertEqual(
        {ok, 42},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = integer}, "42")
    ),
    ?assertEqual(
        {ok, -42},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = integer}, "-42")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = integer},
            "not_a_number"
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = integer}, "3.14")
    ),

    %% float
    ?assertEqual(
        {ok, 3.14},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = float}, "3.14")
    ),
    ?assertEqual(
        {ok, -3.14},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = float}, "-3.14")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = float},
            "not_a_number"
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = float}, "42")
    ),

    %% number (tries integer first, then float)
    ?assertEqual(
        {ok, 42},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = number}, "42")
    ),
    ?assertEqual(
        {ok, 3.14},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = number}, "3.14")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = number},
            "not_a_number"
        )
    ),

    %% boolean
    ?assertEqual(
        {ok, true},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = boolean}, "true")
    ),
    ?assertEqual(
        {ok, false},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = boolean}, "false")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = boolean}, "True")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = boolean}, "1")
    ),

    %% atom
    ?assertEqual(
        {ok, hello},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = atom}, "hello")
    ),
    ?assertEqual(
        {ok, 'hello world'},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = atom},
            "hello world"
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = atom},
            "non_existing_atom_123456789"
        )
    ),

    %% string
    ?assertEqual(
        {ok, "hello"},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = string}, "hello")
    ),
    ?assertEqual(
        {ok, ""},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = string}, "")
    ),
    ?assertEqual(
        {ok, "hello world 123!"},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = string},
            "hello world 123!"
        )
    ),

    %% nonempty_string
    ?assertEqual(
        {ok, "hello"},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_string},
            "hello"
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_string},
            ""
        )
    ),

    %% binary
    ?assertEqual(
        {ok, <<"hello">>},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = binary}, "hello")
    ),
    ?assertEqual(
        {ok, <<"">>},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = binary}, "")
    ),

    %% nonempty_binary
    ?assertEqual(
        {ok, <<"hello">>},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_binary},
            "hello"
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_binary},
            ""
        )
    ),

    %% non_neg_integer
    ?assertEqual(
        {ok, 42},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = non_neg_integer},
            "42"
        )
    ),
    ?assertEqual(
        {ok, 0},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = non_neg_integer},
            "0"
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = non_neg_integer},
            "-1"
        )
    ),

    %% pos_integer
    ?assertEqual(
        {ok, 42},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = pos_integer}, "42")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = pos_integer}, "0")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = pos_integer}, "-1")
    ),

    %% neg_integer
    ?assertEqual(
        {ok, -42},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = neg_integer},
            "-42"
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = neg_integer}, "0")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = neg_integer}, "42")
    ),

    ok.

%% Test sp_range conversions
range_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    Range =
        #sp_range{
            type = integer,
            lower_bound = 1,
            upper_bound = 10
        },

    %% Valid values in range
    ?assertEqual({ok, 1}, spectra_string:from_string(TypeInfo, Range, "1")),
    ?assertEqual({ok, 5}, spectra_string:from_string(TypeInfo, Range, "5")),
    ?assertEqual({ok, 10}, spectra_string:from_string(TypeInfo, Range, "10")),

    %% Invalid values outside range
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, Range, "0")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, Range, "11")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, Range, "-5")
    ),

    %% Invalid non-integer strings
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, Range, "not_a_number")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, Range, "5.5")
    ),

    ok.

%% Test sp_literal conversions
literal_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Literal atom
    AtomLiteral = #sp_literal{value = hello},
    ?assertEqual({ok, hello}, spectra_string:from_string(TypeInfo, AtomLiteral, "hello")),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, AtomLiteral, "world")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(
            TypeInfo,
            AtomLiteral,
            "non_existing_atom_123456789"
        )
    ),

    %% Literal integer
    IntegerLiteral = #sp_literal{value = 42},
    ?assertEqual({ok, 42}, spectra_string:from_string(TypeInfo, IntegerLiteral, "42")),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, IntegerLiteral, "43")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, IntegerLiteral, "not_a_number")
    ),

    %% Literal boolean
    BooleanLiteral = #sp_literal{value = true},
    ?assertEqual({ok, true}, spectra_string:from_string(TypeInfo, BooleanLiteral, "true")),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, BooleanLiteral, "false")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, BooleanLiteral, "True")
    ),

    ok.

%% Test sp_union conversions
union_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Simple union: integer | boolean
    Union =
        #sp_union{types = [#sp_simple_type{type = integer}, #sp_simple_type{type = boolean}]},
    ?assertEqual({ok, 42}, spectra_string:from_string(TypeInfo, Union, "42")),
    ?assertEqual({ok, true}, spectra_string:from_string(TypeInfo, Union, "true")),
    ?assertEqual({ok, false}, spectra_string:from_string(TypeInfo, Union, "false")),
    ?assertMatch(
        {error, [#sp_error{type = no_match}]},
        spectra_string:from_string(TypeInfo, Union, "not_matching")
    ),

    %% Complex union with literals: 1 | 2 | true | false
    ComplexUnion =
        #sp_union{
            types =
                [
                    #sp_literal{value = 1},
                    #sp_literal{value = 2},
                    #sp_literal{value = true},
                    #sp_literal{value = false}
                ]
        },
    ?assertEqual({ok, 1}, spectra_string:from_string(TypeInfo, ComplexUnion, "1")),
    ?assertEqual({ok, 2}, spectra_string:from_string(TypeInfo, ComplexUnion, "2")),
    ?assertEqual({ok, true}, spectra_string:from_string(TypeInfo, ComplexUnion, "true")),
    ?assertEqual({ok, false}, spectra_string:from_string(TypeInfo, ComplexUnion, "false")),
    ?assertMatch(
        {error, [#sp_error{type = no_match}]},
        spectra_string:from_string(TypeInfo, ComplexUnion, "3")
    ),
    ?assertMatch(
        {error, [#sp_error{type = no_match}]},
        spectra_string:from_string(TypeInfo, ComplexUnion, "maybe")
    ),

    ok.

%% Test with type references
type_reference_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Test various type references from the module
    ?assertEqual(
        {ok, 42},
        spectra_string:from_string(TypeInfo, {type, my_integer, 0}, "42")
    ),
    ?assertEqual(
        {ok, 3.14},
        spectra_string:from_string(TypeInfo, {type, my_float, 0}, "3.14")
    ),
    ?assertEqual(
        {ok, 42},
        spectra_string:from_string(TypeInfo, {type, my_number, 0}, "42")
    ),
    ?assertEqual(
        {ok, 3.14},
        spectra_string:from_string(TypeInfo, {type, my_number, 0}, "3.14")
    ),
    ?assertEqual(
        {ok, true},
        spectra_string:from_string(TypeInfo, {type, my_boolean, 0}, "true")
    ),
    ?assertEqual(
        {ok, hello},
        spectra_string:from_string(TypeInfo, {type, my_atom, 0}, "hello")
    ),
    ?assertEqual(
        {ok, "test"},
        spectra_string:from_string(TypeInfo, {type, my_string, 0}, "test")
    ),
    ?assertEqual(
        {ok, <<"test">>},
        spectra_string:from_string(TypeInfo, {type, my_binary, 0}, "test")
    ),

    %% Test range type
    ?assertEqual({ok, 5}, spectra_string:from_string(TypeInfo, {type, my_range, 0}, "5")),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, {type, my_range, 0}, "15")
    ),

    %% Test literal types
    ?assertEqual(
        {ok, hello},
        spectra_string:from_string(TypeInfo, {type, my_literal_atom, 0}, "hello")
    ),
    ?assertEqual(
        {ok, 42},
        spectra_string:from_string(TypeInfo, {type, my_literal_integer, 0}, "42")
    ),
    ?assertEqual(
        {ok, true},
        spectra_string:from_string(TypeInfo, {type, my_literal_boolean, 0}, "true")
    ),

    %% Test union types
    ?assertEqual({ok, 42}, spectra_string:from_string(TypeInfo, {type, my_union, 0}, "42")),
    ?assertEqual(
        {ok, true},
        spectra_string:from_string(TypeInfo, {type, my_union, 0}, "true")
    ),
    ?assertEqual(
        {ok, 1},
        spectra_string:from_string(TypeInfo, {type, my_complex_union, 0}, "1")
    ),
    ?assertEqual(
        {ok, false},
        spectra_string:from_string(TypeInfo, {type, my_complex_union, 0}, "false")
    ),

    ok.

%% Test unsupported types and operations
unsupported_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Record types are not supported for string conversion
    ?assertMatch(
        {error, [#sp_error{type = no_match}]},
        spectra_string:from_string(TypeInfo, {record, some_record}, "test")
    ),

    %% Unsupported simple types should error
    ?assertError(
        {type_not_supported, _},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = pid}, "test")
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = port}, "test")
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = reference}, "test")
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = bitstring}, "test")
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_bitstring},
            "test"
        )
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = none}, "test")
    ),

    %% Unknown type should give type mismatch error
    UnknownType = #sp_tuple{fields = any},
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, UnknownType, "test")
    ),

    ok.

%% Test edge cases
edge_cases_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Empty strings
    ?assertEqual(
        {ok, ""},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = string}, "")
    ),
    ?assertEqual(
        {ok, <<"">>},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = binary}, "")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_string},
            ""
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_binary},
            ""
        )
    ),

    %% Large numbers
    ?assertEqual(
        {ok, 999999999999},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = integer},
            "999999999999"
        )
    ),
    ?assertEqual(
        {ok, -999999999999},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = integer},
            "-999999999999"
        )
    ),

    %% Special float values
    ?assertEqual(
        {ok, 0.0},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = float}, "0.0")
    ),
    ?assertEqual(
        {ok, -0.0},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = float}, "-0.0")
    ),

    %% Boundary values for ranges
    Range =
        #sp_range{
            type = integer,
            lower_bound = -5,
            upper_bound = 5
        },
    ?assertEqual({ok, -5}, spectra_string:from_string(TypeInfo, Range, "-5")),
    ?assertEqual({ok, 5}, spectra_string:from_string(TypeInfo, Range, "5")),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, Range, "-6")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, Range, "6")
    ),

    ok.

%% Test to_string/3 function - Simple types
to_string_simple_types_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% integer
    ?assertEqual(
        {ok, "42"},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = integer}, 42)
    ),
    ?assertEqual(
        {ok, "-42"},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = integer}, -42)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(
            TypeInfo,
            #sp_simple_type{type = integer},
            "not_integer"
        )
    ),

    %% float
    ?assertEqual(
        {ok, "3.14000000000000012434e+00"},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = float}, 3.14)
    ),
    ?assertEqual(
        {ok, "-3.14000000000000012434e+00"},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = float}, -3.14)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = float}, 42)
    ),

    %% number
    ?assertEqual(
        {ok, "42"},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = number}, 42)
    ),
    ?assertEqual(
        {ok, "3.14000000000000012434e+00"},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = number}, 3.14)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(
            TypeInfo,
            #sp_simple_type{type = number},
            "not_number"
        )
    ),

    %% boolean
    ?assertEqual(
        {ok, "true"},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = boolean}, true)
    ),
    ?assertEqual(
        {ok, "false"},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = boolean}, false)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = boolean}, "true")
    ),

    %% atom
    ?assertEqual(
        {ok, "hello"},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = atom}, hello)
    ),
    ?assertEqual(
        {ok, "hello world"},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = atom}, 'hello world')
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = atom}, "hello")
    ),

    %% string
    ?assertEqual(
        {ok, "hello"},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = string}, "hello")
    ),
    ?assertEqual(
        {ok, ""},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = string}, "")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(
            TypeInfo,
            #sp_simple_type{type = string},
            <<"binary">>
        )
    ),

    %% nonempty_string
    ?assertEqual(
        {ok, "hello"},
        spectra_string:to_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_string},
            "hello"
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = nonempty_string}, "")
    ),

    %% binary
    ?assertEqual(
        {ok, "hello"},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = binary}, <<"hello">>)
    ),
    ?assertEqual(
        {ok, ""},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = binary}, <<"">>)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = binary}, "string")
    ),

    %% nonempty_binary
    ?assertEqual(
        {ok, "hello"},
        spectra_string:to_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_binary},
            <<"hello">>
        )
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_binary},
            <<"">>
        )
    ),

    %% non_neg_integer
    ?assertEqual(
        {ok, "42"},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = non_neg_integer}, 42)
    ),
    ?assertEqual(
        {ok, "0"},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = non_neg_integer}, 0)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = non_neg_integer}, -1)
    ),

    %% pos_integer
    ?assertEqual(
        {ok, "42"},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = pos_integer}, 42)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = pos_integer}, 0)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = pos_integer}, -1)
    ),

    %% neg_integer
    ?assertEqual(
        {ok, "-42"},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = neg_integer}, -42)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = neg_integer}, 0)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = neg_integer}, 42)
    ),

    ok.

%% Test to_string/3 function - Range types
to_string_range_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    Range =
        #sp_range{
            type = integer,
            lower_bound = 1,
            upper_bound = 10
        },

    %% Valid values in range
    ?assertEqual({ok, "1"}, spectra_string:to_string(TypeInfo, Range, 1)),
    ?assertEqual({ok, "5"}, spectra_string:to_string(TypeInfo, Range, 5)),
    ?assertEqual({ok, "10"}, spectra_string:to_string(TypeInfo, Range, 10)),

    %% Invalid values outside range
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, Range, 0)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, Range, 11)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, Range, -5)
    ),

    %% Invalid non-integer values
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, Range, "5")
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, Range, 5.5)
    ),

    ok.

%% Test to_string/3 function - Literal types
to_string_literal_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Literal atom
    AtomLiteral = #sp_literal{value = hello},
    ?assertEqual({ok, "hello"}, spectra_string:to_string(TypeInfo, AtomLiteral, hello)),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, AtomLiteral, world)
    ),

    %% Literal integer
    IntegerLiteral = #sp_literal{value = 42},
    ?assertEqual({ok, "42"}, spectra_string:to_string(TypeInfo, IntegerLiteral, 42)),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, IntegerLiteral, 43)
    ),

    %% Literal boolean
    BooleanLiteralTrue = #sp_literal{value = true},
    ?assertEqual(
        {ok, "true"},
        spectra_string:to_string(TypeInfo, BooleanLiteralTrue, true)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, BooleanLiteralTrue, false)
    ),

    BooleanLiteralFalse = #sp_literal{value = false},
    ?assertEqual(
        {ok, "false"},
        spectra_string:to_string(TypeInfo, BooleanLiteralFalse, false)
    ),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, BooleanLiteralFalse, true)
    ),

    ok.

%% Test to_string/3 function - Union types
to_string_union_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Simple union: integer | boolean
    Union =
        #sp_union{types = [#sp_simple_type{type = integer}, #sp_simple_type{type = boolean}]},
    ?assertEqual({ok, "42"}, spectra_string:to_string(TypeInfo, Union, 42)),
    ?assertEqual({ok, "true"}, spectra_string:to_string(TypeInfo, Union, true)),
    ?assertEqual({ok, "false"}, spectra_string:to_string(TypeInfo, Union, false)),
    ?assertMatch(
        {error, [#sp_error{type = no_match}]},
        spectra_string:to_string(TypeInfo, Union, "not_matching")
    ),

    %% Complex union with literals: 1 | 2 | true | false
    ComplexUnion =
        #sp_union{
            types =
                [
                    #sp_literal{value = 1},
                    #sp_literal{value = 2},
                    #sp_literal{value = true},
                    #sp_literal{value = false}
                ]
        },
    ?assertEqual({ok, "1"}, spectra_string:to_string(TypeInfo, ComplexUnion, 1)),
    ?assertEqual({ok, "2"}, spectra_string:to_string(TypeInfo, ComplexUnion, 2)),
    ?assertEqual({ok, "true"}, spectra_string:to_string(TypeInfo, ComplexUnion, true)),
    ?assertEqual({ok, "false"}, spectra_string:to_string(TypeInfo, ComplexUnion, false)),
    ?assertMatch(
        {error, [#sp_error{type = no_match}]},
        spectra_string:to_string(TypeInfo, ComplexUnion, 3)
    ),
    ?assertMatch(
        {error, [#sp_error{type = no_match}]},
        spectra_string:to_string(TypeInfo, ComplexUnion, "maybe")
    ),

    ok.

%% Test to_string/3 function - Type references
to_string_type_reference_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Test various type references from the module
    ?assertEqual({ok, "42"}, spectra_string:to_string(TypeInfo, {type, my_integer, 0}, 42)),
    ?assertEqual(
        {ok, "3.14000000000000012434e+00"},
        spectra_string:to_string(TypeInfo, {type, my_float, 0}, 3.14)
    ),
    ?assertEqual({ok, "42"}, spectra_string:to_string(TypeInfo, {type, my_number, 0}, 42)),
    ?assertEqual(
        {ok, "3.14000000000000012434e+00"},
        spectra_string:to_string(TypeInfo, {type, my_number, 0}, 3.14)
    ),
    ?assertEqual(
        {ok, "true"},
        spectra_string:to_string(TypeInfo, {type, my_boolean, 0}, true)
    ),
    ?assertEqual(
        {ok, "hello"},
        spectra_string:to_string(TypeInfo, {type, my_atom, 0}, hello)
    ),
    ?assertEqual(
        {ok, "test"},
        spectra_string:to_string(TypeInfo, {type, my_string, 0}, "test")
    ),
    ?assertEqual(
        {ok, "test"},
        spectra_string:to_string(TypeInfo, {type, my_binary, 0}, <<"test">>)
    ),

    %% Test range type
    ?assertEqual({ok, "5"}, spectra_string:to_string(TypeInfo, {type, my_range, 0}, 5)),
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, {type, my_range, 0}, 15)
    ),

    %% Test literal types
    ?assertEqual(
        {ok, "hello"},
        spectra_string:to_string(TypeInfo, {type, my_literal_atom, 0}, hello)
    ),
    ?assertEqual(
        {ok, "42"},
        spectra_string:to_string(TypeInfo, {type, my_literal_integer, 0}, 42)
    ),
    ?assertEqual(
        {ok, "true"},
        spectra_string:to_string(TypeInfo, {type, my_literal_boolean, 0}, true)
    ),

    %% Test union types
    ?assertEqual({ok, "42"}, spectra_string:to_string(TypeInfo, {type, my_union, 0}, 42)),
    ?assertEqual(
        {ok, "true"},
        spectra_string:to_string(TypeInfo, {type, my_union, 0}, true)
    ),
    ?assertEqual(
        {ok, "1"},
        spectra_string:to_string(TypeInfo, {type, my_complex_union, 0}, 1)
    ),
    ?assertEqual(
        {ok, "false"},
        spectra_string:to_string(TypeInfo, {type, my_complex_union, 0}, false)
    ),

    ok.

%% Test to_string/3 function - Unsupported types
to_string_unsupported_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Record types are not supported for string conversion
    ?assertMatch(
        {error, [#sp_error{type = no_match}]},
        spectra_string:to_string(TypeInfo, {record, some_record}, some_value)
    ),

    %% Unsupported simple types should error
    ?assertError(
        {type_not_supported, _},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = pid}, self())
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = port}, test)
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = reference}, test)
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = bitstring}, test)
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_string:to_string(
            TypeInfo,
            #sp_simple_type{type = nonempty_bitstring},
            test
        )
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = none}, test)
    ),

    %% Unknown type should give type mismatch error
    UnknownType = #sp_tuple{fields = any},
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, UnknownType, some_value)
    ),

    ok.

%% Test from_string/3 with invalid input data type (not a string)
from_string_invalid_input_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Test that passing non-string input returns proper error
    %% Passing binary instead of string
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(
            TypeInfo,
            #sp_simple_type{type = string},
            <<"binary">>
        )
    ),

    %% Passing integer instead of string
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = integer}, 42)
    ),

    %% Passing atom instead of string
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:from_string(TypeInfo, #sp_simple_type{type = atom}, hello)
    ),

    ok.

%% Test to_string/3 with invalid input data type
to_string_invalid_input_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% These should fail with type_mismatch because the input doesn't match the expected type
    %% Passing binary when expecting to convert from string
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(
            TypeInfo,
            #sp_simple_type{type = string},
            <<"binary">>
        )
    ),

    %% Passing string when expecting to convert from binary
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = binary}, "string")
    ),

    %% Passing list when expecting integer
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = integer}, [1, 2, 3])
    ),

    %% Passing string when expecting atom
    ?assertMatch(
        {error, [#sp_error{type = type_mismatch}]},
        spectra_string:to_string(TypeInfo, #sp_simple_type{type = atom}, "hello")
    ),

    ok.
