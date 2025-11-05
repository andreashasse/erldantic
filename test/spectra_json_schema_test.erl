-module(spectra_json_schema_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

%% Test types for schema generation
-type my_integer() :: integer().
-type my_string() :: string().
-type my_boolean() :: boolean().
-type my_number() :: number().
-type my_atom() :: atom().
-type my_binary() :: binary().
-type my_float() :: float().
-type my_iodata() :: iodata().
-type my_iolist() :: iolist().
%% Range types
-type my_range() :: 1..10.
-type my_byte() :: byte().
-type my_char() :: char().
%% Literal types
-type my_literal_atom() :: hello.
-type my_literal_integer() :: 42.
%% List types
-type my_list() :: [integer()].
-type my_nonempty_list() :: [string(), ...].
%% Union types
-type my_union() :: integer() | string().
-type my_optional() :: integer() | undefined.
%% Map types
-type my_map() :: #{name := string(), age := integer()}.
-type my_flexible_map() :: #{config := string(), timeout := integer()}.
%% Generic map types (allow additional properties)
-type my_generic_map() :: #{atom() => integer()}.
-type my_mixed_map() :: #{name := string(), atom() => integer()}.

%% Record types
-record(user, {id :: integer(), name :: string(), email :: string()}).
-record(product, {id :: integer(), name :: string(), price :: float(), tags :: [string()]}).
-record(user_with_optional, {id :: integer(), name :: string(), email :: string() | undefined}).

%% Type aliases for records to avoid unused warnings
-type user() :: #user{}.
-type product() :: #product{}.
-type user_with_optional() :: #user_with_optional{}.

%% Test simple type mappings
simple_types_test() ->
    %% integer
    ?assertEqual(
        {ok, #{type => <<"integer">>}},
        spectra_json_schema:to_schema(?MODULE, {type, my_integer, 0})
    ),

    %% string
    ?assertEqual(
        {ok, #{type => <<"string">>}},
        spectra_json_schema:to_schema(?MODULE, {type, my_string, 0})
    ),

    %% boolean
    ?assertEqual(
        {ok, #{type => <<"boolean">>}},
        spectra_json_schema:to_schema(?MODULE, {type, my_boolean, 0})
    ),

    %% number
    ?assertEqual(
        {ok, #{type => <<"number">>}},
        spectra_json_schema:to_schema(?MODULE, {type, my_number, 0})
    ),

    %% atom (mapped to string)
    ?assertEqual(
        {ok, #{type => <<"string">>}},
        spectra_json_schema:to_schema(?MODULE, {type, my_atom, 0})
    ),

    %% binary (mapped to string)
    ?assertEqual(
        {ok, #{type => <<"string">>}},
        spectra_json_schema:to_schema(?MODULE, {type, my_binary, 0})
    ),

    %% float
    ?assertEqual(
        {ok, #{type => <<"number">>, format => <<"float">>}},
        spectra_json_schema:to_schema(?MODULE, {type, my_float, 0})
    ),

    %% iodata (mapped to string)
    ?assertEqual(
        {ok, #{type => <<"string">>}},
        spectra_json_schema:to_schema(?MODULE, {type, my_iodata, 0})
    ),

    %% iolist (mapped to string)
    ?assertEqual(
        {ok, #{type => <<"string">>}},
        spectra_json_schema:to_schema(?MODULE, {type, my_iolist, 0})
    ).

%% Test range type mappings
range_types_test() ->
    %% Custom range 1..10
    ?assertEqual(
        {ok, #{
            type => <<"integer">>,
            minimum => 1,
            maximum => 10
        }},
        spectra_json_schema:to_schema(?MODULE, {type, my_range, 0})
    ),

    %% byte (0..255)
    ?assertEqual(
        {ok, #{
            type => <<"integer">>,
            minimum => 0,
            maximum => 255
        }},
        spectra_json_schema:to_schema(?MODULE, {type, my_byte, 0})
    ),

    %% char (0..1114111)
    ?assertEqual(
        {ok, #{
            type => <<"integer">>,
            minimum => 0,
            maximum => 1114111
        }},
        spectra_json_schema:to_schema(?MODULE, {type, my_char, 0})
    ).

%% Test literal type mappings
literal_types_test() ->
    %% Literal atom (converted to binary string)
    ?assertEqual(
        {ok, #{enum => [<<"hello">>]}},
        spectra_json_schema:to_schema(?MODULE, {type, my_literal_atom, 0})
    ),

    %% Literal integer
    ?assertEqual(
        {ok, #{enum => [42]}},
        spectra_json_schema:to_schema(?MODULE, {type, my_literal_integer, 0})
    ).

%% Test list type mappings
list_types_test() ->
    %% Regular list
    ?assertEqual(
        {ok, #{type => <<"array">>, items => #{type => <<"integer">>}}},
        spectra_json_schema:to_schema(?MODULE, {type, my_list, 0})
    ),

    %% Non-empty list
    ?assertEqual(
        {ok, #{
            type => <<"array">>,
            items => #{type => <<"string">>},
            minItems => 1
        }},
        spectra_json_schema:to_schema(?MODULE, {type, my_nonempty_list, 0})
    ).

%% Test union type mappings
union_types_test() ->
    %% Simple union
    ?assertEqual(
        {ok, #{oneOf => [#{type => <<"integer">>}, #{type => <<"string">>}]}},
        spectra_json_schema:to_schema(?MODULE, {type, my_union, 0})
    ),

    %% Optional type (union with undefined) - now returns just the non-undefined type
    ?assertEqual(
        {ok, #{type => <<"integer">>}},
        spectra_json_schema:to_schema(?MODULE, {type, my_optional, 0})
    ).

%% Test map type mappings
map_types_test() ->
    %% Fixed map fields - order doesn't matter, just check structure
    ?assertEqual(
        {ok, #{
            type => <<"object">>,
            properties =>
                #{<<"name">> => #{type => <<"string">>}, <<"age">> => #{type => <<"integer">>}},
            required => [<<"age">>, <<"name">>],
            additionalProperties => false
        }},
        spectra_json_schema:to_schema(?MODULE, {type, my_map, 0})
    ),

    %% Structured map with specific fields
    ?assertEqual(
        {ok, #{
            type => <<"object">>,
            properties =>
                #{
                    <<"config">> => #{type => <<"string">>},
                    <<"timeout">> => #{type => <<"integer">>}
                },
            required => [<<"timeout">>, <<"config">>],
            additionalProperties => false
        }},
        spectra_json_schema:to_schema(?MODULE, {type, my_flexible_map, 0})
    ).

%% Test generic map types with additional properties
generic_map_types_test() ->
    %% Generic map with atom keys and integer values
    ?assertEqual(
        {ok, #{type => <<"object">>, additionalProperties => true}},
        spectra_json_schema:to_schema(?MODULE, {type, my_generic_map, 0})
    ),

    %% Mixed map with both specific and generic fields
    ?assertEqual(
        {ok, #{
            type => <<"object">>,
            properties => #{<<"name">> => #{type => <<"string">>}},
            required => [<<"name">>],
            additionalProperties => true
        }},
        spectra_json_schema:to_schema(?MODULE, {type, my_mixed_map, 0})
    ).

%% Test record type mappings
record_types_test() ->
    %% Simple record - check the actual return format
    ?assertEqual(
        {ok, #{
            type => <<"object">>,
            properties =>
                #{
                    <<"id">> => #{type => <<"integer">>},
                    <<"name">> => #{type => <<"string">>},
                    <<"email">> => #{type => <<"string">>}
                },
            required => [<<"id">>, <<"name">>, <<"email">>]
        }},
        spectra_json_schema:to_schema(?MODULE, {record, user})
    ),
    %% Record with array field
    ExpectedProps =
        #{
            <<"id">> => #{type => <<"integer">>},
            <<"name">> => #{type => <<"string">>},
            <<"price">> => #{type => <<"number">>, format => <<"float">>},
            <<"tags">> => #{type => <<"array">>, items => #{type => <<"string">>}}
        },
    ?assertEqual(
        {ok, #{
            type => <<"object">>,
            properties => ExpectedProps,
            required => [<<"id">>, <<"name">>, <<"price">>, <<"tags">>]
        }},
        spectra_json_schema:to_schema(?MODULE, {record, product})
    ).

%% Test record with optional field
record_with_optional_fields_test() ->
    %% Record with optional email field (can be undefined)
    %% The field appears as simple string type and is excluded from required fields
    ?assertEqual(
        {ok, #{
            type => <<"object">>,
            properties =>
                #{
                    <<"id">> => #{type => <<"integer">>},
                    <<"name">> => #{type => <<"string">>},
                    <<"email">> => #{type => <<"string">>}
                },
            required => [<<"id">>, <<"name">>]
        }},
        spectra_json_schema:to_schema(?MODULE, {record, user_with_optional})
    ).

%% Test error handling
error_handling_test() ->
    ?assertError(_, spectra_json_schema:to_schema(?MODULE, {type, non_existent_type, 0})),
    ?assertError(_, spectra_json_schema:to_schema(?MODULE, {record, non_existent_record})).
