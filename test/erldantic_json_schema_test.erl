-module(openapi_schema_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-compile(nowarn_unused_type).

%% Test types for schema generation
-type my_integer() :: integer().
-type my_string() :: string().
-type my_boolean() :: boolean().
-type my_number() :: number().
-type my_atom() :: atom().
-type my_binary() :: binary().
-type my_float() :: float().
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

%% Record types
-record(user, {id :: integer(), name :: string(), email :: string()}).
-record(product,
        {id :: integer(), name :: string(), price :: float(), tags :: [string()]}).

%% Type aliases for records to avoid unused warnings
-type user() :: #user{}.
-type product() :: #product{}.

%% Test simple type mappings
simple_types_test() ->
    %% integer
    ?assertEqual({ok, #{type => <<"integer">>}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_integer)),

    %% string
    ?assertEqual({ok, #{type => <<"string">>}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_string)),

    %% boolean
    ?assertEqual({ok, #{type => <<"boolean">>}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_boolean)),

    %% number
    ?assertEqual({ok, #{type => <<"number">>}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_number)),

    %% atom (mapped to string)
    ?assertEqual({ok, #{type => <<"string">>}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_atom)),

    %% binary (mapped to string with format)
    ?assertEqual({ok, #{type => <<"string">>, format => <<"binary">>}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_binary)),

    %% float
    ?assertEqual({ok, #{type => <<"number">>, format => <<"float">>}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_float)).

%% Test range type mappings
range_types_test() ->
    %% Custom range 1..10
    ?assertEqual({ok,
                  #{type => <<"integer">>,
                    minimum => 1,
                    maximum => 10}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_range)),

    %% byte (0..255)
    ?assertEqual({ok,
                  #{type => <<"integer">>,
                    minimum => 0,
                    maximum => 255}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_byte)),

    %% char (0..1114111)
    ?assertEqual({ok,
                  #{type => <<"integer">>,
                    minimum => 0,
                    maximum => 1114111}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_char)).

%% Test literal type mappings
literal_types_test() ->
    %% Literal atom
    ?assertEqual({ok, #{enum => [hello]}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_literal_atom)),

    %% Literal integer
    ?assertEqual({ok, #{enum => [42]}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_literal_integer)).

%% Test list type mappings
list_types_test() ->
    %% Regular list
    ?assertEqual({ok, #{type => <<"array">>, items => #{type => <<"integer">>}}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_list)),

    %% Non-empty list
    ?assertEqual({ok,
                  #{type => <<"array">>,
                    items => #{type => <<"string">>},
                    minItems => 1}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_nonempty_list)).

%% Test union type mappings
union_types_test() ->
    %% Simple union
    ?assertEqual({ok, #{oneOf => [#{type => <<"integer">>}, #{type => <<"string">>}]}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_union)),

    %% Optional type (union with undefined)
    ?assertEqual({ok, #{oneOf => [#{type => <<"integer">>}, #{enum => [null]}]}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_optional)).

%% Test map type mappings
map_types_test() ->
    %% Fixed map fields - order doesn't matter, just check structure
    ?assertEqual({ok,
                  #{type => <<"object">>,
                    properties =>
                        #{name => #{type => <<"string">>}, age => #{type => <<"integer">>}},
                    required => [age, name]}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_map)),

    %% Structured map with specific fields
    ?assertEqual({ok,
                  #{type => <<"object">>,
                    properties =>
                        #{config => #{type => <<"string">>}, timeout => #{type => <<"integer">>}},
                    required => [timeout, config]}},
                 erldantic_json_schema:type_to_schema(?MODULE, my_flexible_map)).

%% Test record type mappings
record_types_test() ->
    %% Simple record - check the actual return format
    ?assertEqual({ok,
                  #{type => <<"object">>,
                    properties =>
                        #{id => #{type => <<"integer">>},
                          name => #{type => <<"string">>},
                          email => #{type => <<"string">>}},
                    required => [id, name, email]}},
                 erldantic_json_schema:record_to_schema(?MODULE, user)),

    %% Record with array field
    ExpectedProps =
        #{id => #{type => <<"integer">>},
          name => #{type => <<"string">>},
          price => #{type => <<"number">>, format => <<"float">>},
          tags => #{type => <<"array">>, items => #{type => <<"string">>}}},
    ?assertEqual({ok,
                  #{type => <<"object">>,
                    properties => ExpectedProps,
                    required => [id, name, price, tags]}},
                 erldantic_json_schema:record_to_schema(?MODULE, product)).

%% Test error handling
error_handling_test() ->
    %% Non-existent type
    {error, [Error1]} = erldantic_json_schema:type_to_schema(?MODULE, non_existent_type),
    ?assertEqual(no_match, Error1#ed_error.type),
    ?assertEqual([openapi_schema_test], Error1#ed_error.location),

    %% Non-existent record
    {error, [Error2]} = erldantic_json_schema:record_to_schema(?MODULE, non_existent_record),
    ?assertEqual(no_match, Error2#ed_error.type),
    ?assertEqual([openapi_schema_test], Error2#ed_error.location).
