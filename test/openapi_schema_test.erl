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
-type my_flexible_map() :: #{string() => integer()}.

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
                 erldantic_openapi:type_to_schema(?MODULE, my_integer)),

    %% string
    ?assertEqual({ok, #{type => <<"string">>}},
                 erldantic_openapi:type_to_schema(?MODULE, my_string)),

    %% boolean
    ?assertEqual({ok, #{type => <<"boolean">>}},
                 erldantic_openapi:type_to_schema(?MODULE, my_boolean)),

    %% number
    ?assertEqual({ok, #{type => <<"number">>}},
                 erldantic_openapi:type_to_schema(?MODULE, my_number)),

    %% atom (mapped to string)
    ?assertEqual({ok, #{type => <<"string">>}},
                 erldantic_openapi:type_to_schema(?MODULE, my_atom)),

    %% binary (mapped to string with format)
    ?assertEqual({ok, #{type => <<"string">>, format => <<"binary">>}},
                 erldantic_openapi:type_to_schema(?MODULE, my_binary)),

    %% float
    ?assertEqual({ok, #{type => <<"number">>, format => <<"float">>}},
                 erldantic_openapi:type_to_schema(?MODULE, my_float)).

%% Test range type mappings
range_types_test() ->
    %% Custom range 1..10
    ?assertEqual({ok,
                  #{type => <<"integer">>,
                    minimum => 1,
                    maximum => 10}},
                 erldantic_openapi:type_to_schema(?MODULE, my_range)),

    %% byte (0..255)
    ?assertEqual({ok,
                  #{type => <<"integer">>,
                    minimum => 0,
                    maximum => 255}},
                 erldantic_openapi:type_to_schema(?MODULE, my_byte)),

    %% char (0..1114111)
    ?assertEqual({ok,
                  #{type => <<"integer">>,
                    minimum => 0,
                    maximum => 1114111}},
                 erldantic_openapi:type_to_schema(?MODULE, my_char)).

%% Test literal type mappings
literal_types_test() ->
    %% Literal atom
    ?assertEqual({ok, #{enum => [hello]}},
                 erldantic_openapi:type_to_schema(?MODULE, my_literal_atom)),

    %% Literal integer
    ?assertEqual({ok, #{enum => [42]}},
                 erldantic_openapi:type_to_schema(?MODULE, my_literal_integer)).

%% Test list type mappings
list_types_test() ->
    %% Regular list
    ?assertEqual({ok, #{type => <<"array">>, items => #{type => <<"integer">>}}},
                 erldantic_openapi:type_to_schema(?MODULE, my_list)),

    %% Non-empty list
    ?assertEqual({ok,
                  #{type => <<"array">>,
                    items => #{type => <<"string">>},
                    minItems => 1}},
                 erldantic_openapi:type_to_schema(?MODULE, my_nonempty_list)).

%% Test union type mappings
union_types_test() ->
    %% Simple union
    ?assertEqual({ok, #{oneOf => [#{type => <<"integer">>}, #{type => <<"string">>}]}},
                 erldantic_openapi:type_to_schema(?MODULE, my_union)),

    %% Optional type (union with undefined)
    ?assertEqual({ok, #{oneOf => [#{type => <<"integer">>}, #{enum => [null]}]}},
                 erldantic_openapi:type_to_schema(?MODULE, my_optional)).

%% Test map type mappings
map_types_test() ->
    %% Fixed map fields - order doesn't matter, just check structure
    {ok, MapSchema} = erldantic_openapi:type_to_schema(?MODULE, my_map),
    ?assertEqual(<<"object">>, maps:get(type, MapSchema)),
    ?assertEqual(#{name => #{type => <<"string">>}, age => #{type => <<"integer">>}},
                 maps:get(properties, MapSchema)),
    ?assert(lists:sort([name, age])
            =:= lists:sort(
                    maps:get(required, MapSchema))),

    %% Flexible map (additionalProperties) - check actual output for now
    {ok, FlexMapSchema} = erldantic_openapi:type_to_schema(?MODULE, my_flexible_map),
    ?assertEqual(<<"object">>, maps:get(type, FlexMapSchema)).

%% Test record type mappings
record_types_test() ->
    %% Simple record - check the actual return format
    {ok, UserSchema} = erldantic_openapi:record_to_schema(?MODULE, user),
    ?assertEqual(<<"object">>, maps:get(type, UserSchema)),
    ?assertEqual(#{id => #{type => <<"integer">>},
                   name => #{type => <<"string">>},
                   email => #{type => <<"string">>}},
                 maps:get(properties, UserSchema)),
    ?assert(lists:sort([id, name, email])
            =:= lists:sort(
                    maps:get(required, UserSchema))),

    %% Record with array field
    {ok, ProductSchema} = erldantic_openapi:record_to_schema(?MODULE, product),
    ?assertEqual(<<"object">>, maps:get(type, ProductSchema)),
    ExpectedProps =
        #{id => #{type => <<"integer">>},
          name => #{type => <<"string">>},
          price => #{type => <<"number">>, format => <<"float">>},
          tags => #{type => <<"array">>, items => #{type => <<"string">>}}},
    ?assertEqual(ExpectedProps, maps:get(properties, ProductSchema)),
    ?assert(lists:sort([id, name, price, tags])
            =:= lists:sort(
                    maps:get(required, ProductSchema))).

%% Test error handling
error_handling_test() ->
    %% Non-existent type
    {error, [Error1]} = erldantic_openapi:type_to_schema(?MODULE, non_existent_type),
    ?assertEqual(no_match, Error1#ed_error.type),
    ?assertEqual([openapi_schema_test], Error1#ed_error.location),

    %% Non-existent record
    {error, [Error2]} = erldantic_openapi:record_to_schema(?MODULE, non_existent_record),
    ?assertEqual(no_match, Error2#ed_error.type),
    ?assertEqual([openapi_schema_test], Error2#ed_error.location).
