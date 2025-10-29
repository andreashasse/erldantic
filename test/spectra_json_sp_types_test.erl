-module(spectra_json_sp_types_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").

-compile(nowarn_unused_type).

%% Test types for sp_types testing
-type my_integer() :: integer().
-type my_string() :: string().
-type my_boolean() :: boolean().
-type my_list_of_integers() :: [integer()].
-type my_union() :: integer() | string().
-type my_map() :: #{name := string(), age := integer()}.
-type my_literal_atom() :: hello.
-type my_literal_integer() :: 42.
-type my_range() :: 1..10.
-type my_nonempty_list() :: [integer(), ...].

%% Record for testing
-record(user, {id :: integer(), name :: string(), email :: string()}).

-type user() :: #user{}.

%% Test using sp_types directly with spectra_json:to_json/3
sp_types_to_json_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Test simple types
    {ok, IntegerType} = spectra_type_info:get_type(TypeInfo, my_integer, 0),
    ?assertEqual({ok, 42}, spectra_json:to_json(TypeInfo, IntegerType, 42)),

    {ok, StringType} = spectra_type_info:get_type(TypeInfo, my_string, 0),
    ?assertEqual({ok, <<"hello">>}, spectra_json:to_json(TypeInfo, StringType, "hello")),

    {ok, BooleanType} = spectra_type_info:get_type(TypeInfo, my_boolean, 0),
    ?assertEqual({ok, true}, spectra_json:to_json(TypeInfo, BooleanType, true)),

    %% Test list types
    {ok, ListType} = spectra_type_info:get_type(TypeInfo, my_list_of_integers, 0),
    ?assertEqual({ok, [1, 2, 3]}, spectra_json:to_json(TypeInfo, ListType, [1, 2, 3])),

    %% Test union types
    {ok, UnionType} = spectra_type_info:get_type(TypeInfo, my_union, 0),
    ?assertEqual({ok, 42}, spectra_json:to_json(TypeInfo, UnionType, 42)),
    ?assertEqual({ok, <<"hello">>}, spectra_json:to_json(TypeInfo, UnionType, "hello")),

    %% Test map types
    {ok, MapType} = spectra_type_info:get_type(TypeInfo, my_map, 0),
    MapData = #{name => "John", age => 30},
    ?assertEqual(
        {ok, #{name => <<"John">>, age => 30}},
        spectra_json:to_json(TypeInfo, MapType, MapData)
    ).

%% Test using sp_types directly with spectra_json:from_json/3
sp_types_from_json_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Test simple types
    {ok, IntegerType} = spectra_type_info:get_type(TypeInfo, my_integer, 0),
    ?assertEqual({ok, 42}, spectra_json:from_json(TypeInfo, IntegerType, 42)),

    {ok, StringType} = spectra_type_info:get_type(TypeInfo, my_string, 0),
    ?assertEqual({ok, "hello"}, spectra_json:from_json(TypeInfo, StringType, <<"hello">>)),

    {ok, BooleanType} = spectra_type_info:get_type(TypeInfo, my_boolean, 0),
    ?assertEqual({ok, true}, spectra_json:from_json(TypeInfo, BooleanType, true)),

    %% Test list types
    {ok, ListType} = spectra_type_info:get_type(TypeInfo, my_list_of_integers, 0),
    ?assertEqual({ok, [1, 2, 3]}, spectra_json:from_json(TypeInfo, ListType, [1, 2, 3])),

    %% Test union types
    {ok, UnionType} = spectra_type_info:get_type(TypeInfo, my_union, 0),
    ?assertEqual({ok, 42}, spectra_json:from_json(TypeInfo, UnionType, 42)),
    ?assertEqual({ok, "hello"}, spectra_json:from_json(TypeInfo, UnionType, <<"hello">>)),

    %% Test map types
    {ok, MapType} = spectra_type_info:get_type(TypeInfo, my_map, 0),
    JsonData = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual(
        {ok, #{name => "John", age => 30}},
        spectra_json:from_json(TypeInfo, MapType, JsonData)
    ).

%% Test using sp_rec directly with spectra_json functions
sp_record_types_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Test record serialization using sp_rec
    {ok, UserRecord} = spectra_type_info:get_record(TypeInfo, user),
    UserData =
        #user{
            id = 1,
            name = "Alice",
            email = "alice@example.com"
        },

    ?assertEqual(
        {ok, #{
            id => 1,
            name => <<"Alice">>,
            email => <<"alice@example.com">>
        }},
        spectra_json:to_json(TypeInfo, UserRecord, UserData)
    ),

    %% Test record deserialization using sp_rec
    JsonData =
        #{
            <<"id">> => 1,
            <<"name">> => <<"Alice">>,
            <<"email">> => <<"alice@example.com">>
        },
    ?assertEqual({ok, UserData}, spectra_json:from_json(TypeInfo, UserRecord, JsonData)).

%% Test literal sp_types (extracted from types)
literal_sp_types_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Test literal atom
    {ok, AtomLiteralType} = spectra_type_info:get_type(TypeInfo, my_literal_atom, 0),
    ?assertEqual({ok, hello}, spectra_json:to_json(TypeInfo, AtomLiteralType, hello)),
    ?assertEqual(
        {ok, hello},
        spectra_json:from_json(TypeInfo, AtomLiteralType, <<"hello">>)
    ),

    %% Test literal integer
    {ok, IntLiteralType} = spectra_type_info:get_type(TypeInfo, my_literal_integer, 0),
    ?assertEqual({ok, 42}, spectra_json:to_json(TypeInfo, IntLiteralType, 42)),
    ?assertEqual({ok, 42}, spectra_json:from_json(TypeInfo, IntLiteralType, 42)).

%% Test range sp_types (extracted from types)
range_sp_types_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Test integer range
    {ok, RangeType} = spectra_type_info:get_type(TypeInfo, my_range, 0),
    ?assertEqual({ok, 5}, spectra_json:to_json(TypeInfo, RangeType, 5)),
    ?assertEqual({ok, 5}, spectra_json:from_json(TypeInfo, RangeType, 5)),

    %% Test out of range values
    ?assertMatch({error, _}, spectra_json:to_json(TypeInfo, RangeType, 15)),
    ?assertMatch({error, _}, spectra_json:from_json(TypeInfo, RangeType, 15)).

%% Test nonempty list sp_types (extracted from types)
nonempty_list_sp_types_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Test nonempty list
    {ok, NonEmptyListType} = spectra_type_info:get_type(TypeInfo, my_nonempty_list, 0),
    ?assertEqual(
        {ok, [1, 2, 3]},
        spectra_json:to_json(TypeInfo, NonEmptyListType, [1, 2, 3])
    ),
    ?assertEqual(
        {ok, [1, 2, 3]},
        spectra_json:from_json(TypeInfo, NonEmptyListType, [1, 2, 3])
    ),

    %% Test empty list should fail
    ?assertMatch({error, _}, spectra_json:to_json(TypeInfo, NonEmptyListType, [])),
    ?assertMatch({error, _}, spectra_json:from_json(TypeInfo, NonEmptyListType, [])).

%% Test with type info from different module
cross_module_sp_types_test() ->
    %% Use spectra_json_schema_test module types with this module's TypeInfo
    OtherModuleTypeInfo = spectra_abstract_code:types_in_module(spectra_json_schema_test),

    %% Get a type from the other module and use it
    {ok, MyIntegerType} = spectra_type_info:get_type(OtherModuleTypeInfo, my_integer, 0),
    ?assertEqual({ok, 42}, spectra_json:to_json(OtherModuleTypeInfo, MyIntegerType, 42)),
    ?assertEqual({ok, 42}, spectra_json:from_json(OtherModuleTypeInfo, MyIntegerType, 42)).

%% Test passing TypeInfo vs passing module directly
typeinfo_vs_module_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    {ok, IntegerType} = spectra_type_info:get_type(TypeInfo, my_integer, 0),

    %% These two should give the same result
    Result1 = spectra_json:to_json(?MODULE, IntegerType, 42),
    Result2 = spectra_json:to_json(TypeInfo, IntegerType, 42),
    ?assertEqual(Result1, Result2),
    ?assertEqual({ok, 42}, Result1),

    %% Same for from_json
    JsonResult1 = spectra_json:from_json(?MODULE, IntegerType, 42),
    JsonResult2 = spectra_json:from_json(TypeInfo, IntegerType, 42),
    ?assertEqual(JsonResult1, JsonResult2),
    ?assertEqual({ok, 42}, JsonResult1).
