-module(erldantic_json_ed_types_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").

-compile(nowarn_unused_type).

%% Test types for ed_types testing
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

%% Test using ed_types directly with erldantic_json:to_json/3
ed_types_to_json_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),

    %% Test simple types
    {ok, IntegerType} = erldantic_type_info:get_type(TypeInfo, my_integer, 0),
    ?assertEqual({ok, 42}, erldantic_json:to_json(TypeInfo, IntegerType, 42)),

    {ok, StringType} = erldantic_type_info:get_type(TypeInfo, my_string, 0),
    ?assertEqual({ok, <<"hello">>}, erldantic_json:to_json(TypeInfo, StringType, "hello")),

    {ok, BooleanType} = erldantic_type_info:get_type(TypeInfo, my_boolean, 0),
    ?assertEqual({ok, true}, erldantic_json:to_json(TypeInfo, BooleanType, true)),

    %% Test list types
    {ok, ListType} = erldantic_type_info:get_type(TypeInfo, my_list_of_integers, 0),
    ?assertEqual({ok, [1, 2, 3]}, erldantic_json:to_json(TypeInfo, ListType, [1, 2, 3])),

    %% Test union types
    {ok, UnionType} = erldantic_type_info:get_type(TypeInfo, my_union, 0),
    ?assertEqual({ok, 42}, erldantic_json:to_json(TypeInfo, UnionType, 42)),
    ?assertEqual({ok, <<"hello">>}, erldantic_json:to_json(TypeInfo, UnionType, "hello")),

    %% Test map types
    {ok, MapType} = erldantic_type_info:get_type(TypeInfo, my_map, 0),
    MapData = #{name => "John", age => 30},
    ?assertEqual(
        {ok, #{name => <<"John">>, age => 30}},
        erldantic_json:to_json(TypeInfo, MapType, MapData)
    ).

%% Test using ed_types directly with erldantic_json:from_json/3
ed_types_from_json_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),

    %% Test simple types
    {ok, IntegerType} = erldantic_type_info:get_type(TypeInfo, my_integer, 0),
    ?assertEqual({ok, 42}, erldantic_json:from_json(TypeInfo, IntegerType, 42)),

    {ok, StringType} = erldantic_type_info:get_type(TypeInfo, my_string, 0),
    ?assertEqual({ok, "hello"}, erldantic_json:from_json(TypeInfo, StringType, <<"hello">>)),

    {ok, BooleanType} = erldantic_type_info:get_type(TypeInfo, my_boolean, 0),
    ?assertEqual({ok, true}, erldantic_json:from_json(TypeInfo, BooleanType, true)),

    %% Test list types
    {ok, ListType} = erldantic_type_info:get_type(TypeInfo, my_list_of_integers, 0),
    ?assertEqual({ok, [1, 2, 3]}, erldantic_json:from_json(TypeInfo, ListType, [1, 2, 3])),

    %% Test union types
    {ok, UnionType} = erldantic_type_info:get_type(TypeInfo, my_union, 0),
    ?assertEqual({ok, 42}, erldantic_json:from_json(TypeInfo, UnionType, 42)),
    ?assertEqual({ok, "hello"}, erldantic_json:from_json(TypeInfo, UnionType, <<"hello">>)),

    %% Test map types
    {ok, MapType} = erldantic_type_info:get_type(TypeInfo, my_map, 0),
    JsonData = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual(
        {ok, #{name => "John", age => 30}},
        erldantic_json:from_json(TypeInfo, MapType, JsonData)
    ).

%% Test using ed_rec directly with erldantic_json functions
ed_record_types_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),

    %% Test record serialization using ed_rec
    {ok, UserRecord} = erldantic_type_info:get_record(TypeInfo, user),
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
        erldantic_json:to_json(TypeInfo, UserRecord, UserData)
    ),

    %% Test record deserialization using ed_rec
    JsonData =
        #{
            <<"id">> => 1,
            <<"name">> => <<"Alice">>,
            <<"email">> => <<"alice@example.com">>
        },
    ?assertEqual({ok, UserData}, erldantic_json:from_json(TypeInfo, UserRecord, JsonData)).

%% Test literal ed_types (extracted from types)
literal_ed_types_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),

    %% Test literal atom
    {ok, AtomLiteralType} = erldantic_type_info:get_type(TypeInfo, my_literal_atom, 0),
    ?assertEqual({ok, hello}, erldantic_json:to_json(TypeInfo, AtomLiteralType, hello)),
    ?assertEqual(
        {ok, hello},
        erldantic_json:from_json(TypeInfo, AtomLiteralType, <<"hello">>)
    ),

    %% Test literal integer
    {ok, IntLiteralType} = erldantic_type_info:get_type(TypeInfo, my_literal_integer, 0),
    ?assertEqual({ok, 42}, erldantic_json:to_json(TypeInfo, IntLiteralType, 42)),
    ?assertEqual({ok, 42}, erldantic_json:from_json(TypeInfo, IntLiteralType, 42)).

%% Test range ed_types (extracted from types)
range_ed_types_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),

    %% Test integer range
    {ok, RangeType} = erldantic_type_info:get_type(TypeInfo, my_range, 0),
    ?assertEqual({ok, 5}, erldantic_json:to_json(TypeInfo, RangeType, 5)),
    ?assertEqual({ok, 5}, erldantic_json:from_json(TypeInfo, RangeType, 5)),

    %% Test out of range values
    ?assertMatch({error, _}, erldantic_json:to_json(TypeInfo, RangeType, 15)),
    ?assertMatch({error, _}, erldantic_json:from_json(TypeInfo, RangeType, 15)).

%% Test nonempty list ed_types (extracted from types)
nonempty_list_ed_types_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),

    %% Test nonempty list
    {ok, NonEmptyListType} = erldantic_type_info:get_type(TypeInfo, my_nonempty_list, 0),
    ?assertEqual(
        {ok, [1, 2, 3]},
        erldantic_json:to_json(TypeInfo, NonEmptyListType, [1, 2, 3])
    ),
    ?assertEqual(
        {ok, [1, 2, 3]},
        erldantic_json:from_json(TypeInfo, NonEmptyListType, [1, 2, 3])
    ),

    %% Test empty list should fail
    ?assertMatch({error, _}, erldantic_json:to_json(TypeInfo, NonEmptyListType, [])),
    ?assertMatch({error, _}, erldantic_json:from_json(TypeInfo, NonEmptyListType, [])).

%% Test with type info from different module
cross_module_ed_types_test() ->
    %% Use erldantic_json_schema_test module types with this module's TypeInfo
    OtherModuleTypeInfo = erldantic_abstract_code:types_in_module(erldantic_json_schema_test),

    %% Get a type from the other module and use it
    {ok, MyIntegerType} = erldantic_type_info:get_type(OtherModuleTypeInfo, my_integer, 0),
    ?assertEqual({ok, 42}, erldantic_json:to_json(OtherModuleTypeInfo, MyIntegerType, 42)),
    ?assertEqual({ok, 42}, erldantic_json:from_json(OtherModuleTypeInfo, MyIntegerType, 42)).

%% Test passing TypeInfo vs passing module directly
typeinfo_vs_module_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),
    {ok, IntegerType} = erldantic_type_info:get_type(TypeInfo, my_integer, 0),

    %% These two should give the same result
    Result1 = erldantic_json:to_json(?MODULE, IntegerType, 42),
    Result2 = erldantic_json:to_json(TypeInfo, IntegerType, 42),
    ?assertEqual(Result1, Result2),
    ?assertEqual({ok, 42}, Result1),

    %% Same for from_json
    JsonResult1 = erldantic_json:from_json(?MODULE, IntegerType, 42),
    JsonResult2 = erldantic_json:from_json(TypeInfo, IntegerType, 42),
    ?assertEqual(JsonResult1, JsonResult2),
    ?assertEqual({ok, 42}, JsonResult1).
