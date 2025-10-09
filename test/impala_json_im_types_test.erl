-module(impala_json_im_types_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/impala.hrl").

-compile(nowarn_unused_type).

%% Test types for im_types testing
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

%% Test using im_types directly with impala_json:to_json/3
im_types_to_json_test() ->
    TypeInfo = impala_abstract_code:types_in_module(?MODULE),

    %% Test simple types
    {ok, IntegerType} = impala_type_info:get_type(TypeInfo, my_integer, 0),
    ?assertEqual({ok, 42}, impala_json:to_json(TypeInfo, IntegerType, 42)),

    {ok, StringType} = impala_type_info:get_type(TypeInfo, my_string, 0),
    ?assertEqual({ok, <<"hello">>}, impala_json:to_json(TypeInfo, StringType, "hello")),

    {ok, BooleanType} = impala_type_info:get_type(TypeInfo, my_boolean, 0),
    ?assertEqual({ok, true}, impala_json:to_json(TypeInfo, BooleanType, true)),

    %% Test list types
    {ok, ListType} = impala_type_info:get_type(TypeInfo, my_list_of_integers, 0),
    ?assertEqual({ok, [1, 2, 3]}, impala_json:to_json(TypeInfo, ListType, [1, 2, 3])),

    %% Test union types
    {ok, UnionType} = impala_type_info:get_type(TypeInfo, my_union, 0),
    ?assertEqual({ok, 42}, impala_json:to_json(TypeInfo, UnionType, 42)),
    ?assertEqual({ok, <<"hello">>}, impala_json:to_json(TypeInfo, UnionType, "hello")),

    %% Test map types
    {ok, MapType} = impala_type_info:get_type(TypeInfo, my_map, 0),
    MapData = #{name => "John", age => 30},
    ?assertEqual({ok, #{name => <<"John">>, age => 30}},
                 impala_json:to_json(TypeInfo, MapType, MapData)).

%% Test using im_types directly with impala_json:from_json/3
im_types_from_json_test() ->
    TypeInfo = impala_abstract_code:types_in_module(?MODULE),

    %% Test simple types
    {ok, IntegerType} = impala_type_info:get_type(TypeInfo, my_integer, 0),
    ?assertEqual({ok, 42}, impala_json:from_json(TypeInfo, IntegerType, 42)),

    {ok, StringType} = impala_type_info:get_type(TypeInfo, my_string, 0),
    ?assertEqual({ok, "hello"}, impala_json:from_json(TypeInfo, StringType, <<"hello">>)),

    {ok, BooleanType} = impala_type_info:get_type(TypeInfo, my_boolean, 0),
    ?assertEqual({ok, true}, impala_json:from_json(TypeInfo, BooleanType, true)),

    %% Test list types
    {ok, ListType} = impala_type_info:get_type(TypeInfo, my_list_of_integers, 0),
    ?assertEqual({ok, [1, 2, 3]}, impala_json:from_json(TypeInfo, ListType, [1, 2, 3])),

    %% Test union types
    {ok, UnionType} = impala_type_info:get_type(TypeInfo, my_union, 0),
    ?assertEqual({ok, 42}, impala_json:from_json(TypeInfo, UnionType, 42)),
    ?assertEqual({ok, "hello"}, impala_json:from_json(TypeInfo, UnionType, <<"hello">>)),

    %% Test map types
    {ok, MapType} = impala_type_info:get_type(TypeInfo, my_map, 0),
    JsonData = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual({ok, #{name => "John", age => 30}},
                 impala_json:from_json(TypeInfo, MapType, JsonData)).

%% Test using im_rec directly with impala_json functions
im_record_types_test() ->
    TypeInfo = impala_abstract_code:types_in_module(?MODULE),

    %% Test record serialization using im_rec
    {ok, UserRecord} = impala_type_info:get_record(TypeInfo, user),
    UserData =
        #user{id = 1,
              name = "Alice",
              email = "alice@example.com"},

    ?assertEqual({ok,
                  #{id => 1,
                    name => <<"Alice">>,
                    email => <<"alice@example.com">>}},
                 impala_json:to_json(TypeInfo, UserRecord, UserData)),

    %% Test record deserialization using im_rec
    JsonData =
        #{<<"id">> => 1,
          <<"name">> => <<"Alice">>,
          <<"email">> => <<"alice@example.com">>},
    ?assertEqual({ok, UserData}, impala_json:from_json(TypeInfo, UserRecord, JsonData)).

%% Test literal im_types (extracted from types)
literal_im_types_test() ->
    TypeInfo = impala_abstract_code:types_in_module(?MODULE),

    %% Test literal atom
    {ok, AtomLiteralType} = impala_type_info:get_type(TypeInfo, my_literal_atom, 0),
    ?assertEqual({ok, hello}, impala_json:to_json(TypeInfo, AtomLiteralType, hello)),
    ?assertEqual({ok, hello}, impala_json:from_json(TypeInfo, AtomLiteralType, <<"hello">>)),

    %% Test literal integer
    {ok, IntLiteralType} = impala_type_info:get_type(TypeInfo, my_literal_integer, 0),
    ?assertEqual({ok, 42}, impala_json:to_json(TypeInfo, IntLiteralType, 42)),
    ?assertEqual({ok, 42}, impala_json:from_json(TypeInfo, IntLiteralType, 42)).

%% Test range im_types (extracted from types)
range_im_types_test() ->
    TypeInfo = impala_abstract_code:types_in_module(?MODULE),

    %% Test integer range
    {ok, RangeType} = impala_type_info:get_type(TypeInfo, my_range, 0),
    ?assertEqual({ok, 5}, impala_json:to_json(TypeInfo, RangeType, 5)),
    ?assertEqual({ok, 5}, impala_json:from_json(TypeInfo, RangeType, 5)),

    %% Test out of range values
    ?assertMatch({error, _}, impala_json:to_json(TypeInfo, RangeType, 15)),
    ?assertMatch({error, _}, impala_json:from_json(TypeInfo, RangeType, 15)).

%% Test nonempty list im_types (extracted from types)
nonempty_list_im_types_test() ->
    TypeInfo = impala_abstract_code:types_in_module(?MODULE),

    %% Test nonempty list
    {ok, NonEmptyListType} = impala_type_info:get_type(TypeInfo, my_nonempty_list, 0),
    ?assertEqual({ok, [1, 2, 3]}, impala_json:to_json(TypeInfo, NonEmptyListType, [1, 2, 3])),
    ?assertEqual({ok, [1, 2, 3]},
                 impala_json:from_json(TypeInfo, NonEmptyListType, [1, 2, 3])),

    %% Test empty list should fail
    ?assertMatch({error, _}, impala_json:to_json(TypeInfo, NonEmptyListType, [])),
    ?assertMatch({error, _}, impala_json:from_json(TypeInfo, NonEmptyListType, [])).

%% Test with type info from different module
cross_module_im_types_test() ->
    %% Use impala_json_schema_test module types with this module's TypeInfo
    OtherModuleTypeInfo = impala_abstract_code:types_in_module(impala_json_schema_test),

    %% Get a type from the other module and use it
    {ok, MyIntegerType} = impala_type_info:get_type(OtherModuleTypeInfo, my_integer, 0),
    ?assertEqual({ok, 42}, impala_json:to_json(OtherModuleTypeInfo, MyIntegerType, 42)),
    ?assertEqual({ok, 42}, impala_json:from_json(OtherModuleTypeInfo, MyIntegerType, 42)).

%% Test passing TypeInfo vs passing module directly
typeinfo_vs_module_test() ->
    TypeInfo = impala_abstract_code:types_in_module(?MODULE),
    {ok, IntegerType} = impala_type_info:get_type(TypeInfo, my_integer, 0),

    %% These two should give the same result
    Result1 = impala_json:to_json(?MODULE, IntegerType, 42),
    Result2 = impala_json:to_json(TypeInfo, IntegerType, 42),
    ?assertEqual(Result1, Result2),
    ?assertEqual({ok, 42}, Result1),

    %% Same for from_json
    JsonResult1 = impala_json:from_json(?MODULE, IntegerType, 42),
    JsonResult2 = impala_json:from_json(TypeInfo, IntegerType, 42),
    ?assertEqual(JsonResult1, JsonResult2),
    ?assertEqual({ok, 42}, JsonResult1).
