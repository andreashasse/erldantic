-module(json_schema_validation_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

%% Test types for Jesse validation
-type user_id() :: pos_integer().
-type user_name() :: string().
-type user_email() :: binary().

-record(user_profile, {
    id :: user_id(), name :: user_name(), email :: user_email(), age :: integer()
}).

-type user_profile() :: #user_profile{}.
%% Complex map type with various constraints
-type user_settings() ::
    #{
        theme := dark | light,
        notifications := boolean(),
        max_items := pos_integer(),
        tags => [string()],
        metadata => #{version := string(), created_at := string()}
    }.
%% Test type with binary keys - this should fail
-type binary_key_map() :: #{binary() => integer()}.

%% Tests
simple_type_validation_test() ->
    %% Test integer type validation
    {ok, IntSchema} = erldantic_json_schema:to_schema(?MODULE, {type, user_id, 0}),
    %% Convert to Jesse-compatible format by encoding then decoding
    JesseSchema = json:decode(iolist_to_binary(json:encode(IntSchema))),

    %% Valid positive integer
    ?assertEqual({ok, 123}, jesse:validate_with_schema(JesseSchema, 123)),

    %% Invalid negative integer (pos_integer should only allow positive)
    Result1 = jesse:validate_with_schema(JesseSchema, -1),
    ?assertMatch({error, _}, Result1),

    %% Invalid type (string instead of integer)
    Result2 = jesse:validate_with_schema(JesseSchema, <<"not_an_integer">>),
    ?assertMatch({error, _}, Result2).

string_type_validation_test() ->
    %% Test string type validation
    {ok, StringSchema} = erldantic_json_schema:to_schema(?MODULE, {type, user_name, 0}),
    JesseSchema = json:decode(iolist_to_binary(json:encode(StringSchema))),

    %% Valid string
    ?assertEqual(
        {ok, <<"John Doe">>},
        jesse:validate_with_schema(JesseSchema, <<"John Doe">>)
    ),

    %% Invalid type (integer instead of string)
    Result = jesse:validate_with_schema(JesseSchema, 123),
    ?assertMatch({error, _}, Result).

binary_type_validation_test() ->
    %% Test binary type validation
    {ok, BinarySchema} = erldantic_json_schema:to_schema(?MODULE, {type, user_email, 0}),
    JesseSchema = json:decode(iolist_to_binary(json:encode(BinarySchema))),

    %% Valid binary string
    ?assertEqual(
        {ok, <<"user@example.com">>},
        jesse:validate_with_schema(JesseSchema, <<"user@example.com">>)
    ),

    %% Invalid type (integer instead of string/binary)
    Result = jesse:validate_with_schema(JesseSchema, 123),
    ?assertMatch({error, _}, Result).

record_validation_test() ->
    %% Test record schema validation
    {ok, RecordSchema} = erldantic_json_schema:to_schema(?MODULE, {record, user_profile}),
    JesseSchema = json:decode(iolist_to_binary(json:encode(RecordSchema))),

    %% Valid record data - use binary keys for Jesse
    ValidData =
        #{
            <<"id">> => 1,
            <<"name">> => <<"John Doe">>,
            <<"email">> => <<"john@example.com">>,
            <<"age">> => 30
        },
    ?assertEqual({ok, ValidData}, jesse:validate_with_schema(JesseSchema, ValidData)),

    %% Missing required field
    InvalidData1 =
        #{
            <<"name">> => <<"John Doe">>,
            <<"email">> => <<"john@example.com">>,
            <<"age">> => 30
        },
    %% missing id
    Result1 = jesse:validate_with_schema(JesseSchema, InvalidData1),
    ?assertMatch({error, _}, Result1),

    %% Wrong type for field
    InvalidData2 =
        %% Should be integer
        #{
            <<"id">> => <<"not_an_integer">>,
            <<"name">> => <<"John Doe">>,
            <<"email">> => <<"john@example.com">>,
            <<"age">> => 30
        },
    Result2 = jesse:validate_with_schema(JesseSchema, InvalidData2),
    ?assertMatch({error, _}, Result2).

complex_map_validation_test() ->
    %% Test complex map schema validation
    {ok, MapSchema} = erldantic_json_schema:to_schema(?MODULE, {type, user_settings, 0}),
    JesseSchema = json:decode(iolist_to_binary(json:encode(MapSchema))),

    %% Valid complex map data with all required and optional fields
    ValidCompleteData =
        #{
            <<"theme">> => <<"dark">>,
            <<"notifications">> => true,
            <<"max_items">> => 50,
            <<"tags">> => [<<"erlang">>, <<"json">>, <<"api">>],
            <<"metadata">> => #{<<"version">> => <<"1.0">>, <<"created_at">> => <<"2023-01-01">>}
        },
    ?assertEqual(
        {ok, ValidCompleteData},
        jesse:validate_with_schema(JesseSchema, ValidCompleteData)
    ),

    %% Valid data with only required fields
    ValidMinimalData =
        #{
            <<"theme">> => <<"light">>,
            <<"notifications">> => false,
            <<"max_items">> => 10
        },
    ?assertEqual(
        {ok, ValidMinimalData},
        jesse:validate_with_schema(JesseSchema, ValidMinimalData)
    ),

    %% Invalid - missing required field 'theme'
    InvalidData1 = #{<<"notifications">> => true, <<"max_items">> => 25},
    Result1 = jesse:validate_with_schema(JesseSchema, InvalidData1),
    ?assertMatch({error, _}, Result1),

    %% Invalid - wrong type for 'theme' (should be dark or light)
    InvalidData2 =
        %% Invalid enum value
        #{
            <<"theme">> => <<"blue">>,
            <<"notifications">> => true,
            <<"max_items">> => 25
        },
    Result2 = jesse:validate_with_schema(JesseSchema, InvalidData2),
    ?assertMatch({error, _}, Result2),

    %% Invalid - negative max_items (should be pos_integer)
    InvalidData3 =
        #{
            <<"theme">> => <<"dark">>,
            <<"notifications">> => true,
            %% Should be positive
            <<"max_items">> => -5
        },
    Result3 = jesse:validate_with_schema(JesseSchema, InvalidData3),
    ?assertMatch({error, _}, Result3),

    %% Invalid - wrong type for tags array
    InvalidData4 =
        #{
            <<"theme">> => <<"light">>,
            <<"notifications">> => false,
            <<"max_items">> => 20,
            %% Should be strings
            <<"tags">> => [123, 456]
        },
    Result4 = jesse:validate_with_schema(JesseSchema, InvalidData4),
    ?assertMatch({error, _}, Result4).
