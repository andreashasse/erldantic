-module(json_schema_validation_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

-type user_id() :: pos_integer().
-type user_name() :: string().
-type user_email() :: binary().

-record(user_profile, {
    id :: user_id(), name :: user_name(), email :: user_email(), age :: integer()
}).

-type user_profile() :: #user_profile{}.

-type user_settings() ::
    #{
        theme := dark | light,
        notifications := boolean(),
        max_items := pos_integer(),
        tags => [string()],
        metadata => #{version := string(), created_at := string()}
    }.

-type binary_key_map() :: #{binary() => integer()}.

simple_type_validation_test() ->
    {ok, IntSchema} = spectra_json_schema:to_schema(?MODULE, {type, user_id, 0}),
    JesseSchema = json:decode(iolist_to_binary(json:encode(IntSchema))),
    ?assertEqual({ok, 123}, jesse:validate_with_schema(JesseSchema, 123)),
    ?assertMatch({error, _}, jesse:validate_with_schema(JesseSchema, -1)),
    ?assertMatch({error, _}, jesse:validate_with_schema(JesseSchema, <<"not_an_integer">>)).

string_type_validation_test() ->
    {ok, StringSchema} = spectra_json_schema:to_schema(?MODULE, {type, user_name, 0}),
    JesseSchema = json:decode(iolist_to_binary(json:encode(StringSchema))),
    ?assertEqual({ok, <<"John Doe">>}, jesse:validate_with_schema(JesseSchema, <<"John Doe">>)),
    ?assertMatch({error, _}, jesse:validate_with_schema(JesseSchema, 123)).

binary_type_validation_test() ->
    {ok, BinarySchema} = spectra_json_schema:to_schema(?MODULE, {type, user_email, 0}),
    JesseSchema = json:decode(iolist_to_binary(json:encode(BinarySchema))),
    ?assertEqual(
        {ok, <<"user@example.com">>},
        jesse:validate_with_schema(JesseSchema, <<"user@example.com">>)
    ),
    ?assertMatch({error, _}, jesse:validate_with_schema(JesseSchema, 123)).

record_validation_test() ->
    {ok, RecordSchema} = spectra_json_schema:to_schema(?MODULE, {record, user_profile}),
    JesseSchema = json:decode(iolist_to_binary(json:encode(RecordSchema))),
    ValidData =
        #{
            <<"id">> => 1,
            <<"name">> => <<"John Doe">>,
            <<"email">> => <<"john@example.com">>,
            <<"age">> => 30
        },
    ?assertEqual({ok, ValidData}, jesse:validate_with_schema(JesseSchema, ValidData)),
    ?assertMatch(
        {error, _},
        jesse:validate_with_schema(
            JesseSchema,
            #{
                <<"name">> => <<"John Doe">>,
                <<"email">> => <<"john@example.com">>,
                <<"age">> => 30
            }
        )
    ),
    ?assertMatch(
        {error, _},
        jesse:validate_with_schema(
            JesseSchema,
            #{
                <<"id">> => <<"not_an_integer">>,
                <<"name">> => <<"John Doe">>,
                <<"email">> => <<"john@example.com">>,
                <<"age">> => 30
            }
        )
    ).

complex_map_validation_test() ->
    {ok, MapSchema} = spectra_json_schema:to_schema(?MODULE, {type, user_settings, 0}),
    JesseSchema = json:decode(iolist_to_binary(json:encode(MapSchema))),
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
    ?assertMatch(
        {error, _},
        jesse:validate_with_schema(
            JesseSchema,
            #{<<"notifications">> => true, <<"max_items">> => 25}
        )
    ),
    ?assertMatch(
        {error, _},
        jesse:validate_with_schema(
            JesseSchema,
            #{
                <<"theme">> => <<"blue">>,
                <<"notifications">> => true,
                <<"max_items">> => 25
            }
        )
    ),
    ?assertMatch(
        {error, _},
        jesse:validate_with_schema(
            JesseSchema,
            #{
                <<"theme">> => <<"dark">>,
                <<"notifications">> => true,
                <<"max_items">> => -5
            }
        )
    ),
    ?assertMatch(
        {error, _},
        jesse:validate_with_schema(
            JesseSchema,
            #{
                <<"theme">> => <<"light">>,
                <<"notifications">> => false,
                <<"max_items">> => 20,
                <<"tags">> => [123, 456]
            }
        )
    ).
