-module(integer_key_map_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-compile(nowarn_unused_type).

%% Test types with integer keys
-type int_literal_key_map() :: #{1 := binary(), 2 := binary()}.
-type int_literal_optional_map() :: #{1 => binary(), 2 => atom()}.
-type mixed_int_atom_keys() :: #{1 := binary(), foo := integer()}.
-type nested_int_key_map() :: #{outer := #{1 := binary(), 2 := integer()}}.

%%====================================================================
%% JSON Encoding Tests - Integer keys should become strings in JSON
%%====================================================================

encode_int_literal_key_test() ->
    ?assertEqual(
        {ok, #{<<"1">> => <<"value1">>, <<"2">> => <<"value2">>}},
        spectra_json:to_json(?MODULE, {type, int_literal_key_map, 0}, #{
            1 => <<"value1">>, 2 => <<"value2">>
        })
    ).

encode_int_literal_key_missing_required_test() ->
    ?assertMatch(
        {error, [#sp_error{location = [1], type = missing_data}]},
        spectra_json:to_json(?MODULE, {type, int_literal_key_map, 0}, #{2 => <<"value2">>})
    ),
    ?assertMatch(
        {error, [#sp_error{location = [2], type = missing_data}]},
        spectra_json:to_json(?MODULE, {type, int_literal_key_map, 0}, #{1 => <<"value1">>})
    ).

encode_int_literal_key_wrong_value_test() ->
    ?assertMatch(
        {error, [#sp_error{location = [1], type = type_mismatch}]},
        spectra_json:to_json(?MODULE, {type, int_literal_key_map, 0}, #{
            1 => not_a_binary, 2 => <<"value2">>
        })
    ).

encode_int_literal_optional_map_test() ->
    ?assertEqual(
        {ok, #{<<"1">> => <<"value1">>}},
        spectra_json:to_json(?MODULE, {type, int_literal_optional_map, 0}, #{1 => <<"value1">>})
    ),
    ?assertEqual(
        {ok, #{<<"2">> => some_atom}},
        spectra_json:to_json(?MODULE, {type, int_literal_optional_map, 0}, #{2 => some_atom})
    ),
    ?assertEqual(
        {ok, #{<<"1">> => <<"value1">>, <<"2">> => other_atom}},
        spectra_json:to_json(?MODULE, {type, int_literal_optional_map, 0}, #{
            1 => <<"value1">>, 2 => other_atom
        })
    ).

encode_mixed_int_atom_keys_test() ->
    ?assertEqual(
        {ok, #{<<"1">> => <<"value1">>, <<"foo">> => 42}},
        spectra_json:to_json(?MODULE, {type, mixed_int_atom_keys, 0}, #{
            1 => <<"value1">>, foo => 42
        })
    ).

encode_nested_int_key_map_test() ->
    ?assertEqual(
        {ok, #{
            <<"outer">> =>
                #{
                    <<"1">> => <<"inner_value">>,
                    <<"2">> => 100
                }
        }},
        spectra_json:to_json(?MODULE, {type, nested_int_key_map, 0}, #{
            outer => #{1 => <<"inner_value">>, 2 => 100}
        })
    ).

%%====================================================================
%% JSON Decoding Tests - String keys should become integers
%%====================================================================

decode_int_literal_key_test() ->
    ?assertEqual(
        {ok, #{1 => <<"value1">>, 2 => <<"value2">>}},
        spectra_json:from_json(?MODULE, {type, int_literal_key_map, 0}, #{
            <<"1">> => <<"value1">>, <<"2">> => <<"value2">>
        })
    ).

decode_int_literal_key_missing_required_test() ->
    ?assertMatch(
        {error, [#sp_error{location = [1], type = missing_data}]},
        spectra_json:from_json(?MODULE, {type, int_literal_key_map, 0}, #{<<"2">> => <<"value2">>})
    ),
    ?assertMatch(
        {error, [#sp_error{location = [2], type = missing_data}]},
        spectra_json:from_json(?MODULE, {type, int_literal_key_map, 0}, #{<<"1">> => <<"value1">>})
    ).

decode_int_literal_key_wrong_value_test() ->
    ?assertMatch(
        {error, [#sp_error{location = [1], type = type_mismatch}]},
        spectra_json:from_json(?MODULE, {type, int_literal_key_map, 0}, #{
            <<"1">> => 123, <<"2">> => <<"value2">>
        })
    ).

decode_int_literal_key_wrong_key_test() ->
    ?assertMatch(
        {error, [#sp_error{location = [1], type = missing_data}]},
        spectra_json:from_json(?MODULE, {type, int_literal_key_map, 0}, #{
            <<"3">> => <<"value">>, <<"2">> => <<"value2">>
        })
    ).

decode_int_literal_optional_map_test() ->
    ?assertEqual(
        {ok, #{1 => <<"value1">>}},
        spectra_json:from_json(?MODULE, {type, int_literal_optional_map, 0}, #{
            <<"1">> => <<"value1">>
        })
    ),
    ?assertEqual(
        {ok, #{2 => some_atom}},
        spectra_json:from_json(?MODULE, {type, int_literal_optional_map, 0}, #{
            <<"2">> => <<"some_atom">>
        })
    ),
    ?assertEqual(
        {ok, #{1 => <<"value1">>, 2 => other_atom}},
        spectra_json:from_json(?MODULE, {type, int_literal_optional_map, 0}, #{
            <<"1">> => <<"value1">>, <<"2">> => <<"other_atom">>
        })
    ).

decode_mixed_int_atom_keys_test() ->
    ?assertEqual(
        {ok, #{1 => <<"value1">>, foo => 42}},
        spectra_json:from_json(?MODULE, {type, mixed_int_atom_keys, 0}, #{
            <<"1">> => <<"value1">>, <<"foo">> => 42
        })
    ).

decode_nested_int_key_map_test() ->
    ?assertEqual(
        {ok, #{outer => #{1 => <<"inner_value">>, 2 => 100}}},
        spectra_json:from_json(?MODULE, {type, nested_int_key_map, 0}, #{
            <<"outer">> =>
                #{
                    <<"1">> => <<"inner_value">>,
                    <<"2">> => 100
                }
        })
    ).

%%====================================================================
%% Round-trip Tests
%%====================================================================

round_trip_int_literal_key_test() ->
    Data = #{1 => <<"value1">>, 2 => <<"value2">>},
    {ok, Json} = spectra_json:to_json(?MODULE, {type, int_literal_key_map, 0}, Data),
    {ok, Decoded} = spectra_json:from_json(?MODULE, {type, int_literal_key_map, 0}, Json),
    ?assertEqual(Data, Decoded).

round_trip_mixed_int_atom_keys_test() ->
    Data = #{1 => <<"value1">>, foo => 42},
    {ok, Json} = spectra_json:to_json(?MODULE, {type, mixed_int_atom_keys, 0}, Data),
    {ok, Decoded} = spectra_json:from_json(?MODULE, {type, mixed_int_atom_keys, 0}, Json),
    ?assertEqual(Data, Decoded).

round_trip_nested_int_key_map_test() ->
    Data = #{outer => #{1 => <<"inner_value">>, 2 => 100}},
    {ok, Json} = spectra_json:to_json(?MODULE, {type, nested_int_key_map, 0}, Data),
    {ok, Decoded} = spectra_json:from_json(?MODULE, {type, nested_int_key_map, 0}, Json),
    ?assertEqual(Data, Decoded).

%%====================================================================
%% JSON Schema Tests - Integer keys should appear as strings
%%====================================================================

schema_int_literal_key_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, int_literal_key_map, 0}),
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{
                <<"1">> := #{type := <<"string">>},
                <<"2">> := #{type := <<"string">>}
            },
            required := _
        },
        Schema
    ),
    #{required := Required} = Schema,
    ?assertEqual([<<"1">>, <<"2">>], lists:sort(Required)).

schema_int_literal_optional_map_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {
        type,
        int_literal_optional_map,
        0
    }),
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{<<"1">> := _, <<"2">> := _}
        },
        Schema
    ),
    ?assertEqual(false, maps:is_key(required, Schema)).

schema_mixed_int_atom_keys_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, mixed_int_atom_keys, 0}),
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{
                <<"1">> := #{type := <<"string">>},
                <<"foo">> := #{type := <<"integer">>}
            },
            required := _
        },
        Schema
    ),
    #{required := Required} = Schema,
    ?assertEqual([<<"1">>, <<"foo">>], lists:sort(Required)).

schema_nested_int_key_map_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, nested_int_key_map, 0}),
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{<<"outer">> := _},
            required := [<<"outer">>]
        },
        Schema
    ),
    #{properties := #{<<"outer">> := OuterSchema}} = Schema,
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{<<"1">> := _, <<"2">> := _}
        },
        OuterSchema
    ).
