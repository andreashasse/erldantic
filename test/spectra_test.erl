-module(spectra_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-compile(nowarn_unused_type).

%% Test types with ranges
-type user_id() :: pos_integer().
-type age() :: 0..150.
-type port_number() :: 1..65535.
-type status() :: active | inactive | pending.
%% Test types with non-empty lists
-type tags() :: [string(), ...].

%% Test records
-record(user, {id :: user_id(), name :: binary(), age :: age()}).
-record(post, {id :: pos_integer(), title :: binary(), tags :: tags()}).

%% Test map types
-type user_map() ::
    #{
        id := user_id(),
        name := binary(),
        age := age()
    }.
%% Map with optional fields
-type config_map() ::
    #{
        host := string(),
        port := port_number(),
        timeout => pos_integer()
    }.
%% Nested map with non-empty list
-type group_map() :: #{name := string(), members := [user_id(), ...]}.
%% Nested maps
-type nested_map() :: #{user := user_map(), tags := tags()}.

%%====================================================================
%% decode_type/4 tests - JSON format
%%====================================================================

decode_type_json_simple_test() ->
    % Simple integer type
    Json = <<"123">>,
    ?assertEqual({ok, 123}, spectra:decode(json, ?MODULE, user_id, Json)),

    % Atom/enum type
    JsonAtom = <<"\"active\"">>,
    ?assertEqual({ok, active}, spectra:decode(json, ?MODULE, status, JsonAtom)).

decode_type_json_range_test() ->
    % Valid age within range
    ?assertEqual({ok, 25}, spectra:decode(json, ?MODULE, age, <<"25">>)),
    ?assertEqual({ok, 0}, spectra:decode(json, ?MODULE, age, <<"0">>)),
    ?assertEqual({ok, 150}, spectra:decode(json, ?MODULE, age, <<"150">>)),

    % Invalid age - out of range
    {error, Errors1} = spectra:decode(json, ?MODULE, age, <<"151">>),
    ?assertMatch([#sp_error{type = type_mismatch}], Errors1),

    % Valid port in range
    ?assertEqual({ok, 8080}, spectra:decode(json, ?MODULE, port_number, <<"8080">>)),

    % Invalid port - out of range
    {error, Errors2} = spectra:decode(json, ?MODULE, port_number, <<"70000">>),
    ?assertMatch([#sp_error{type = type_mismatch}], Errors2).

decode_type_json_nonempty_list_test() ->
    % Valid non-empty list
    JsonValid = <<"[\"tag1\",\"tag2\",\"tag3\"]">>,
    ?assertEqual(
        {ok, ["tag1", "tag2", "tag3"]},
        spectra:decode(json, ?MODULE, tags, JsonValid)
    ),

    % Single element list
    JsonSingle = <<"[\"single\"]">>,
    ?assertEqual({ok, ["single"]}, spectra:decode(json, ?MODULE, tags, JsonSingle)),

    % Empty list should fail
    JsonEmpty = <<"[]">>,
    {error, Errors} = spectra:decode(json, ?MODULE, tags, JsonEmpty),
    ?assertMatch([#sp_error{type = type_mismatch}], Errors).

decode_type_json_map_test() ->
    % Simple map with all required fields
    Json = <<"{\"id\":1,\"name\":\"Alice\",\"age\":30}">>,
    Expected =
        #{
            id => 1,
            name => <<"Alice">>,
            age => 30
        },
    ?assertEqual({ok, Expected}, spectra:decode(json, ?MODULE, user_map, Json)).

decode_type_json_map_with_optional_test() ->
    % Config map with all fields
    JsonAll = <<"{\"host\":\"localhost\",\"port\":8080,\"timeout\":5000}">>,
    ExpectedAll =
        #{
            host => "localhost",
            port => 8080,
            timeout => 5000
        },
    ?assertEqual({ok, ExpectedAll}, spectra:decode(json, ?MODULE, config_map, JsonAll)),

    % Config map with only required fields
    JsonRequired = <<"{\"host\":\"localhost\",\"port\":8080}">>,
    ExpectedRequired = #{host => "localhost", port => 8080},
    ?assertEqual(
        {ok, ExpectedRequired},
        spectra:decode(json, ?MODULE, config_map, JsonRequired)
    ).

decode_type_json_map_with_nonempty_list_test() ->
    % Group with non-empty members list
    JsonGroup = <<"{\"name\":\"Team A\",\"members\":[1,2,3]}">>,
    ExpectedGroup = #{name => "Team A", members => [1, 2, 3]},
    ?assertEqual({ok, ExpectedGroup}, spectra:decode(json, ?MODULE, group_map, JsonGroup)),

    % Empty members should fail
    JsonEmptyMembers = <<"{\"name\":\"Team B\",\"members\":[]}">>,
    {error, MemberErrors} = spectra:decode(json, ?MODULE, group_map, JsonEmptyMembers),
    ?assertMatch([#sp_error{type = type_mismatch}], MemberErrors).

decode_type_json_nested_map_test() ->
    % Nested maps with non-empty list
    Json =
        <<"{\"user\":{\"id\":1,\"name\":\"Alice\",\"age\":30},\"tags\":[\"tag1\",\"tag2\"]}">>,
    Expected =
        #{
            user =>
                #{
                    id => 1,
                    name => <<"Alice">>,
                    age => 30
                },
            tags => ["tag1", "tag2"]
        },
    ?assertEqual({ok, Expected}, spectra:decode(json, ?MODULE, nested_map, Json)).

decode_type_json_error_test() ->
    % Invalid value - negative number for pos_integer
    JsonInvalid = <<"-5">>,
    {error, Errors} = spectra:decode(json, ?MODULE, user_id, JsonInvalid),
    ?assertMatch([#sp_error{type = type_mismatch}], Errors).

%%====================================================================
%% decode_record/4 tests - JSON format
%%====================================================================

decode_record_json_test() ->
    Json = <<"{\"id\":42,\"name\":\"Bob\",\"age\":25}">>,
    Expected =
        #user{
            id = 42,
            name = <<"Bob">>,
            age = 25
        },
    ?assertEqual({ok, Expected}, spectra:decode(json, ?MODULE, user, Json)).

decode_record_json_with_nonempty_list_test() ->
    Json = <<"{\"id\":1,\"title\":\"First Post\",\"tags\":[\"erlang\",\"testing\"]}">>,
    Expected =
        #post{
            id = 1,
            title = <<"First Post">>,
            tags = ["erlang", "testing"]
        },
    ?assertEqual({ok, Expected}, spectra:decode(json, ?MODULE, post, Json)).

decode_record_json_error_missing_field_test() ->
    % Missing required field 'age'
    JsonMissing = <<"{\"id\":42,\"name\":\"Bob\"}">>,
    {error, Errors} = spectra:decode(json, ?MODULE, user, JsonMissing),
    ?assertMatch([#sp_error{}], Errors).

%%====================================================================
%% encode_type/4 tests - JSON format
%%====================================================================

encode_type_json_simple_test() ->
    ?assertEqual({ok, <<"123">>}, spectra:encode(json, ?MODULE, user_id, 123)),

    % json:encode returns iolist, need to convert to binary for comparison
    {ok, StatusJson} = spectra:encode(json, ?MODULE, status, active),
    ?assertEqual(<<"\"active\"">>, iolist_to_binary(StatusJson)).

encode_type_json_range_test() ->
    % Valid age
    {ok, AgeJson} = spectra:encode(json, ?MODULE, age, 30),
    ?assertEqual(<<"30">>, iolist_to_binary(AgeJson)),

    % Out of range should error
    {error, Errors} = spectra:encode(json, ?MODULE, age, 200),
    ?assertMatch([#sp_error{type = type_mismatch}], Errors).

encode_type_json_nonempty_list_test() ->
    % Valid non-empty list
    {ok, JsonIoList} = spectra:encode(json, ?MODULE, tags, ["tag1", "tag2"]),
    JsonBinary = iolist_to_binary(JsonIoList),
    {ok, Decoded} = spectra:decode(json, ?MODULE, tags, JsonBinary),
    ?assertEqual(["tag1", "tag2"], Decoded),

    % Empty list should error
    {error, Errors} = spectra:encode(json, ?MODULE, tags, []),
    ?assertMatch([#sp_error{type = type_mismatch}], Errors).

encode_type_json_map_test() ->
    Data =
        #{
            id => 1,
            name => <<"Alice">>,
            age => 30
        },
    {ok, JsonIoList} = spectra:encode(json, ?MODULE, user_map, Data),
    JsonBinary = iolist_to_binary(JsonIoList),

    % Decode it back to verify it's valid JSON
    {ok, Decoded} = spectra:decode(json, ?MODULE, user_map, JsonBinary),
    ?assertEqual(Data, Decoded).

encode_type_json_map_with_optional_test() ->
    % With optional field
    DataWith =
        #{
            host => "localhost",
            port => 8080,
            timeout => 5000
        },
    {ok, JsonWithIoList} = spectra:encode(json, ?MODULE, config_map, DataWith),
    JsonWith = iolist_to_binary(JsonWithIoList),
    {ok, DecodedWith} = spectra:decode(json, ?MODULE, config_map, JsonWith),
    ?assertEqual(DataWith, DecodedWith),

    % Without optional field
    DataWithout = #{host => "localhost", port => 8080},
    {ok, JsonWithoutIoList} = spectra:encode(json, ?MODULE, config_map, DataWithout),
    JsonWithout = iolist_to_binary(JsonWithoutIoList),
    {ok, DecodedWithout} = spectra:decode(json, ?MODULE, config_map, JsonWithout),
    ?assertEqual(DataWithout, DecodedWithout).

encode_type_json_nested_map_test() ->
    Data =
        #{
            user =>
                #{
                    id => 1,
                    name => <<"Alice">>,
                    age => 30
                },
            tags => ["tag1", "tag2"]
        },
    {ok, JsonIoList} = spectra:encode(json, ?MODULE, nested_map, Data),
    JsonBinary = iolist_to_binary(JsonIoList),
    {ok, Decoded} = spectra:decode(json, ?MODULE, nested_map, JsonBinary),
    ?assertEqual(Data, Decoded).

encode_type_json_error_test() ->
    % Invalid data - atom that doesn't match status type
    {error, Errors} = spectra:encode(json, ?MODULE, status, invalid_status),
    ?assertMatch([#sp_error{type = no_match}], Errors).

%%====================================================================
%% encode_record/4 tests - JSON format
%%====================================================================

encode_record_json_test() ->
    User =
        #user{
            id = 42,
            name = <<"Bob">>,
            age = 25
        },
    {ok, JsonIoList} = spectra:encode(json, ?MODULE, user, User),
    JsonBinary = iolist_to_binary(JsonIoList),

    % Decode it back to verify it's valid
    {ok, Decoded} = spectra:decode(json, ?MODULE, user, JsonBinary),
    ?assertEqual(User, Decoded).

encode_record_json_with_nonempty_list_test() ->
    Post =
        #post{
            id = 1,
            title = <<"First Post">>,
            tags = ["erlang", "testing"]
        },
    {ok, JsonIoList} = spectra:encode(json, ?MODULE, post, Post),
    JsonBinary = iolist_to_binary(JsonIoList),
    {ok, Decoded} = spectra:decode(json, ?MODULE, post, JsonBinary),
    ?assertEqual(Post, Decoded).

%%====================================================================
%% decode/4 and encode/4 tests with sp_type_reference
%%====================================================================

decode_with_type_reference_test() ->
    % Test using {type, Name, Arity} reference
    Json = <<"123">>,
    ?assertEqual({ok, 123}, spectra:decode(json, ?MODULE, {type, user_id, 0}, Json)),

    % Test using {record, Name} reference
    RecordJson = <<"{\"id\":42,\"name\":\"Bob\",\"age\":25}">>,
    Expected =
        #user{
            id = 42,
            name = <<"Bob">>,
            age = 25
        },
    ?assertEqual({ok, Expected}, spectra:decode(json, ?MODULE, {record, user}, RecordJson)).

encode_with_type_reference_test() ->
    % Test using {type, Name, Arity} reference
    ?assertEqual({ok, <<"123">>}, spectra:encode(json, ?MODULE, {type, user_id, 0}, 123)),

    % Test using {record, Name} reference
    User =
        #user{
            id = 42,
            name = <<"Bob">>,
            age = 25
        },
    {ok, JsonIoList} = spectra:encode(json, ?MODULE, {record, user}, User),
    JsonBinary = iolist_to_binary(JsonIoList),
    {ok, Decoded} = spectra:decode(json, ?MODULE, {record, user}, JsonBinary),
    ?assertEqual(User, Decoded).

%%====================================================================
%% Module vs type_info tests
%%====================================================================

decode_with_type_info_test() ->
    % Get type info once and reuse it
    TypeInfo = spectra_module_types:get(?MODULE),

    % Use it for multiple operations
    Json = <<"123">>,
    ?assertEqual({ok, 123}, spectra:decode(json, TypeInfo, user_id, Json)),

    MapJson = <<"{\"id\":1,\"name\":\"Alice\",\"age\":30}">>,
    Expected =
        #{
            id => 1,
            name => <<"Alice">>,
            age => 30
        },
    ?assertEqual({ok, Expected}, spectra:decode(json, TypeInfo, user_map, MapJson)).

encode_with_type_info_test() ->
    % Get type info once and reuse it
    TypeInfo = spectra_module_types:get(?MODULE),

    % Use it for multiple operations
    ?assertEqual({ok, <<"123">>}, spectra:encode(json, TypeInfo, user_id, 123)),

    Data =
        #{
            id => 1,
            name => <<"Alice">>,
            age => 30
        },
    {ok, JsonIoList} = spectra:encode(json, TypeInfo, user_map, Data),
    JsonBinary = iolist_to_binary(JsonIoList),
    {ok, Decoded} = spectra:decode(json, TypeInfo, user_map, JsonBinary),
    ?assertEqual(Data, Decoded).

%%====================================================================
%% Round-trip tests
%%====================================================================

round_trip_json_type_test() ->
    % Test simple type
    Data1 = 42,
    {ok, Json1IoList} = spectra:encode(json, ?MODULE, user_id, Data1),
    Json1 = iolist_to_binary(Json1IoList),
    {ok, Result1} = spectra:decode(json, ?MODULE, user_id, Json1),
    ?assertEqual(Data1, Result1),

    % Test enum type
    Data2 = active,
    {ok, Json2IoList} = spectra:encode(json, ?MODULE, status, Data2),
    Json2 = iolist_to_binary(Json2IoList),
    {ok, Result2} = spectra:decode(json, ?MODULE, status, Json2),
    ?assertEqual(Data2, Result2),

    % Test map type
    Data3 =
        #{
            id => 1,
            name => <<"Test">>,
            age => 30
        },
    {ok, Json3IoList} = spectra:encode(json, ?MODULE, user_map, Data3),
    Json3 = iolist_to_binary(Json3IoList),
    {ok, Result3} = spectra:decode(json, ?MODULE, user_map, Json3),
    ?assertEqual(Data3, Result3).

round_trip_json_record_test() ->
    User =
        #user{
            id = 99,
            name = <<"Charlie">>,
            age = 30
        },
    {ok, JsonIoList} = spectra:encode(json, ?MODULE, user, User),
    JsonBinary = iolist_to_binary(JsonIoList),
    {ok, Result} = spectra:decode(json, ?MODULE, user, JsonBinary),
    ?assertEqual(User, Result).

%%====================================================================
%% Error handling tests
%%====================================================================

error_handling_decode_test() ->
    % Type mismatch - string where integer expected
    JsonString = <<"\"not_a_number\"">>,
    {error, Errors1} = spectra:decode(json, ?MODULE, user_id, JsonString),
    ?assertMatch([#sp_error{type = type_mismatch}], Errors1),

    % Invalid enum value
    JsonInvalidStatus = <<"\"invalid\"">>,
    {error, Errors2} = spectra:decode(json, ?MODULE, status, JsonInvalidStatus),
    ?assertMatch([#sp_error{type = no_match}], Errors2),

    % Missing required field
    JsonMissingField = <<"{\"id\":1,\"name\":\"Alice\"}">>,
    {error, Errors3} = spectra:decode(json, ?MODULE, user_map, JsonMissingField),
    ?assertMatch([#sp_error{}], Errors3).

error_handling_invalid_json_test() ->
    InvalidJson1 = <<"{\"id\":1,\"name\":\"Alice\"">>,
    {error, Errors1} = spectra:decode(json, ?MODULE, user_map, InvalidJson1),
    ?assertMatch([#sp_error{type = decode_error, location = []}], Errors1),

    InvalidJson2 = <<"{invalid}">>,
    {error, Errors2} = spectra:decode(json, ?MODULE, user_map, InvalidJson2),
    ?assertMatch([#sp_error{type = decode_error, location = []}], Errors2),

    InvalidJson3 = <<"{\"id\":">>,
    {error, Errors3} = spectra:decode(json, ?MODULE, user_id, InvalidJson3),
    ?assertMatch([#sp_error{type = decode_error, location = []}], Errors3),

    InvalidJson4 = <<"this is not json">>,
    {error, Errors4} = spectra:decode(json, ?MODULE, user_id, InvalidJson4),
    ?assertMatch([#sp_error{type = decode_error, location = []}], Errors4),

    InvalidJson5 = <<"">>,
    {error, Errors5} = spectra:decode(json, ?MODULE, user_id, InvalidJson5),
    ?assertMatch([#sp_error{type = decode_error, location = []}], Errors5).

error_handling_encode_test() ->
    % Wrong type for status
    {error, Errors1} = spectra:encode(json, ?MODULE, status, wrong_atom),
    ?assertMatch([#sp_error{type = no_match}], Errors1),

    % Negative number for pos_integer
    {error, Errors2} = spectra:encode(json, ?MODULE, user_id, -5),
    ?assertMatch([#sp_error{type = type_mismatch}], Errors2),

    % Missing required field in map

    % missing age
    InvalidMap = #{id => 1, name => <<"Alice">>},
    {error, Errors3} = spectra:encode(json, ?MODULE, user_map, InvalidMap),
    ?assertMatch([#sp_error{}], Errors3).

%%====================================================================
%% API consistency tests
%%====================================================================

api_consistency_decode_test() ->
    % Verify decode_type is equivalent to decode with {type, _, 0}
    Json = <<"123">>,
    Result1 = spectra:decode(json, ?MODULE, user_id, Json),
    Result2 = spectra:decode(json, ?MODULE, {type, user_id, 0}, Json),
    ?assertEqual(Result1, Result2),

    % Verify decode_record is equivalent to decode with {record, _}
    RecordJson = <<"{\"id\":42,\"name\":\"Bob\",\"age\":25}">>,
    Result3 = spectra:decode(json, ?MODULE, user, RecordJson),
    Result4 = spectra:decode(json, ?MODULE, {record, user}, RecordJson),
    ?assertEqual(Result3, Result4).

api_consistency_encode_test() ->
    % Verify encode_type is equivalent to encode with {type, _, 0}
    Result1 = spectra:encode(json, ?MODULE, user_id, 123),
    Result2 = spectra:encode(json, ?MODULE, {type, user_id, 0}, 123),
    ?assertEqual(Result1, Result2),

    % Verify encode_record is equivalent to encode with {record, _}
    User =
        #user{
            id = 42,
            name = <<"Bob">>,
            age = 25
        },
    Result3 = spectra:encode(json, ?MODULE, user, User),
    Result4 = spectra:encode(json, ?MODULE, {record, user}, User),
    ?assertEqual(Result3, Result4).

%%====================================================================
%% binary_string format tests
%%====================================================================

decode_binary_string_simple_types_test() ->
    % Integer
    ?assertEqual({ok, 123}, spectra:decode(binary_string, ?MODULE, user_id, <<"123">>)),

    % Age with range
    ?assertEqual({ok, 25}, spectra:decode(binary_string, ?MODULE, age, <<"25">>)),
    ?assertEqual({ok, 0}, spectra:decode(binary_string, ?MODULE, age, <<"0">>)),
    ?assertEqual({ok, 150}, spectra:decode(binary_string, ?MODULE, age, <<"150">>)),

    % Out of range
    {error, RangeErrors} = spectra:decode(binary_string, ?MODULE, age, <<"200">>),
    ?assertMatch([#sp_error{type = type_mismatch}], RangeErrors),

    % Port number
    ?assertEqual(
        {ok, 8080},
        spectra:decode(binary_string, ?MODULE, port_number, <<"8080">>)
    ),

    % Status (atom)
    ?assertEqual(
        {ok, active},
        spectra:decode(binary_string, ?MODULE, status, <<"active">>)
    ),
    ?assertEqual(
        {ok, inactive},
        spectra:decode(binary_string, ?MODULE, status, <<"inactive">>)
    ).

decode_binary_string_error_test() ->
    % Invalid integer
    {error, Errors1} = spectra:decode(binary_string, ?MODULE, user_id, <<"not_a_number">>),
    ?assertMatch([#sp_error{type = type_mismatch}], Errors1),

    % Invalid atom for status
    {error, Errors2} = spectra:decode(binary_string, ?MODULE, status, <<"invalid_status">>),
    ?assertMatch([#sp_error{type = no_match}], Errors2).

encode_binary_string_simple_types_test() ->
    % Integer
    ?assertEqual({ok, <<"123">>}, spectra:encode(binary_string, ?MODULE, user_id, 123)),

    % Age with range
    ?assertEqual({ok, <<"25">>}, spectra:encode(binary_string, ?MODULE, age, 25)),
    ?assertEqual({ok, <<"0">>}, spectra:encode(binary_string, ?MODULE, age, 0)),
    ?assertEqual({ok, <<"150">>}, spectra:encode(binary_string, ?MODULE, age, 150)),

    % Out of range
    {error, RangeErrors} = spectra:encode(binary_string, ?MODULE, age, 200),
    ?assertMatch([#sp_error{type = type_mismatch}], RangeErrors),

    % Port number
    ?assertEqual(
        {ok, <<"8080">>},
        spectra:encode(binary_string, ?MODULE, port_number, 8080)
    ),

    % Status (atom)
    ?assertEqual(
        {ok, <<"active">>},
        spectra:encode(binary_string, ?MODULE, status, active)
    ),
    ?assertEqual(
        {ok, <<"inactive">>},
        spectra:encode(binary_string, ?MODULE, status, inactive)
    ).

round_trip_binary_string_test() ->
    % Integer round trip
    Data1 = 42,
    {ok, Bin1} = spectra:encode(binary_string, ?MODULE, user_id, Data1),
    {ok, Result1} = spectra:decode(binary_string, ?MODULE, user_id, Bin1),
    ?assertEqual(Data1, Result1),

    % Age with range round trip
    Data2 = 30,
    {ok, Bin2} = spectra:encode(binary_string, ?MODULE, age, Data2),
    {ok, Result2} = spectra:decode(binary_string, ?MODULE, age, Bin2),
    ?assertEqual(Data2, Result2),

    % Atom round trip
    Data3 = pending,
    {ok, Bin3} = spectra:encode(binary_string, ?MODULE, status, Data3),
    {ok, Result3} = spectra:decode(binary_string, ?MODULE, status, Bin3),
    ?assertEqual(Data3, Result3).

%%====================================================================
%% string format tests
%%====================================================================

decode_string_simple_types_test() ->
    % Integer
    ?assertEqual({ok, 123}, spectra:decode(string, ?MODULE, user_id, "123")),

    % Age with range
    ?assertEqual({ok, 25}, spectra:decode(string, ?MODULE, age, "25")),
    ?assertEqual({ok, 0}, spectra:decode(string, ?MODULE, age, "0")),
    ?assertEqual({ok, 150}, spectra:decode(string, ?MODULE, age, "150")),

    % Out of range
    {error, RangeErrors} = spectra:decode(string, ?MODULE, age, "200"),
    ?assertMatch([#sp_error{type = type_mismatch}], RangeErrors),

    % Port number
    ?assertEqual({ok, 8080}, spectra:decode(string, ?MODULE, port_number, "8080")),

    % Status (atom)
    ?assertEqual({ok, active}, spectra:decode(string, ?MODULE, status, "active")),
    ?assertEqual({ok, inactive}, spectra:decode(string, ?MODULE, status, "inactive")).

decode_string_error_test() ->
    % Invalid integer
    {error, Errors1} = spectra:decode(string, ?MODULE, user_id, "not_a_number"),
    ?assertMatch([#sp_error{type = type_mismatch}], Errors1),

    % Invalid atom for status
    {error, Errors2} = spectra:decode(string, ?MODULE, status, "invalid_status"),
    ?assertMatch([#sp_error{type = no_match}], Errors2).

encode_string_simple_types_test() ->
    % Integer
    ?assertEqual({ok, "123"}, spectra:encode(string, ?MODULE, user_id, 123)),

    % Age with range
    ?assertEqual({ok, "25"}, spectra:encode(string, ?MODULE, age, 25)),
    ?assertEqual({ok, "0"}, spectra:encode(string, ?MODULE, age, 0)),
    ?assertEqual({ok, "150"}, spectra:encode(string, ?MODULE, age, 150)),

    % Out of range
    {error, RangeErrors} = spectra:encode(string, ?MODULE, age, 200),
    ?assertMatch([#sp_error{type = type_mismatch}], RangeErrors),

    % Port number
    ?assertEqual({ok, "8080"}, spectra:encode(string, ?MODULE, port_number, 8080)),

    % Status (atom)
    ?assertEqual({ok, "active"}, spectra:encode(string, ?MODULE, status, active)),
    ?assertEqual({ok, "inactive"}, spectra:encode(string, ?MODULE, status, inactive)).

round_trip_string_test() ->
    % Integer round trip
    Data1 = 42,
    {ok, Str1} = spectra:encode(string, ?MODULE, user_id, Data1),
    {ok, Result1} = spectra:decode(string, ?MODULE, user_id, Str1),
    ?assertEqual(Data1, Result1),

    % Age with range round trip
    Data2 = 30,
    {ok, Str2} = spectra:encode(string, ?MODULE, age, Data2),
    {ok, Result2} = spectra:decode(string, ?MODULE, age, Str2),
    ?assertEqual(Data2, Result2),

    % Atom round trip
    Data3 = pending,
    {ok, Str3} = spectra:encode(string, ?MODULE, status, Data3),
    {ok, Result3} = spectra:decode(string, ?MODULE, status, Str3),
    ?assertEqual(Data3, Result3).
