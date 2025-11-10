-module(iodata_iolist_test).

-include_lib("eunit/include/eunit.hrl").

-type my_iodata() :: iodata().
-type my_iolist() :: iolist().

-export_type([my_iodata/0, my_iolist/0]).

-record(iodata_record, {
    data :: iodata(),
    name :: binary()
}).

-record(iolist_record, {
    data :: iolist(),
    name :: binary()
}).

%%% iodata encoding tests %%%

iodata_binary_test() ->
    Binary = <<"hello">>,
    {ok, Encoded} = spectra:encode(json, ?MODULE, my_iodata, Binary),
    ?assertEqual(<<"\"hello\"">>, iolist_to_binary(Encoded)).

iodata_list_of_binaries_test() ->
    IoData = [<<"hello">>, <<" ">>, <<"world">>],
    {ok, Encoded} = spectra:encode(json, ?MODULE, my_iodata, IoData),
    ?assertEqual(<<"\"hello world\"">>, iolist_to_binary(Encoded)).

iodata_nested_list_test() ->
    IoData = [<<"a">>, [<<"b">>, [<<"c">>]]],
    {ok, Encoded} = spectra:encode(json, ?MODULE, my_iodata, IoData),
    ?assertEqual(<<"\"abc\"">>, iolist_to_binary(Encoded)).

iodata_with_bytes_test() ->
    IoData = [<<"hello">>, 32, <<"world">>],
    {ok, Encoded} = spectra:encode(json, ?MODULE, my_iodata, IoData),
    ?assertEqual(<<"\"hello world\"">>, iolist_to_binary(Encoded)).

iodata_in_record_test() ->
    Record = #iodata_record{
        data = [<<"test">>, 32, <<"data">>],
        name = <<"example">>
    },
    {ok, Encoded} = spectra:encode(json, ?MODULE, iodata_record, Record),
    Decoded = json:decode(iolist_to_binary(Encoded)),
    ?assertEqual(#{<<"data">> => <<"test data">>, <<"name">> => <<"example">>}, Decoded).

%%% iolist encoding tests %%%

iolist_simple_list_test() ->
    IoList = [<<"hello">>, <<" ">>, <<"world">>],
    {ok, Encoded} = spectra:encode(json, ?MODULE, my_iolist, IoList),
    ?assertEqual(<<"\"hello world\"">>, iolist_to_binary(Encoded)).

iolist_nested_test() ->
    IoList = [<<"a">>, [<<"b">>, [<<"c">>, <<"d">>]]],
    {ok, Encoded} = spectra:encode(json, ?MODULE, my_iolist, IoList),
    ?assertEqual(<<"\"abcd\"">>, iolist_to_binary(Encoded)).

iolist_with_bytes_test() ->
    IoList = [<<"hello">>, 32, [<<"nested">>, 32, <<"world">>]],
    {ok, Encoded} = spectra:encode(json, ?MODULE, my_iolist, IoList),
    ?assertEqual(<<"\"hello nested world\"">>, iolist_to_binary(Encoded)).

iolist_in_record_test() ->
    Record = #iolist_record{
        data = [<<"test">>, [32, <<"list">>]],
        name = <<"example">>
    },
    {ok, Encoded} = spectra:encode(json, ?MODULE, iolist_record, Record),
    Decoded = json:decode(iolist_to_binary(Encoded)),
    ?assertEqual(#{<<"data">> => <<"test list">>, <<"name">> => <<"example">>}, Decoded).

%%% iodata decoding tests %%%

iodata_decode_binary_test() ->
    Json = <<"\"hello world\"">>,
    {ok, Decoded} = spectra:decode(json, ?MODULE, my_iodata, Json),
    ?assertEqual(<<"hello world">>, Decoded).

iodata_decode_from_record_test() ->
    Json = <<"{\"data\":\"test data\",\"name\":\"example\"}">>,
    {ok, Decoded} = spectra:decode(json, ?MODULE, iodata_record, Json),
    ?assertEqual(
        #iodata_record{data = <<"test data">>, name = <<"example">>},
        Decoded
    ).

%%% iolist decoding tests %%%

iolist_decode_binary_test() ->
    Json = <<"\"hello world\"">>,
    {ok, Decoded} = spectra:decode(json, ?MODULE, my_iolist, Json),
    % iolist from JSON wraps binary in a list
    ?assertEqual([<<"hello world">>], Decoded).

iolist_decode_from_record_test() ->
    Json = <<"{\"data\":\"test list\",\"name\":\"example\"}">>,
    {ok, Decoded} = spectra:decode(json, ?MODULE, iolist_record, Json),
    ?assertEqual(
        #iolist_record{data = [<<"test list">>], name = <<"example">>},
        Decoded
    ).

%%% Round-trip tests %%%

iodata_roundtrip_binary_test() ->
    Original = <<"hello">>,
    {ok, Encoded} = spectra:encode(json, ?MODULE, my_iodata, Original),
    EncodedBinary = iolist_to_binary(Encoded),
    {ok, Decoded} = spectra:decode(json, ?MODULE, my_iodata, EncodedBinary),
    ?assertEqual(Original, Decoded).

iodata_roundtrip_list_test() ->
    Original = [<<"hello">>, <<" ">>, <<"world">>],
    Expected = <<"hello world">>,
    {ok, Encoded} = spectra:encode(json, ?MODULE, my_iodata, Original),
    EncodedBinary = iolist_to_binary(Encoded),
    {ok, Decoded} = spectra:decode(json, ?MODULE, my_iodata, EncodedBinary),
    % After round-trip, iodata is normalized to binary
    ?assertEqual(Expected, Decoded).

iolist_roundtrip_test() ->
    Original = [<<"hello">>, [<<" ">>, <<"world">>]],
    Expected = [<<"hello world">>],
    {ok, Encoded} = spectra:encode(json, ?MODULE, my_iolist, Original),
    EncodedBinary = iolist_to_binary(Encoded),
    {ok, Decoded} = spectra:decode(json, ?MODULE, my_iolist, EncodedBinary),
    % After round-trip, iolist is normalized to [binary()]
    ?assertEqual(Expected, Decoded).

%%% Edge cases %%%

iodata_empty_binary_test() ->
    {ok, Encoded} = spectra:encode(json, ?MODULE, my_iodata, <<>>),
    EncodedBinary = iolist_to_binary(Encoded),
    ?assertEqual(<<"\"\"">>, EncodedBinary),
    {ok, Decoded} = spectra:decode(json, ?MODULE, my_iodata, EncodedBinary),
    ?assertEqual(<<>>, Decoded).

iolist_empty_list_test() ->
    {ok, Encoded} = spectra:encode(json, ?MODULE, my_iolist, []),
    EncodedBinary = iolist_to_binary(Encoded),
    ?assertEqual(<<"\"\"">>, EncodedBinary),
    {ok, Decoded} = spectra:decode(json, ?MODULE, my_iolist, EncodedBinary),
    ?assertEqual([<<>>], Decoded).

iodata_unicode_test() ->
    Unicode = <<"hello 世界"/utf8>>,
    {ok, Encoded} = spectra:encode(json, ?MODULE, my_iodata, Unicode),
    EncodedBinary = iolist_to_binary(Encoded),
    {ok, Decoded} = spectra:decode(json, ?MODULE, my_iodata, EncodedBinary),
    ?assertEqual(Unicode, Decoded).

iolist_unicode_test() ->
    UnicodeList = [<<"hello ">>, <<"世界"/utf8>>],
    {ok, Encoded} = spectra:encode(json, ?MODULE, my_iolist, UnicodeList),
    EncodedBinary = iolist_to_binary(Encoded),
    {ok, Decoded} = spectra:decode(json, ?MODULE, my_iolist, EncodedBinary),
    Expected = [<<"hello 世界"/utf8>>],
    ?assertEqual(Expected, Decoded).
