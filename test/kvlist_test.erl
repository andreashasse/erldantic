-module(kvlist_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").

%% Key-value list types with different key types
-type string_kvlist() :: [{string(), integer()}].
-type atom_kvlist() :: [{atom(), binary()}].
-type binary_kvlist() :: [{binary(), string()}].
-type integer_kvlist() :: [{integer(), atom()}].
-type float_kvlist() :: [{float(), binary()}].

%% Key-value lists with complex value types
-type kvlist_with_map_values() :: [{atom(), #{atom() => integer()}}].
-type kvlist_with_list_values() :: [{binary(), [integer()]}].
-type kvlist_with_nested_kvlist() :: [{string(), [{atom(), binary()}]}].

-export_type([
    string_kvlist/0,
    atom_kvlist/0,
    binary_kvlist/0,
    integer_kvlist/0,
    float_kvlist/0,
    kvlist_with_map_values/0,
    kvlist_with_list_values/0,
    kvlist_with_nested_kvlist/0
]).

%% =============================================================================
%% Tests for different key types
%% =============================================================================

%% String keys
encode_string_keys_test() ->
    Input = [{"key1", 1}, {"key2", 2}, {"key3", 3}],
    {ok, JsonBinary} = spectra:encode(json, ?MODULE, string_kvlist, Input),
    Decoded = json:decode(iolist_to_binary(JsonBinary)),
    ?assertEqual(#{<<"key1">> => 1, <<"key2">> => 2, <<"key3">> => 3}, Decoded).

decode_string_keys_test() ->
    JsonBinary = iolist_to_binary(
        json:encode(#{<<"key1">> => 1, <<"key2">> => 2, <<"key3">> => 3})
    ),
    {ok, Result} = spectra:decode(json, ?MODULE, string_kvlist, JsonBinary),
    ?assertEqual(3, length(Result)),
    ?assert(lists:member({"key1", 1}, Result)),
    ?assert(lists:member({"key2", 2}, Result)),
    ?assert(lists:member({"key3", 3}, Result)).

%% Atom keys
encode_atom_keys_test() ->
    Input = [{name, <<"John">>}, {email, <<"john@example.com">>}],
    {ok, JsonBinary} = spectra:encode(json, ?MODULE, atom_kvlist, Input),
    Decoded = json:decode(iolist_to_binary(JsonBinary)),
    ?assertEqual(#{<<"name">> => <<"John">>, <<"email">> => <<"john@example.com">>}, Decoded).

decode_atom_keys_test() ->
    JsonBinary = iolist_to_binary(
        json:encode(#{<<"name">> => <<"John">>, <<"email">> => <<"john@example.com">>})
    ),
    {ok, Result} = spectra:decode(json, ?MODULE, atom_kvlist, JsonBinary),
    ?assertEqual(2, length(Result)),
    ?assert(lists:member({name, <<"John">>}, Result)),
    ?assert(lists:member({email, <<"john@example.com">>}, Result)).

%% Binary keys
encode_binary_keys_test() ->
    Input = [{<<"key1">>, "value1"}, {<<"key2">>, "value2"}],
    {ok, JsonBinary} = spectra:encode(json, ?MODULE, binary_kvlist, Input),
    Decoded = json:decode(iolist_to_binary(JsonBinary)),
    ?assertEqual(#{<<"key1">> => <<"value1">>, <<"key2">> => <<"value2">>}, Decoded).

decode_binary_keys_test() ->
    JsonBinary = iolist_to_binary(
        json:encode(#{<<"key1">> => <<"value1">>, <<"key2">> => <<"value2">>})
    ),
    {ok, Result} = spectra:decode(json, ?MODULE, binary_kvlist, JsonBinary),
    ?assertEqual(2, length(Result)),
    ?assert(lists:member({<<"key1">>, "value1"}, Result)),
    ?assert(lists:member({<<"key2">>, "value2"}, Result)).

%% Integer keys
encode_integer_keys_test() ->
    Input = [{1, atom1}, {2, atom2}, {42, atom42}],
    {ok, JsonBinary} = spectra:encode(json, ?MODULE, integer_kvlist, Input),
    Decoded = json:decode(iolist_to_binary(JsonBinary)),
    %% Note: JSON encodes integer keys as their string representation
    ?assertEqual(3, map_size(Decoded)),
    %% Values should be present
    Values = lists:sort(maps:values(Decoded)),
    ?assertEqual([<<"atom1">>, <<"atom2">>, <<"atom42">>], Values).

%% Note: Decoding integer keys from JSON strings is not supported
%% because JSON only has string keys, and we can't reliably convert
%% "1" back to integer 1 vs string "1" without additional type information

%% Float keys
encode_float_keys_test() ->
    Input = [{1.5, <<"val1">>}, {2.7, <<"val2">>}, {3.14159, <<"pi">>}],
    {ok, JsonBinary} = spectra:encode(json, ?MODULE, float_kvlist, Input),
    Decoded = json:decode(iolist_to_binary(JsonBinary)),
    %% Note: JSON encodes float keys as their string representation
    ?assertEqual(3, map_size(Decoded)),
    Values = lists:sort(maps:values(Decoded)),
    ?assertEqual([<<"pi">>, <<"val1">>, <<"val2">>], Values).

%% Note: Decoding float keys from JSON strings is not supported
%% because JSON only has string keys, and we can't reliably convert
%% "1.5" back to float 1.5 vs string "1.5" without additional type information

%% =============================================================================
%% Tests for complex value types
%% =============================================================================

%% Map values
encode_map_values_test() ->
    Input = [{key1, #{field1 => 10, field2 => 20}}, {key2, #{field1 => 30}}],
    {ok, JsonBinary} = spectra:encode(json, ?MODULE, kvlist_with_map_values, Input),
    Decoded = json:decode(iolist_to_binary(JsonBinary)),
    ?assertEqual(
        #{
            <<"key1">> => #{<<"field1">> => 10, <<"field2">> => 20},
            <<"key2">> => #{<<"field1">> => 30}
        },
        Decoded
    ).

decode_map_values_test() ->
    JsonBinary = iolist_to_binary(
        json:encode(#{
            <<"key1">> => #{<<"field1">> => 10, <<"field2">> => 20},
            <<"key2">> => #{<<"field1">> => 30}
        })
    ),
    {ok, Result} = spectra:decode(json, ?MODULE, kvlist_with_map_values, JsonBinary),
    ?assertEqual(2, length(Result)),
    ?assert(lists:member({key1, #{field1 => 10, field2 => 20}}, Result)),
    ?assert(lists:member({key2, #{field1 => 30}}, Result)).

%% List values
encode_list_values_test() ->
    Input = [{<<"key1">>, [1, 2, 3]}, {<<"key2">>, [4, 5]}, {<<"key3">>, []}],
    {ok, JsonBinary} = spectra:encode(json, ?MODULE, kvlist_with_list_values, Input),
    Decoded = json:decode(iolist_to_binary(JsonBinary)),
    ?assertEqual(
        #{<<"key1">> => [1, 2, 3], <<"key2">> => [4, 5], <<"key3">> => []},
        Decoded
    ).

decode_list_values_test() ->
    JsonBinary = iolist_to_binary(
        json:encode(#{<<"key1">> => [1, 2, 3], <<"key2">> => [4, 5], <<"key3">> => []})
    ),
    {ok, Result} = spectra:decode(json, ?MODULE, kvlist_with_list_values, JsonBinary),
    ?assertEqual(3, length(Result)),
    ?assert(lists:member({<<"key1">>, [1, 2, 3]}, Result)),
    ?assert(lists:member({<<"key2">>, [4, 5]}, Result)),
    ?assert(lists:member({<<"key3">>, []}, Result)).

%% Nested kvlist values
encode_nested_kvlist_test() ->
    Input = [
        {"outer1", [{inner1, <<"a">>}, {inner2, <<"b">>}]},
        {"outer2", [{inner3, <<"c">>}]}
    ],
    {ok, JsonBinary} = spectra:encode(json, ?MODULE, kvlist_with_nested_kvlist, Input),
    Decoded = json:decode(iolist_to_binary(JsonBinary)),
    ?assertEqual(
        #{
            <<"outer1">> => #{<<"inner1">> => <<"a">>, <<"inner2">> => <<"b">>},
            <<"outer2">> => #{<<"inner3">> => <<"c">>}
        },
        Decoded
    ).

decode_nested_kvlist_test() ->
    JsonBinary = iolist_to_binary(
        json:encode(#{
            <<"outer1">> => #{<<"inner1">> => <<"a">>, <<"inner2">> => <<"b">>},
            <<"outer2">> => #{<<"inner3">> => <<"c">>}
        })
    ),
    {ok, Result} = spectra:decode(json, ?MODULE, kvlist_with_nested_kvlist, JsonBinary),
    ?assertEqual(2, length(Result)),
    ?assert(
        lists:member({"outer1", [{inner1, <<"a">>}, {inner2, <<"b">>}]}, Result) orelse
            lists:member({"outer1", [{inner2, <<"b">>}, {inner1, <<"a">>}]}, Result)
    ),
    ?assert(lists:member({"outer2", [{inner3, <<"c">>}]}, Result)).

%% =============================================================================
%% Edge cases and error tests
%% =============================================================================

%% Empty kvlist
encode_empty_kvlist_test() ->
    {ok, JsonBinary} = spectra:encode(json, ?MODULE, string_kvlist, []),
    Decoded = json:decode(iolist_to_binary(JsonBinary)),
    ?assertEqual(#{}, Decoded).

decode_empty_kvlist_test() ->
    JsonBinary = iolist_to_binary(json:encode(#{})),
    ?assertEqual({ok, []}, spectra:decode(json, ?MODULE, string_kvlist, JsonBinary)).

%% Invalid: not a list
encode_not_a_list_test() ->
    ?assertMatch({error, [_ | _]}, spectra:encode(json, ?MODULE, string_kvlist, not_a_list)).

%% Invalid: wrong tuple size
encode_wrong_tuple_size_test() ->
    Input = [{"key1", 1, extra}],
    ?assertMatch({error, [_ | _]}, spectra:encode(json, ?MODULE, string_kvlist, Input)).

%% Invalid: wrong key type
encode_wrong_key_type_test() ->
    %% integer key where string expected
    Input = [{123, 1}, {"key2", 2}],
    ?assertMatch({error, [_ | _]}, spectra:encode(json, ?MODULE, string_kvlist, Input)).

%% Invalid: wrong value type
encode_wrong_value_type_test() ->
    Input = [{"key1", 1}, {"key2", "not_an_integer"}],
    ?assertMatch({error, [_ | _]}, spectra:encode(json, ?MODULE, string_kvlist, Input)).

%% Invalid: decode from non-map
decode_not_a_map_test() ->
    JsonBinary = iolist_to_binary(json:encode([1, 2, 3])),
    ?assertMatch({error, [_ | _]}, spectra:decode(json, ?MODULE, string_kvlist, JsonBinary)).

%% Invalid: decode with wrong value type
decode_wrong_value_type_test() ->
    JsonBinary = iolist_to_binary(
        json:encode(#{<<"key1">> => 1, <<"key2">> => <<"not_an_integer">>})
    ),
    ?assertMatch({error, [_ | _]}, spectra:decode(json, ?MODULE, string_kvlist, JsonBinary)).
