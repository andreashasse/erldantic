-module(map_key_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-type map_key_kvlist() :: [{#{atom() => integer()}, binary()}].
-type list_key_kvlist() :: [{[integer()], binary()}].

-export_type([map_key_kvlist/0, list_key_kvlist/0]).

%% Test that map keys return a proper error
%% because JSON doesn't support map keys
encode_map_key_returns_error_test() ->
    Input = [{#{a => 1}, <<"value1">>}, {#{b => 2}, <<"value2">>}],
    Result = spectra:encode(json, ?MODULE, map_key_kvlist, Input),
    ?assertMatch({error, [_]}, Result),
    {error, [Error]} = Result,
    ?assertEqual(type_mismatch, Error#sp_error.type),
    ?assertMatch(#{expected := json_compatible_key_type, key_type := #sp_map{}}, Error#sp_error.ctx).

%% Test that list keys (non-string) return a proper error
%% because JSON doesn't support list keys (except strings)
encode_list_key_returns_error_test() ->
    Input = [{[1, 2, 3], <<"value1">>}, {[4, 5], <<"value2">>}],
    Result = spectra:encode(json, ?MODULE, list_key_kvlist, Input),
    ?assertMatch({error, [_]}, Result),
    {error, [Error]} = Result,
    ?assertEqual(type_mismatch, Error#sp_error.type),
    ?assertMatch(#{expected := json_compatible_key_type, key_type := #sp_list{}}, Error#sp_error.ctx).
