-module(remote_type_test).

-include_lib("eunit/include/eunit.hrl").

-type remote() :: #{a => other:account()}.
-type missing() :: #{a => pelle:kolle()}.

%% Test function to validate remote type (should work with existing other:account())
validate_remote_test() ->
    % Test JSON conversion using to_json
    ValidData = #{a => #{id => "123", balance => 1000}},

    % Test with valid data
    ?assertEqual({ok, #{a => #{id => <<"123">>, balance => 1000}}},
                 to_json_remote(ValidData)),

    % Test JSON conversion using from_json
    ValidJson = #{<<"a">> => #{<<"id">> => <<"123">>, <<"balance">> => 1000}},

    % Test from_json with valid data
    ?assertEqual({ok, #{a => #{id => "123", balance => 1000}}}, from_json_remote(ValidJson)).

%% Test function to validate missing remote type (should fail)
validate_missing_test() ->
    % This should fail because pelle:kolle() doesn't exist
    % Test that the type system correctly handles missing remote types
    ValidData = #{a => some_value},

    % This should return an error due to missing remote type
    ?assertError({module_types_not_found, _, _}, to_json_missing(ValidData)),

    % Test from_json as well
    Json = #{<<"a">> => <<"some_value">>},
    ?assertError({module_types_not_found, _, _}, from_json_missing(Json)).

-spec to_json_remote(remote()) -> {ok, json:encode_value()} | {error, [impala:error()]}.
to_json_remote(Data) ->
    impala_json:to_json(?MODULE, {type, remote, 0}, Data).

-spec from_json_remote(json:encode_value()) -> {ok, remote()} | {error, [impala:error()]}.
from_json_remote(Json) ->
    impala_json:from_json(?MODULE, {type, remote, 0}, Json).

-spec to_json_missing(missing()) -> {ok, json:encode_value()} | {error, [impala:error()]}.
to_json_missing(Data) ->
    impala_json:to_json(?MODULE, {type, missing, 0}, Data).

-spec from_json_missing(json:encode_value()) ->
                           {ok, missing()} | {error, [impala:error()]}.
from_json_missing(Json) ->
    impala_json:from_json(?MODULE, {type, missing, 0}, Json).
