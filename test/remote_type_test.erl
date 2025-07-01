-module(remote_type_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/record_type_introspect.hrl").

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
    Result = to_json_missing(ValidData),
    ?assertMatch({error, _}, Result),

    % Test from_json as well
    Json = #{<<"a">> => <<"some_value">>},
    FromJsonResult = from_json_missing(Json),
    ?assertMatch({error, _}, FromJsonResult).

-spec to_json_remote(remote()) -> {ok, json:json()} | {error, [#ed_error{}]}.
to_json_remote(Data) ->
    erldantic_json:type_to_json(?MODULE, remote, 0, Data).

-spec from_json_remote(json:json()) -> {ok, remote()} | {error, [#ed_error{}]}.
from_json_remote(Json) ->
    erldantic_json:type_from_json(?MODULE, remote, 0, Json).

-spec to_json_missing(missing()) -> {ok, json:json()} | {error, [#ed_error{}]}.
to_json_missing(Data) ->
    erldantic_json:type_to_json(?MODULE, missing, 0, Data).

-spec from_json_missing(json:json()) -> {ok, missing()} | {error, [#ed_error{}]}.
from_json_missing(Json) ->
    erldantic_json:type_from_json(?MODULE, missing, 0, Json).
