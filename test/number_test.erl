-module(number_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/record_type_introspect.hrl").

-type number_data() :: #{name := string(), value := number()}.

%% Test function to validate number type
validate_number_test() ->
    % Test JSON conversion using to_json
    ValidIntegerData = #{name => "Integer", value => 42},
    ValidFloatData = #{name => "Float", value => 3.14},
    InvalidData = #{name => "Invalid", value => "not a number"},

    % Test with integer
    ?assertEqual({ok, #{name => <<"Integer">>, value => 42}}, to_json(ValidIntegerData)),

    % Test with float
    ?assertEqual({ok, #{name => <<"Float">>, value => 3.14}}, to_json(ValidFloatData)),

    % Test with invalid data
    {error, Errors} = to_json(InvalidData),
    ?assertMatch([#ed_error{type = type_mismatch,
                            ctx = #{type := {type, number}, value := "not a number"}}],
                 Errors),

    % Test JSON conversion using from_json
    ValidIntegerJson = #{<<"name">> => <<"Integer">>, <<"value">> => 42},
    ValidFloatJson = #{<<"name">> => <<"Float">>, <<"value">> => 3.14},
    InvalidJson = #{<<"name">> => <<"Invalid">>, <<"value">> => <<"not a number">>},

    % Test from_json with integer
    ?assertEqual({ok, #{name => "Integer", value => 42}}, from_json(ValidIntegerJson)),

    % Test from_json with float
    ?assertEqual({ok, #{name => "Float", value => 3.14}}, from_json(ValidFloatJson)),

    % Test from_json with invalid data
    {error, FromErrors} = from_json(InvalidJson),
    ?assertMatch([#ed_error{type = type_mismatch,
                            ctx = #{type := {type, number}, value := <<"not a number">>}}],
                 FromErrors).

-spec to_json(number_data()) -> {ok, json:json()} | {error, [#ed_error{}]}.
to_json(Data) ->
    erldantic_json:type_to_json(?MODULE, number_data, 0, Data).

-spec from_json(json:json()) -> {ok, number_data()} | {error, [#ed_error{}]}.
from_json(Json) ->
    erldantic_json:type_from_json(?MODULE, number_data, 0, Json).
