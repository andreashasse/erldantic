-module(nonempty_string_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/record_type_introspect.hrl").

-type nonempty_user() :: #{name := nonempty_string(), email := nonempty_string()}.

%% Test function to validate nonempty_string
validate_nonempty_string_test() ->
    % Test JSON conversion using to_json
    ValidUser = #{name => "John", email => "john@example.com"},
    InvalidUser = #{name => "", email => "john@example.com"},

    {ok, Json} = to_json(ValidUser),
    ?assertMatch(#{name := <<"John">>, email := <<"john@example.com">>}, Json),

    {error, Errors} = to_json(InvalidUser),
    ?assertMatch([#ed_error{type = type_mismatch}], Errors),

    % Test JSON conversion using from_json
    ValidJson = #{<<"name">> => <<"Jane">>, <<"email">> => <<"jane@example.com">>},
    InvalidJson = #{<<"name">> => <<"">>, <<"email">> => <<"jane@example.com">>},

    {ok, User} = from_json(ValidJson),
    ?assertEqual(#{name => "Jane", email => "jane@example.com"}, User),

    {error, FromErrors} = from_json(InvalidJson),
    ?assertMatch([#ed_error{type = type_mismatch,
                            ctx = #{type := {type, nonempty_string}, value := <<>>}}],
                 FromErrors).

-spec to_json(nonempty_user()) -> {ok, json:json()} | {error, [#ed_error{}]}.
to_json(User) ->
    erldantic_json:to_json_no_pt({?MODULE, nonempty_user, 0}, User).

-spec from_json(json:json()) -> {ok, nonempty_user()} | {error, [#ed_error{}]}.
from_json(Json) ->
    erldantic_json:from_json_no_pt({?MODULE, nonempty_user, 0}, Json).
