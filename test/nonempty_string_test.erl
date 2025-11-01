-module(nonempty_string_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-type nonempty_user() :: #{name := nonempty_string(), email := nonempty_string()}.

%% Test function to validate nonempty_string
validate_nonempty_string_test() ->
    % Test JSON conversion using to_json
    ValidUser = #{name => "John", email => "john@example.com"},
    InvalidUser = #{name => "", email => "john@example.com"},

    {ok, Json} = to_json(ValidUser),
    ?assertMatch(#{<<"name">> := <<"John">>, <<"email">> := <<"john@example.com">>}, Json),

    {error, Errors} = to_json(InvalidUser),
    ?assertMatch([#sp_error{type = type_mismatch}], Errors),

    % Test JSON conversion using from_json
    ValidJson = #{<<"name">> => <<"Jane">>, <<"email">> => <<"jane@example.com">>},
    InvalidJson = #{<<"name">> => <<"">>, <<"email">> => <<"jane@example.com">>},

    {ok, User} = from_json(ValidJson),
    ?assertEqual(#{name => "Jane", email => "jane@example.com"}, User),

    {error, FromErrors} = from_json(InvalidJson),
    ?assertMatch(
        [
            #sp_error{
                type = type_mismatch,
                ctx =
                    #{type := #sp_simple_type{type = nonempty_string}, value := <<>>}
            }
        ],
        FromErrors
    ).

-spec to_json(nonempty_user()) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
to_json(User) ->
    spectra_json:to_json(?MODULE, {type, nonempty_user, 0}, User).

-spec from_json(json:encode_value()) ->
    {ok, nonempty_user()} | {error, [spectra:error()]}.
from_json(Json) ->
    spectra_json:from_json(?MODULE, {type, nonempty_user, 0}, Json).
