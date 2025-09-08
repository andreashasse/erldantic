-module(nonempty_list_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-type item() :: integer().
-type nonempty_items() :: [item(), ...].
-type user_with_items() :: #{name := string(), items := nonempty_items()}.

%% Test function to validate nonempty_list
validate_nonempty_list_test() ->
    % Test JSON conversion using to_json
    ValidUser = #{name => "John", items => [1, 2, 3]},
    InvalidUser = #{name => "John", items => []},

    ?assertEqual({ok, #{name => <<"John">>, items => [1, 2, 3]}}, to_json(ValidUser)),

    {error, Errors} = to_json(InvalidUser),
    ?assertEqual([#ed_error{location = [items],
                            type = type_mismatch,
                            ctx =
                                #{type =>
                                      {nonempty_list,
                                       #ed_user_type_ref{type_name = item, variables = []}},
                                  value => []}}],
                 Errors),

    % Test JSON conversion using from_json
    ValidJson = #{<<"name">> => <<"Jane">>, <<"items">> => [4, 5, 6]},
    InvalidJson = #{<<"name">> => <<"Jane">>, <<"items">> => []},

    {ok, User} = from_json(ValidJson),
    ?assertEqual(#{name => "Jane", items => [4, 5, 6]}, User),

    {error, FromErrors} = from_json(InvalidJson),
    ?assertEqual([#ed_error{location = [items],
                            type = type_mismatch,
                            ctx =
                                #{type =>
                                      {nonempty_list,
                                       #ed_user_type_ref{type_name = item, variables = []}},
                                  value => []}}],
                 FromErrors).

-spec to_json(user_with_items()) ->
                 {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json(User) ->
    erldantic_json:to_json(?MODULE, {type, user_with_items, 0}, User).

-spec from_json(json:encode_value()) ->
                   {ok, user_with_items()} | {error, [erldantic:error()]}.
from_json(Json) ->
    erldantic_json:from_json(?MODULE, {type, user_with_items, 0}, Json).
