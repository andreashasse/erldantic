-module(enum_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/record_type_introspect.hrl").

-type non_atom_enum() :: 1 | 3.
-type role() :: admin | user | guest.

%% Test function to validate non_atom_enum type
validate_non_atom_enum_test() ->
    % Test JSON conversion using to_json
    ValidData1 = 1,
    ValidData2 = 3,
    InvalidData = 2,

    % Test with valid values
    ?assertEqual({ok, 1}, to_json_non_atom_enum(ValidData1)),
    ?assertEqual({ok, 3}, to_json_non_atom_enum(ValidData2)),

    % Test with invalid data
    {error, Errors} = to_json_non_atom_enum(InvalidData),
    ?assertMatch([#ed_error{type = no_match,
                            ctx = #{type := {union, [{literal, 1}, {literal, 3}]}, value := 2}}],
                 Errors),

    % Test JSON conversion using from_json
    ValidJson1 = 1,
    ValidJson2 = 3,
    InvalidJson = 2,

    % Test from_json with valid values
    ?assertEqual({ok, 1}, from_json_non_atom_enum(ValidJson1)),
    ?assertEqual({ok, 3}, from_json_non_atom_enum(ValidJson2)),

    % Test from_json with invalid data
    {error, FromErrors} = from_json_non_atom_enum(InvalidJson),
    ?assertMatch([#ed_error{type = no_match,
                            ctx = #{type := {union, [{literal, 1}, {literal, 3}]}, value := 2}}],
                 FromErrors).

%% Test function to validate role type
validate_role_test() ->
    % Test JSON conversion using to_json
    ValidData1 = admin,
    ValidData2 = user,
    ValidData3 = guest,
    InvalidData = moderator,

    % Test with valid values
    ?assertEqual({ok, admin}, to_json_role(ValidData1)),
    ?assertEqual({ok, user}, to_json_role(ValidData2)),
    ?assertEqual({ok, guest}, to_json_role(ValidData3)),

    % Test with invalid data
    {error, Errors} = to_json_role(InvalidData),
    ?assertMatch([#ed_error{type = no_match,
                            ctx =
                                #{type :=
                                      {union,
                                       [{literal, admin}, {literal, user}, {literal, guest}]},
                                  value := moderator}}],
                 Errors),

    % Test JSON conversion using from_json
    ValidJson1 = <<"admin">>,
    ValidJson2 = <<"user">>,
    ValidJson3 = <<"guest">>,
    InvalidJson = <<"moderator">>,

    % Test from_json with valid values
    ?assertEqual({ok, admin}, from_json_role(ValidJson1)),
    ?assertEqual({ok, user}, from_json_role(ValidJson2)),
    ?assertEqual({ok, guest}, from_json_role(ValidJson3)),

    % Test from_json with invalid data
    {error, FromErrors} = from_json_role(InvalidJson),
    ?assertMatch([#ed_error{type = no_match,
                            ctx =
                                #{type :=
                                      {union,
                                       [{literal, admin}, {literal, user}, {literal, guest}]},
                                  value := <<"moderator">>}}],
                 FromErrors).

-spec to_json_non_atom_enum(non_atom_enum()) ->
                               {ok, json:json()} | {error, [#ed_error{}]}.
to_json_non_atom_enum(Data) ->
    erldantic_json:type_to_json(?MODULE, non_atom_enum, 0, Data).

-spec from_json_non_atom_enum(json:json()) ->
                                 {ok, non_atom_enum()} | {error, [#ed_error{}]}.
from_json_non_atom_enum(Json) ->
    erldantic_json:type_from_json(?MODULE, non_atom_enum, 0, Json).

-spec to_json_role(role()) -> {ok, json:json()} | {error, [#ed_error{}]}.
to_json_role(Data) ->
    erldantic_json:type_to_json(?MODULE, role, 0, Data).

-spec from_json_role(json:json()) -> {ok, role()} | {error, [#ed_error{}]}.
from_json_role(Json) ->
    erldantic_json:type_from_json(?MODULE, role, 0, Json).
