-module(enum_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/impala.hrl").
-include("../include/impala_internal.hrl").

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
    ?assertMatch([#im_error{type = no_match,
                            ctx =
                                #{type :=
                                      #im_union{types =
                                                    [#im_literal{value = 1},
                                                     #im_literal{value = 3}]},
                                  value := 2}}],
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
    ?assertMatch([#im_error{type = no_match,
                            ctx =
                                #{type :=
                                      #im_union{types =
                                                    [#im_literal{value = 1},
                                                     #im_literal{value = 3}]},
                                  value := 2}}],
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
    ?assertMatch([#im_error{type = no_match,
                            ctx =
                                #{type :=
                                      #im_union{types =
                                                    [#im_literal{value = admin},
                                                     #im_literal{value = user},
                                                     #im_literal{value = guest}]},
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
    ?assertMatch([#im_error{type = no_match,
                            ctx =
                                #{type :=
                                      #im_union{types =
                                                    [#im_literal{value = admin},
                                                     #im_literal{value = user},
                                                     #im_literal{value = guest}]},
                                  value := <<"moderator">>}}],
                 FromErrors).

-spec to_json_non_atom_enum(non_atom_enum()) ->
                               {ok, json:encode_value()} | {error, [impala:error()]}.
to_json_non_atom_enum(Data) ->
    impala_json:to_json(?MODULE, {type, non_atom_enum, 0}, Data).

-spec from_json_non_atom_enum(json:encode_value()) ->
                                 {ok, non_atom_enum()} | {error, [impala:error()]}.
from_json_non_atom_enum(Json) ->
    impala_json:from_json(?MODULE, {type, non_atom_enum, 0}, Json).

-spec to_json_role(role()) -> {ok, json:encode_value()} | {error, [impala:error()]}.
to_json_role(Data) ->
    impala_json:to_json(?MODULE, {type, role, 0}, Data).

-spec from_json_role(json:encode_value()) -> {ok, role()} | {error, [impala:error()]}.
from_json_role(Json) ->
    impala_json:from_json(?MODULE, {type, role, 0}, Json).
