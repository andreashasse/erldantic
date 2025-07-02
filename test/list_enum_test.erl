-module(list_enum_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/record_type_introspect.hrl").

-type accesses() :: [read | write].

%% Test function to validate accesses type
validate_accesses_test() ->
    % Test JSON conversion using to_json
    ValidData1 = [read],
    ValidData2 = [write],
    ValidData3 = [read, write],
    ValidData4 = [],
    InvalidData = [read, invalid],

    % Test with valid values
    ?assertEqual({ok, [read]}, to_json_accesses(ValidData1)),
    ?assertEqual({ok, [write]}, to_json_accesses(ValidData2)),
    ?assertEqual({ok, [read, write]}, to_json_accesses(ValidData3)),
    ?assertEqual({ok, []}, to_json_accesses(ValidData4)),

    % Test with invalid data
    {error, Errors} = to_json_accesses(InvalidData),
    ?assertMatch([#ed_error{type = no_match,
                            ctx =
                                #{type := {union, [{literal, read}, {literal, write}]},
                                  value := invalid}}],
                 Errors),

    % Test JSON conversion using from_json
    ValidJson1 = [<<"read">>],
    ValidJson2 = [<<"write">>],
    ValidJson3 = [<<"read">>, <<"write">>],
    ValidJson4 = [],
    InvalidJson = [<<"read">>, <<"invalid">>],

    % Test from_json with valid values
    ?assertEqual({ok, [read]}, from_json_accesses(ValidJson1)),
    ?assertEqual({ok, [write]}, from_json_accesses(ValidJson2)),
    ?assertEqual({ok, [read, write]}, from_json_accesses(ValidJson3)),
    ?assertEqual({ok, []}, from_json_accesses(ValidJson4)),

    % Test from_json with invalid data
    {error, FromErrors} = from_json_accesses(InvalidJson),
    ?assertMatch([#ed_error{type = no_match,
                            ctx =
                                #{type := {union, [{literal, read}, {literal, write}]},
                                  value := <<"invalid">>}}],
                 FromErrors).

-spec to_json_accesses(accesses()) ->
                          {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json_accesses(Data) ->
    erldantic_json:type_to_json(?MODULE, accesses, Data).

-spec from_json_accesses(json:encode_value()) ->
                            {ok, accesses()} | {error, [erldantic:error()]}.
from_json_accesses(Json) ->
    erldantic_json:type_from_json(?MODULE, accesses, Json).
