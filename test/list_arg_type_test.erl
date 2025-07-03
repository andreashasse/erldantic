-module(list_arg_type_test).

-include_lib("erldantic/include/erldantic.hrl").
-include_lib("erldantic/include/erldantic_internal.hrl").
-include_lib("eunit/include/eunit.hrl").

-type int_result() :: result(integer()).
-type result(ResultType) :: [ResultType].

valid_int_result_to_json_test() ->
    Valid = [1, 2, 3],
    ?assertEqual({ok, [1, 2, 3]}, to_json_int_result(Valid)).

invalid_int_result_to_json_test() ->
    Invalid = [1, 2, "three"],
    Result = to_json_int_result(Invalid),
    ?assertMatch({error, [_ | _]}, Result).

valid_int_result_from_json_test() ->
    ValidJson = [1, 2, 3],
    ?assertEqual({ok, [1, 2, 3]}, from_json_int_result(ValidJson)).

invalid_int_result_from_json_test() ->
    InvalidJson = [1, 2, "three"],
    Result = from_json_int_result(InvalidJson),
    ?assertMatch({error, [_ | _]}, Result).

-spec to_json_int_result(int_result()) ->
                            {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json_int_result(Result) ->
    erldantic_json:type_to_json(?MODULE, int_result, Result).

-spec from_json_int_result(json:encode_value()) ->
                              {ok, int_result()} | {error, [erldantic:error()]}.
from_json_int_result(Json) ->
    erldantic_json:type_from_json(?MODULE, int_result, Json).
