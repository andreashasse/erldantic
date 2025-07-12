-module(same_name_type_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-type result() :: result(integer()).
-type result(ResultType) :: #{result => ResultType, errors => [atom()]}.

type_in_form_test() ->
    {ok, Types} = erldantic_abstract_code:types_in_module(?MODULE),
    ?assertEqual(#type_with_arguments{type =
                                          #a_map{fields =
                                                     [{map_field_assoc,
                                                       result,
                                                       {var, 'ResultType'}},
                                                      {map_field_assoc,
                                                       errors,
                                                       {list, {type, atom}}}]},
                                      vars = ['ResultType']},
                 maps:get({type, result, 1}, Types)).

result_0_to_json_test() ->
    ?assertEqual({ok, #{result => 1, errors => []}},
                 to_json_result_0(#{result => 1, errors => []})),
    ?assertEqual({error,
                  [#ed_error{location = [result],
                             type = type_mismatch,
                             ctx = #{type => {type, integer}, value => pelle}}]},
                 to_json_result_0(#{result => pelle, errors => []})).

result_0_from_json_test() ->
    ?assertEqual({ok, #{result => 1, errors => []}},
                 from_json_result_0(#{<<"result">> => 1, <<"errors">> => []})),
    ?assertEqual({error,
                  [#ed_error{location = [result],
                             type = type_mismatch,
                             ctx = #{type => {type, integer}, value => <<"hej">>}}]},
                 from_json_result_0(#{<<"result">> => <<"hej">>, <<"errors">> => []})).

-spec from_json_result_0(term()) -> result().
from_json_result_0(Data) ->
    erldantic_json:type_from_json(?MODULE, result, Data).

-spec to_json_result_0(result()) -> json:encode_value().
to_json_result_0(Data) ->
    erldantic_json:type_to_json(?MODULE, result, Data).
