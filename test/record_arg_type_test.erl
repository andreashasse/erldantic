-module(record_arg_type_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-record(result, {value, errors = [] :: [atom()]}).

-type int_result() :: result_t(atom()).
-type result_t(ResultType) :: #result{value :: integer(), errors :: [ResultType]}.

type_in_form_test() ->
    {ok, Types} = erldantic_abstract_code:types_in_module(?MODULE),
    ?assertEqual(#ed_user_type_ref{type_name = result_t, variables = [{type, atom}]},
                 maps:get({type, int_result, 0}, Types)),
    ?assertEqual(#ed_type_with_variables{type =
                                             #ed_rec_ref{record_name = result,
                                                         field_types =
                                                             [{value, {type, integer}},
                                                              {errors,
                                                               #ed_list{type =
                                                                            #ed_var{name =
                                                                                        'ResultType'}}}]},
                                         vars = ['ResultType']},
                 maps:get({type, result_t, 1}, Types)).

map1_to_json_test() ->
    ?assertEqual({ok, #{value => 1, errors => []}},
                 to_json_result_1(#result{value = 1, errors = []})),
    ?assertEqual({error,
                  [#ed_error{location = [value],
                             type = type_mismatch,
                             ctx = #{type => {type, integer}, value => pelle}}]},
                 to_json_result_1(#result{value = pelle, errors = []})).

map1_from_json_test() ->
    ?assertEqual({ok, #result{value = 1, errors = []}},
                 from_json_result_1(#{<<"value">> => 1, <<"errors">> => []})),
    ?assertEqual({error,
                  [#ed_error{location = [value],
                             type = type_mismatch,
                             ctx = #{type => {type, integer}, value => <<"hej">>}}]},
                 from_json_result_1(#{<<"value">> => <<"hej">>, <<"errors">> => []})).

-spec from_json_result_1(term()) -> int_result().
from_json_result_1(Data) ->
    erldantic_json:type_from_json(?MODULE, int_result, Data).

-spec to_json_result_1(int_result()) -> json:encode_value().
to_json_result_1(Data) ->
    erldantic_json:type_to_json(?MODULE, int_result, Data).
