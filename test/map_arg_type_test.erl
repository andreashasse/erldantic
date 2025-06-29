-module(map_arg_type_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/record_type_introspect.hrl").

-type int_result() :: result(integer()).
%-type map_result_2() :: result(#{atom() => integer()}, atom()).
-type result(ResultType) :: #{result => ResultType, errors => [atom()]}.

type_in_form_test() ->
    {ok, Types} = erldantic_parse_transform:types_in_module(?MODULE),
    ?assertEqual(#a_type{type = {user_type_ref, result, [{type, integer}]}, vars = []},
                 maps:get({type, int_result, 0}, Types)),

    ?assertEqual(#a_type{type =
                             #a_map{fields =
                                        [{map_field_assoc, result, {var, 'ResultType'}},
                                         {map_field_assoc, errors, {list, {type, atom}}}]},
                         vars = ['ResultType']},
                 maps:get({type, result, 1}, Types)).

map_to_json_test() ->
    ?assertEqual({ok, #{result => 1, errors => []}},
                 to_json_result_1(#{result => 1, errors => []})),
    ?assertEqual({error,
                  [#ed_error{location = [result],
                             type = type_mismatch,
                             ctx = #{type => {type, integer}, value => pelle}}]},
                 to_json_result_1(#{result => pelle, errors => []})).

map_from_json_test() ->
    ?assertEqual({ok, #{result => 1, errors => []}},
                 from_json_result_1(#{<<"result">> => 1, <<"errors">> => []})),
    ?assertEqual({error,
                  [#ed_error{location = [result],
                             type = type_mismatch,
                             ctx = #{type => {type, integer}, value => <<"hej">>}}]},
                 from_json_result_1(#{<<"result">> => <<"hej">>, <<"errors">> => []})).

-spec from_json_result_1(term()) -> int_result().
from_json_result_1(Data) ->
    erldantic_json:from_json_no_pt({?MODULE, int_result, 0}, Data).

-spec to_json_result_1(int_result()) -> json:encode_value().
to_json_result_1(Data) ->
    erldantic_json:to_json_no_pt({?MODULE, int_result, 0}, Data).
