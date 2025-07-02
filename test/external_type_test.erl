-module(external_type_test).

-include_lib("eunit/include/eunit.hrl").

-include("external_type.hrl").
-include("../include/record_type_introspect.hrl").

-type int_result() :: external_type:result_t(integer()).

type_in_form_test() ->
    {ok, Types} = erldantic_abstract_code:types_in_module(?MODULE),
    ?assertEqual(#remote_type{mfargs = {external_type, result_t, [{type, integer}]}},
                 maps:get({type, int_result, 0}, Types)).

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
