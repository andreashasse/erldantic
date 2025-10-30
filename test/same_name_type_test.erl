-module(same_name_type_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-type result() :: result(integer()).
-type result(ResultType) :: #{result => ResultType, errors => [atom()]}.

type_in_form_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    {ok, ResultType} = spectra_type_info:get_type(TypeInfo, result, 1),
    ?assertEqual(
        #sp_type_with_variables{
            type =
                #sp_map{
                    fields =
                        [
                            #literal_map_field{
                                kind = assoc,
                                name = result,
                                binary_name = <<"result">>,
                                val_type = #sp_var{name = 'ResultType'}
                            },
                            #literal_map_field{
                                kind = assoc,
                                name = errors,
                                binary_name = <<"errors">>,
                                val_type = #sp_list{
                                    type =
                                        #sp_simple_type{
                                            type =
                                                atom
                                        }
                                }
                            }
                        ]
                },
            vars = ['ResultType']
        },
        ResultType
    ).

result_0_to_json_test() ->
    ?assertEqual(
        {ok, #{result => 1, errors => []}},
        to_json_result_0(#{result => 1, errors => []})
    ),
    ?assertEqual(
        {error, [
            #sp_error{
                location = [result],
                type = type_mismatch,
                ctx = #{type => #sp_simple_type{type = integer}, value => pelle}
            }
        ]},
        to_json_result_0(#{result => pelle, errors => []})
    ).

result_0_from_json_test() ->
    ?assertEqual(
        {ok, #{result => 1, errors => []}},
        from_json_result_0(#{<<"result">> => 1, <<"errors">> => []})
    ),
    ?assertEqual(
        {error, [
            #sp_error{
                location = [result],
                type = type_mismatch,
                ctx =
                    #{type => #sp_simple_type{type = integer}, value => <<"hej">>}
            }
        ]},
        from_json_result_0(#{<<"result">> => <<"hej">>, <<"errors">> => []})
    ).

-spec from_json_result_0(term()) -> result().
from_json_result_0(Data) ->
    spectra_json:from_json(?MODULE, {type, result, 0}, Data).

-spec to_json_result_0(result()) -> json:encode_value().
to_json_result_0(Data) ->
    spectra_json:to_json(?MODULE, {type, result, 0}, Data).
