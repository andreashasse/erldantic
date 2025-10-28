-module(map_arg_type_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-type int_result() :: result(integer()).
%-type map_result_2() :: result(#{atom() => integer()}, atom()).
-type result(ResultType) :: #{result => ResultType, errors => [atom()]}.

type_in_form_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    {ok, IntResultType} = spectra_type_info:get_type(TypeInfo, int_result, 0),
    ?assertEqual(
        #sp_user_type_ref{
            type_name = result,
            variables = [#sp_simple_type{type = integer}]
        },
        IntResultType
    ),

    {ok, ResultType} = spectra_type_info:get_type(TypeInfo, result, 1),
    ?assertEqual(
        #sp_type_with_variables{
            type =
                #sp_map{
                    fields =
                        [
                            {map_field_assoc, result, #sp_var{name = 'ResultType'}},
                            {map_field_assoc, errors, #sp_list{
                                type =
                                    #sp_simple_type{
                                        type =
                                            atom
                                    }
                            }}
                        ]
                },
            vars = ['ResultType']
        },
        ResultType
    ).

map_to_json_test() ->
    ?assertEqual(
        {ok, #{result => 1, errors => []}},
        to_json_result_1(#{result => 1, errors => []})
    ),
    ?assertEqual(
        {error, [
            #sp_error{
                location = [result],
                type = type_mismatch,
                ctx = #{type => #sp_simple_type{type = integer}, value => pelle}
            }
        ]},
        to_json_result_1(#{result => pelle, errors => []})
    ).

map_from_json_test() ->
    ?assertEqual(
        {ok, #{result => 1, errors => []}},
        from_json_result_1(#{<<"result">> => 1, <<"errors">> => []})
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
        from_json_result_1(#{<<"result">> => <<"hej">>, <<"errors">> => []})
    ).

-spec from_json_result_1(term()) -> int_result().
from_json_result_1(Data) ->
    spectra_json:from_json(?MODULE, {type, int_result, 0}, Data).

-spec to_json_result_1(int_result()) -> json:encode_value().
to_json_result_1(Data) ->
    spectra_json:to_json(?MODULE, {type, int_result, 0}, Data).
