-module(same_type_name_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-type map_result() :: result(#{atom() => integer()}).
%-type map_result_2() :: result(#{atom() => integer()}, atom()).
-type result(OkType) :: OkType | error.

%-type result(OkType, ErrorType) :: OkType | ErrorType.

type_in_form_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    {ok, MapResultType} = spectra_type_info:get_type(TypeInfo, map_result, 0),
    ?assertEqual(
        #sp_user_type_ref{
            type_name = result,
            variables =
                [
                    #sp_map{
                        fields =
                            [
                                #typed_map_field{
                                    kind = assoc,
                                    key_type = #sp_simple_type{type = atom},
                                    val_type = #sp_simple_type{type = integer}
                                }
                            ]
                    }
                ]
        },
        MapResultType
    ),
    {ok, ResultType} = spectra_type_info:get_type(TypeInfo, result, 1),
    ?assertEqual(
        #sp_type_with_variables{
            type =
                #sp_union{
                    types =
                        [
                            #sp_var{name = 'OkType'},
                            #sp_literal{value = error, binary_value = <<"error">>}
                        ]
                },
            vars = ['OkType']
        },
        ResultType
    ).

map1_to_json_test() ->
    ?assertEqual({ok, #{a1 => 1}}, to_json_result_1(#{a1 => 1})),
    ?assertEqual({ok, <<"error">>}, to_json_result_1(error)).

map1_from_json_test() ->
    ?assertEqual({ok, #{a1 => 1}}, from_json_result_1(#{a1 => 1})),
    ?assertEqual({ok, error}, from_json_result_1(<<"error">>)).

map1_to_json_bad_test() ->
    ?assertEqual(
        {error, [
            #sp_error{
                location = [],
                type = no_match,
                ctx =
                    #{
                        type =>
                            #sp_union{
                                types =
                                    [
                                        #sp_map{
                                            fields =
                                                [
                                                    #typed_map_field{
                                                        kind = assoc,
                                                        key_type = #sp_simple_type{type = atom},
                                                        val_type = #sp_simple_type{
                                                            type =
                                                                integer
                                                        }
                                                    }
                                                ]
                                        },
                                        #sp_literal{value = error, binary_value = <<"error">>}
                                    ]
                            },
                        value => #{a1 => hej}
                    }
            }
        ]},
        to_json_result_1(#{a1 => hej})
    ),
    ?assertEqual(
        {error, [
            #sp_error{
                location = [],
                type = no_match,
                ctx =
                    #{
                        type =>
                            #sp_union{
                                types =
                                    [
                                        #sp_map{
                                            fields =
                                                [
                                                    #typed_map_field{
                                                        kind = assoc,
                                                        key_type = #sp_simple_type{type = atom},
                                                        val_type = #sp_simple_type{
                                                            type =
                                                                integer
                                                        }
                                                    }
                                                ]
                                        },
                                        #sp_literal{value = error, binary_value = <<"error">>}
                                    ]
                            },
                        value => pelle
                    }
            }
        ]},
        to_json_result_1(pelle)
    ).

map1_from_json_bad_test() ->
    ?assertEqual(
        {error, [
            #sp_error{
                location = [],
                type = no_match,
                ctx =
                    #{
                        type =>
                            #sp_union{
                                types =
                                    [
                                        #sp_map{
                                            fields =
                                                [
                                                    #typed_map_field{
                                                        kind = assoc,
                                                        key_type = #sp_simple_type{type = atom},
                                                        val_type = #sp_simple_type{
                                                            type =
                                                                integer
                                                        }
                                                    }
                                                ]
                                        },
                                        #sp_literal{value = error, binary_value = <<"error">>}
                                    ]
                            },
                        value => #{a1 => hej}
                    }
            }
        ]},
        from_json_result_1(#{a1 => hej})
    ),
    ?assertEqual(
        {error, [
            #sp_error{
                location = [],
                type = no_match,
                ctx =
                    #{
                        type =>
                            #sp_union{
                                types =
                                    [
                                        #sp_map{
                                            fields =
                                                [
                                                    #typed_map_field{
                                                        kind = assoc,
                                                        key_type = #sp_simple_type{type = atom},
                                                        val_type = #sp_simple_type{
                                                            type =
                                                                integer
                                                        }
                                                    }
                                                ]
                                        },
                                        #sp_literal{value = error, binary_value = <<"error">>}
                                    ]
                            },
                        value => pelle
                    }
            }
        ]},
        from_json_result_1(pelle)
    ).

-spec from_json_result_1(term()) -> map_result().
from_json_result_1(Data) ->
    spectra_json:from_json(?MODULE, {type, map_result, 0}, Data).

-spec to_json_result_1(map_result()) -> json:encode_value().
to_json_result_1(Data) ->
    spectra_json:to_json(?MODULE, {type, map_result, 0}, Data).
