-module(same_type_name_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-type map_result() :: result(#{atom() => integer()}).
%-type map_result_2() :: result(#{atom() => integer()}, atom()).
-type result(OkType) :: OkType | error.

%-type result(OkType, ErrorType) :: OkType | ErrorType.

type_in_form_test() ->
    {ok, Types} = erldantic_abstract_code:types_in_module(?MODULE),
    ?assertEqual({ed_user_type_ref,
                  result,
                  [#a_map{fields = [{map_field_type_assoc, {type, atom}, {type, integer}}]}]},
                 maps:get({type, map_result, 0}, Types)),
    ?assertEqual(#type_with_arguments{type =
                                          #ed_union{types =
                                                        [{ed_var, 'OkType'},
                                                         #ed_literal{value = error}]},
                                      vars = ['OkType']},
                 maps:get({type, result, 1}, Types)).

map1_to_json_test() ->
    ?assertEqual({ok, #{a1 => 1}}, to_json_result_1(#{a1 => 1})),
    ?assertEqual({ok, error}, to_json_result_1(error)).

map1_from_json_test() ->
    ?assertEqual({ok, #{a1 => 1}}, from_json_result_1(#{a1 => 1})),
    ?assertEqual({ok, error}, from_json_result_1(<<"error">>)).

map1_to_json_bad_test() ->
    ?assertEqual({error,
                  [#ed_error{location = [],
                             type = no_match,
                             ctx =
                                 #{type =>
                                       #ed_union{types =
                                                     [{a_map,
                                                       [{map_field_type_assoc,
                                                         {type, atom},
                                                         {type, integer}}]},
                                                      #ed_literal{value = error}]},
                                   value => #{a1 => hej}}}]},
                 to_json_result_1(#{a1 => hej})),
    ?assertEqual({error,
                  [#ed_error{location = [],
                             type = no_match,
                             ctx =
                                 #{type =>
                                       #ed_union{types =
                                                     [{a_map,
                                                       [{map_field_type_assoc,
                                                         {type, atom},
                                                         {type, integer}}]},
                                                      #ed_literal{value = error}]},
                                   value => pelle}}]},
                 to_json_result_1(pelle)).

map1_from_json_bad_test() ->
    ?assertEqual({error,
                  [#ed_error{location = [],
                             type = no_match,
                             ctx =
                                 #{type =>
                                       #ed_union{types =
                                                     [{a_map,
                                                       [{map_field_type_assoc,
                                                         {type, atom},
                                                         {type, integer}}]},
                                                      #ed_literal{value = error}]},
                                   value => #{a1 => hej}}}]},
                 from_json_result_1(#{a1 => hej})),
    ?assertEqual({error,
                  [#ed_error{location = [],
                             type = no_match,
                             ctx =
                                 #{type =>
                                       #ed_union{types =
                                                     [{a_map,
                                                       [{map_field_type_assoc,
                                                         {type, atom},
                                                         {type, integer}}]},
                                                      #ed_literal{value = error}]},
                                   value => pelle}}]},
                 from_json_result_1(pelle)).

-spec from_json_result_1(term()) -> map_result().
from_json_result_1(Data) ->
    erldantic_json:type_from_json(?MODULE, map_result, Data).

-spec to_json_result_1(map_result()) -> json:encode_value().
to_json_result_1(Data) ->
    erldantic_json:type_to_json(?MODULE, map_result, Data).
