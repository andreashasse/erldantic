-module(map_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/record_type_introspect.hrl").

-type atom_map() :: #{a1 := integer(), atom() => atom()}.
-type atom_map2() :: #{a1 => 1, atom() => atom()}.
-type atom_map3() :: #{a1 := kalle, atom() => atom()}.
-type type_shaddow_literal_map() :: #{atom() => atom(), a1 => 1}.

map1_test() ->
    ?assertEqual({ok, #{a1 => 1}}, to_json_atom_map(#{a1 => 1})).

map1_bad_test() ->
    ?assertEqual({error,
                  [#ed_error{location = [a1],
                             type = type_mismatch,
                             ctx = #{type => {type, integer}, value => hej}}]},
                 to_json_atom_map(#{a1 => hej})).

map2_test() ->
    ?assertEqual({ok, #{a1 => 1, other => value2}},
                 to_json_atom_map2(#{a1 => 1, other => value2})).

map2_bad_test() ->
    ?assertEqual({error,
                  [#ed_error{location = [a1],
                             type = type_mismatch,
                             ctx = #{type => {literal, 1}, value => should_not_work}}]},
                 to_json_atom_map2(#{a1 => should_not_work, other => value2})),
    ?assertMatch({error,
                  [#ed_error{location = [a1],
                             type = type_mismatch,
                             ctx = #{value := "hej"}}]},
                 to_json_atom_map2(#{a1 => "hej"})).

map3_test() ->
    ?assertEqual({ok, #{a1 => kalle}}, to_json_atom_map3(#{a1 => kalle})).

map3_bad_test() ->
    ?assertEqual({error, [#ed_error{location = [a1], type = missing_data}]},
                 to_json_atom_map3(#{not_a1 => kalle})).

type_shaddow_literal_map_test() ->
    ?assertEqual({ok, #{a1 => pelle, some_atom => some_value}},
                 to_json_type_shaddow_literal_map(#{a1 => pelle, some_atom => some_value})).

type_shaddow_literal_map_bad_test() ->
    ?assertEqual({error,
                  [#ed_error{location = [a1],
                             type = type_mismatch,
                             ctx = #{type => {type, atom}, value => 1}}]},
                 to_json_type_shaddow_literal_map(#{a1 => 1, some_atom => some_value})).

-spec to_json_atom_map(term()) -> {ok, atom_map()} | {error, [#ed_error{}]}.
to_json_atom_map(Data) ->
    erldantic_json:to_json_no_pt({?MODULE, atom_map, 0}, Data).

-spec to_json_atom_map2(term()) -> {ok, atom_map2()} | {error, [#ed_error{}]}.
to_json_atom_map2(Data) ->
    erldantic_json:to_json_no_pt({?MODULE, atom_map2, 0}, Data).

-spec to_json_atom_map3(term()) -> {ok, atom_map3()} | {error, [#ed_error{}]}.
to_json_atom_map3(Data) ->
    erldantic_json:to_json_no_pt({?MODULE, atom_map3, 0}, Data).

-spec to_json_type_shaddow_literal_map(term()) ->
                                          {ok, type_shaddow_literal_map()} | {error, [#ed_error{}]}.
to_json_type_shaddow_literal_map(Data) ->
    erldantic_json:to_json_no_pt({?MODULE, type_shaddow_literal_map, 0}, Data).
