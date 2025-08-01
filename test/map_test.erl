-module(map_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-type atom_map() :: #{a1 := integer(), atom() => atom()}.
-type atom_map2() :: #{a1 => 1, atom() => atom()}.
-type atom_map3() :: #{a1 := kalle, atom() => atom()}.
-type type_shaddow_literal_map() :: #{atom() => atom(), a1 => 1}.
-type mandatory_type_map() :: #{atom() := atom()}.
-type empty_map() :: map().
-type map_with_tuple_value() :: #{atom() => tuple()}.
-type map_with_tuple_key() :: #{tuple() => atom()}.
-type map_with_fun_value() :: #{atom() => fun()}.
-type map_with_fun_key() :: #{fun() => atom()}.

map1_test() ->
    ?assertEqual({ok, #{a1 => 1}}, to_json_atom_map(#{a1 => 1})).

map1_bad_test() ->
    ?assertEqual({error,
                  [#ed_error{location = [a1],
                             type = type_mismatch,
                             ctx = #{type => #ed_simple_type{type = integer}, value => hej}}]},
                 to_json_atom_map(#{a1 => hej})),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 to_json_atom_map(<<"not_a_map">>)).

map2_test() ->
    ?assertEqual({ok, #{a1 => 1, other => value2}},
                 to_json_atom_map2(#{a1 => 1, other => value2})).

map2_bad_test() ->
    ?assertEqual({error,
                  [#ed_error{location = [a1],
                             type = type_mismatch,
                             ctx = #{type => #ed_literal{value = 1}, value => should_not_work}}]},
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
                             ctx = #{type => #ed_simple_type{type = atom}, value => 1}}]},
                 to_json_type_shaddow_literal_map(#{a1 => 1, some_atom => some_value})).

mandatory_type_map_test() ->
    ?assertEqual({ok, #{a1 => kalle}}, to_json_mandatory_type_map(#{a1 => kalle})),
    ?assertEqual({ok, #{a1 => undefined}}, to_json_mandatory_type_map(#{a1 => undefined})),
    ?assertEqual({ok, #{undefined => kalle}},
                 to_json_mandatory_type_map(#{undefined => kalle})).

mandatory_type_map_bad_test() ->
    ?assertEqual({error,
                  [#ed_error{location = [a1],
                             type = type_mismatch,
                             ctx = #{type => #ed_simple_type{type = atom}, value => 1}}]},
                 to_json_mandatory_type_map(#{a1 => 1})),
    ?assertEqual({error,
                  [#ed_error{location = [],
                             type = not_matched_fields,
                             ctx =
                                 #{type =>
                                       {map_field_type_exact,
                                        #ed_simple_type{type = atom},
                                        #ed_simple_type{type = atom}}}}]},
                 to_json_mandatory_type_map(#{})).

from_json_map1_test() ->
    ?assertEqual({ok, #{a1 => 123}}, from_json_atom_map(#{<<"a1">> => 123})).

from_json_map1_bad_test() ->
    ?assertEqual({error,
                  [#ed_error{location = [a1],
                             type = type_mismatch,
                             ctx =
                                 #{type => #ed_simple_type{type = integer},
                                   value => <<"not_an_integer">>}}]},
                 from_json_atom_map(#{<<"a1">> => <<"not_an_integer">>})),
    ?assertMatch({error, [#ed_error{type = type_mismatch}]},
                 from_json_atom_map(<<"not_a_map">>)).

from_json_map2_test() ->
    ?assertEqual({ok, #{a1 => 1, other => value2}},
                 from_json_atom_map2(#{<<"a1">> => 1, <<"other">> => <<"value2">>})).

from_json_map2_bad_test() ->
    ?assertEqual({error,
                  [#ed_error{location = [a1],
                             type = type_mismatch,
                             ctx = #{type => #ed_literal{value = 1}, value => 2}}]},
                 from_json_atom_map2(#{<<"a1">> => 2, <<"other">> => <<"value2">>})).

from_json_map3_test() ->
    ?assertEqual({ok, #{a1 => kalle}}, from_json_atom_map3(#{<<"a1">> => <<"kalle">>})).

from_json_map3_bad_test() ->
    ?assertEqual({error, [#ed_error{location = [a1], type = missing_data}]},
                 from_json_atom_map3(#{<<"not_a1">> => <<"kalle">>})).

from_json_type_shaddow_literal_map_test() ->
    ?assertEqual({ok, #{a1 => pelle, some_atom => some_value}},
                 from_json_type_shaddow_literal_map(#{<<"a1">> => <<"pelle">>,
                                                      <<"some_atom">> => <<"some_value">>})).

from_json_type_shaddow_literal_map_bad_test() ->
    ?assertEqual({error,
                  [#ed_error{location = [],
                             type = type_mismatch,
                             ctx = #{type => #ed_simple_type{type = map}, value => not_a_map}}]},
                 from_json_type_shaddow_literal_map(not_a_map)).

from_json_mandatory_type_map_test() ->
    ?assertEqual({ok, #{a1 => value}},
                 from_json_mandatory_type_map(#{<<"a1">> => <<"value">>})),
    ?assertEqual({ok, #{a1 => undefined}},
                 from_json_mandatory_type_map(#{<<"a1">> => <<"undefined">>})),
    ?assertEqual({ok, #{undefined => value}},
                 from_json_mandatory_type_map(#{<<"undefined">> => <<"value">>})).

from_json_mandatory_type_map_bad_test() ->
    ?assertEqual({error,
                  [#ed_error{location = [],
                             type = type_mismatch,
                             ctx = #{type => #ed_simple_type{type = map}, value => []}}]},
                 from_json_mandatory_type_map([])).

empty_map_test() ->
    ?assertEqual({ok, #{}}, to_json_empty_map(#{})),
    ?assertEqual({ok, #{a => 1}}, to_json_empty_map(#{a => 1})),
    ?assertEqual({ok, #{a => 1, b => 2}}, to_json_empty_map(#{a => 1, b => 2})),
    ?assertEqual({ok, #{a => 1, b => {a}}},
                 to_json_empty_map(#{a => 1, b => {a}}),
                 "All values are passed along when the type is term, even types that can not be converted to json by json.erl").

empty_map_bad_test() ->
    ?assertEqual({error,
                  [#ed_error{location = [],
                             type = type_mismatch,
                             ctx = #{type => #ed_simple_type{type = map}, value => not_a_map}}]},
                 to_json_empty_map(not_a_map)),
    ?assertEqual({error,
                  [#ed_error{location = [],
                             type = type_mismatch,
                             ctx = #{type => #ed_simple_type{type = map}, value => []}}]},
                 to_json_empty_map([])).

from_json_empty_map_test() ->
    ?assertEqual({ok, #{}}, from_json_empty_map(#{})),
    ?assertEqual({ok, #{<<"a">> => 1}}, from_json_empty_map(#{<<"a">> => 1})),
    ?assertEqual({ok, #{<<"a">> => <<"value">>, <<"b">> => <<"other">>}},
                 from_json_empty_map(#{<<"a">> => <<"value">>, <<"b">> => <<"other">>})),
    ?assertEqual({ok, #{<<"k1">> => 1, <<"k2">> => #{1 => 1}}},
                 to_json_empty_map(#{<<"k1">> => 1, <<"k2">> => #{1 => 1}})),
    ?assertEqual({ok, #{<<"k1">> => 1, <<"k2">> => <<"value">>}},
                 to_json_empty_map(#{<<"k1">> => 1, <<"k2">> => <<"value">>})).

from_json_empty_map_bad_test() ->
    ?assertEqual({error,
                  [#ed_error{location = [],
                             type = type_mismatch,
                             ctx = #{type => #ed_simple_type{type = map}, value => not_a_map}}]},
                 from_json_empty_map(not_a_map)),
    ?assertEqual({error,
                  [#ed_error{location = [],
                             type = type_mismatch,
                             ctx = #{type => #ed_simple_type{type = map}, value => []}}]},
                 from_json_empty_map([])).

map_with_tuple_value_test() ->
    ?assertError({type_not_supported, _}, to_json_map_with_tuple_value(#{a => {a}})),
    ?assertError({type_not_supported, _}, to_json_map_with_tuple_value(#{b => {1, 2, 3}})).

map_with_tuple_key_test() ->
    ?assertError({type_not_supported, _}, to_json_map_with_tuple_key(#{{a, b} => value})),
    ?assertError({type_not_supported, _}, to_json_map_with_tuple_key(#{{1, 2} => atom})).

map_with_fun_value_test() ->
    ?assertError({type_not_supported, _},
                 to_json_map_with_fun_value(#{a => fun() -> ok end})),
    ?assertError({type_not_supported, _},
                 to_json_map_with_fun_value(#{handler => fun(X) -> X + 1 end})).

map_with_fun_key_test() ->
    Fun = fun() -> ok end,
    ?assertError({type_not_supported, _}, to_json_map_with_fun_key(#{Fun => value})).

from_json_map_with_tuple_value_test() ->
    ?assertError({type_not_supported, _},
                 from_json_map_with_tuple_value(#{<<"a">> => [1, 2]})),
    ?assertError({type_not_supported, _},
                 from_json_map_with_tuple_value(#{<<"b">> => [1, 2, 3]})).

from_json_map_with_tuple_key_test() ->
    ?assertError({type_not_supported, _},
                 from_json_map_with_tuple_key(#{<<"key">> => <<"value">>})).

from_json_map_with_fun_value_test() ->
    ?assertError({type_not_supported, _},
                 from_json_map_with_fun_value(#{<<"a">> => <<"function">>})),
    ?assertError({type_not_supported, _},
                 from_json_map_with_fun_value(#{<<"handler">> => <<"callback">>})).

from_json_map_with_fun_key_test() ->
    ?assertError({type_not_supported, _},
                 from_json_map_with_fun_key(#{<<"key">> => <<"value">>})).

-spec to_json_mandatory_type_map(term()) ->
                                    {ok, mandatory_type_map()} | {error, [erldantic:error()]}.
to_json_mandatory_type_map(Data) ->
    erldantic_json:type_to_json(?MODULE, mandatory_type_map, Data).

-spec to_json_atom_map(term()) -> {ok, atom_map()} | {error, [erldantic:error()]}.
to_json_atom_map(Data) ->
    erldantic_json:type_to_json(?MODULE, atom_map, Data).

-spec to_json_atom_map2(term()) -> {ok, atom_map2()} | {error, [erldantic:error()]}.
to_json_atom_map2(Data) ->
    erldantic_json:type_to_json(?MODULE, atom_map2, Data).

-spec to_json_atom_map3(term()) -> {ok, atom_map3()} | {error, [erldantic:error()]}.
to_json_atom_map3(Data) ->
    erldantic_json:type_to_json(?MODULE, atom_map3, Data).

-spec to_json_type_shaddow_literal_map(term()) ->
                                          {ok, type_shaddow_literal_map()} |
                                          {error, [erldantic:error()]}.
to_json_type_shaddow_literal_map(Data) ->
    erldantic_json:type_to_json(?MODULE, type_shaddow_literal_map, Data).

-spec from_json_atom_map(term()) -> {ok, atom_map()} | {error, [erldantic:error()]}.
from_json_atom_map(Data) ->
    erldantic_json:type_from_json(?MODULE, atom_map, Data).

-spec from_json_atom_map2(term()) -> {ok, atom_map2()} | {error, [erldantic:error()]}.
from_json_atom_map2(Data) ->
    erldantic_json:type_from_json(?MODULE, atom_map2, Data).

-spec from_json_atom_map3(term()) -> {ok, atom_map3()} | {error, [erldantic:error()]}.
from_json_atom_map3(Data) ->
    erldantic_json:type_from_json(?MODULE, atom_map3, Data).

-spec from_json_type_shaddow_literal_map(term()) ->
                                            {ok, type_shaddow_literal_map()} |
                                            {error, [erldantic:error()]}.
from_json_type_shaddow_literal_map(Data) ->
    erldantic_json:type_from_json(?MODULE, type_shaddow_literal_map, Data).

-spec from_json_mandatory_type_map(term()) ->
                                      {ok, mandatory_type_map()} | {error, [erldantic:error()]}.
from_json_mandatory_type_map(Data) ->
    erldantic_json:type_from_json(?MODULE, mandatory_type_map, Data).

-spec to_json_empty_map(term()) -> {ok, empty_map()} | {error, [erldantic:error()]}.
to_json_empty_map(Data) ->
    erldantic_json:type_to_json(?MODULE, empty_map, Data).

-spec from_json_empty_map(term()) -> {ok, empty_map()} | {error, [erldantic:error()]}.
from_json_empty_map(Data) ->
    erldantic_json:type_from_json(?MODULE, empty_map, Data).

-spec to_json_map_with_tuple_value(term()) ->
                                      {ok, map_with_tuple_value()} | {error, [erldantic:error()]}.
to_json_map_with_tuple_value(Data) ->
    erldantic_json:type_to_json(?MODULE, map_with_tuple_value, Data).

-spec from_json_map_with_tuple_value(term()) ->
                                        {ok, map_with_tuple_value()} | {error, [erldantic:error()]}.
from_json_map_with_tuple_value(Data) ->
    erldantic_json:type_from_json(?MODULE, map_with_tuple_value, Data).

-spec to_json_map_with_tuple_key(term()) ->
                                    {ok, map_with_tuple_key()} | {error, [erldantic:error()]}.
to_json_map_with_tuple_key(Data) ->
    erldantic_json:type_to_json(?MODULE, map_with_tuple_key, Data).

-spec from_json_map_with_tuple_key(term()) ->
                                      {ok, map_with_tuple_key()} | {error, [erldantic:error()]}.
from_json_map_with_tuple_key(Data) ->
    erldantic_json:type_from_json(?MODULE, map_with_tuple_key, Data).

-spec to_json_map_with_fun_value(term()) ->
                                    {ok, map_with_fun_value()} | {error, [erldantic:error()]}.
to_json_map_with_fun_value(Data) ->
    erldantic_json:type_to_json(?MODULE, map_with_fun_value, Data).

-spec from_json_map_with_fun_value(term()) ->
                                      {ok, map_with_fun_value()} | {error, [erldantic:error()]}.
from_json_map_with_fun_value(Data) ->
    erldantic_json:type_from_json(?MODULE, map_with_fun_value, Data).

-spec to_json_map_with_fun_key(term()) ->
                                  {ok, map_with_fun_key()} | {error, [erldantic:error()]}.
to_json_map_with_fun_key(Data) ->
    erldantic_json:type_to_json(?MODULE, map_with_fun_key, Data).

-spec from_json_map_with_fun_key(term()) ->
                                    {ok, map_with_fun_key()} | {error, [erldantic:error()]}.
from_json_map_with_fun_key(Data) ->
    erldantic_json:type_from_json(?MODULE, map_with_fun_key, Data).
