-module(opaque_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic_internal.hrl").

-compile(nowarn_unused_type).

-opaque person() :: #{name => binary(), age => pos_integer()}.

-record(my_rec, {id :: integer(), data}).

-opaque my_rec_t() :: #my_rec{data :: person()}.

-export_type([person/0, my_rec_t/0]).

simple_test() ->
    Types = erldantic_abstract_code:types_in_module(?MODULE),

    %% Normal opaque type
    ?assertEqual(#ed_map{fields =
                             [{map_field_assoc, name, #ed_simple_type{type = binary}},
                              {map_field_assoc, age, #ed_simple_type{type = pos_integer}}]},
                 maps:get({type, person, 0}, Types)),
    Person = #{name => <<"John">>, age => 42},
    ?assertEqual({ok, Person}, erldantic_json:type_to_json(?MODULE, person, Person)),
    ?assertEqual({ok, Person},
                 erldantic_json:type_from_json(?MODULE,
                                               person,
                                               #{<<"name">> => <<"John">>, <<"age">> => 42})),

    %% Opaque record type
    ?assertEqual(#ed_rec{name = my_rec,
                         fields =
                             [{id, #ed_simple_type{type = integer}},
                              {data, #ed_simple_type{type = term}}],
                         arity = 3},
                 maps:get({record, my_rec}, Types)),
    ?assertEqual(#ed_rec_ref{record_name = my_rec,
                             field_types =
                                 [{data, #ed_user_type_ref{type_name = person, variables = []}}]},
                 maps:get({type, my_rec_t, 0}, Types)),

    ?assertEqual({ok, #{id => 1, data => #{name => <<"John">>, age => 42}}},
                 erldantic_json:type_to_json(?MODULE,
                                             my_rec_t,
                                             #my_rec{id = 1,
                                                     data = #{name => <<"John">>, age => 42}})),
    ?assertEqual({ok, #my_rec{id = 1, data = #{name => <<"John">>, age => 42}}},
                 erldantic_json:type_from_json(?MODULE,
                                               my_rec_t,
                                               #{<<"id">> => 1,
                                                 <<"data">> =>
                                                     #{<<"name">> => <<"John">>,
                                                       <<"age">> => 42}})).
