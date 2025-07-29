-module(record_type_alias_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-record(person, {name :: string(), age :: integer()}).
-record(address, {street :: string(), city :: string()}).

-type person_alias() :: #person{name :: string(), age :: integer()}.
-type address_alias() :: #address{street :: string(), city :: string()}.
-type person_new_age() :: #person{age :: non_neg_integer()}.
-type person_t() :: #person{}.

type_in_form_test() ->
    Types = erldantic_abstract_code:types_in_module(?MODULE),

    ?assertMatch(#ed_rec_ref{record_name = person,
                             field_types =
                                 [{name, #ed_simple_type{type = string}},
                                  {age, #ed_simple_type{type = integer}}]},
                 maps:get({type, person_alias, 0}, Types)),

    ?assertMatch(#ed_rec_ref{record_name = address,
                             field_types =
                                 [{street, #ed_simple_type{type = string}},
                                  {city, #ed_simple_type{type = string}}]},
                 maps:get({type, address_alias, 0}, Types)),

    ?assertMatch(#ed_rec_ref{record_name = person,
                             field_types = [{age, #ed_simple_type{type = non_neg_integer}}]},
                 maps:get({type, person_new_age, 0}, Types)),

    ?assertMatch(#ed_rec_ref{record_name = person, field_types = []},
                 maps:get({type, person_t, 0}, Types)).

to_json_person_record_test() ->
    Person = #person{name = "John", age = 30},
    ?assertEqual({ok, #{name => <<"John">>, age => 30}}, to_json_person(Person)).

to_json_person_record_bad_test() ->
    NotPersonArity = {person, "John"},
    ?assertMatch({error, [#ed_error{type = type_mismatch}]}, to_json_person(NotPersonArity)).

from_json_person_record_test() ->
    Person = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual({ok, #person{name = "John", age = 30}}, from_json_person(Person)).

to_json_person_alias_test() ->
    Person = #person{name = "John", age = -1},
    ?assertEqual({ok, #{name => <<"John">>, age => -1}}, to_json_person_alias(Person)).

to_json_person_alias_bad_test() ->
    Person = #person{name = "John", age = "not_an_integer"},
    ?assertEqual({error,
                  [#ed_error{location = [age],
                             type = type_mismatch,
                             ctx =
                                 #{type => #ed_simple_type{type = integer},
                                   value => "not_an_integer"}}]},
                 to_json_person_alias(Person)).

to_json_person_new_age_test() ->
    Person = #person{name = "John", age = 0},
    ?assertEqual({ok, #{name => <<"John">>, age => 0}}, to_json_person_new_age(Person)).

to_json_person_new_age_bad_test() ->
    Person = #person{name = "John", age = -1},
    ?assertEqual({error,
                  [#ed_error{location = [age],
                             type = type_mismatch,
                             ctx =
                                 #{type => #ed_simple_type{type = non_neg_integer}, value => -1}}]},
                 to_json_person_new_age(Person)).

to_json_person_t_test() ->
    Person = #person{name = "John", age = 0},
    ?assertEqual({ok, #{name => <<"John">>, age => 0}}, to_json_person_t(Person)).

from_json_person_alias_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual({ok, #person{name = "John", age = 30}}, from_json_person_alias(Json)).

from_json_person_alias_bad_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => <<"not_an_integer">>},
    ?assertEqual({error,
                  [#ed_error{location = [age],
                             type = type_mismatch,
                             ctx =
                                 #{type => #ed_simple_type{type = integer},
                                   value => <<"not_an_integer">>}}]},
                 from_json_person_alias(Json)).

from_json_person_new_age_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual({ok, #person{name = "John", age = 30}}, from_json_person_new_age(Json)).

from_json_person_new_age_bad_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => -1},
    ?assertEqual({error,
                  [#ed_error{location = [age],
                             type = type_mismatch,
                             ctx =
                                 #{type => #ed_simple_type{type = non_neg_integer}, value => -1}}]},
                 from_json_person_new_age(Json)).

from_json_person_t_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual({ok, #person{name = "John", age = 30}}, from_json_person_t(Json)).

from_json_person_t_bad_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => <<"not_an_integer">>},
    ?assertEqual({error,
                  [#ed_error{location = [age],
                             type = type_mismatch,
                             ctx =
                                 #{type => #ed_simple_type{type = integer},
                                   value => <<"not_an_integer">>}}]},
                 from_json_person_t(Json)).

to_json_address_alias_test() ->
    Address = #address{street = "Main St", city = "Boston"},
    ?assertEqual({ok, #{street => <<"Main St">>, city => <<"Boston">>}},
                 to_json_address_alias(Address)).

from_json_address_alias_test() ->
    Json = #{<<"street">> => <<"Main St">>, <<"city">> => <<"Boston">>},
    ?assertEqual({ok, #address{street = "Main St", city = "Boston"}},
                 from_json_address_alias(Json)).

-spec to_json_person_new_age(person_new_age()) ->
                                {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json_person_new_age(Data) ->
    erldantic_json:type_to_json(?MODULE, person_new_age, Data).

-spec to_json_person_t(person_t()) ->
                          {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json_person_t(Data) ->
    erldantic_json:type_to_json(?MODULE, person_t, Data).

-spec to_json_person(#person{}) ->
                        {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json_person(Person) ->
    erldantic_json:record_to_json(?MODULE, person, Person).

-spec from_json_person(json:decode_value()) ->
                          {ok, #person{}} | {error, [erldantic:error()]}.
from_json_person(Person) ->
    erldantic_json:record_from_json(?MODULE, person, Person).

-spec to_json_person_alias(term()) -> {ok, person_alias()} | {error, [erldantic:error()]}.
to_json_person_alias(Data) ->
    erldantic_json:type_to_json(?MODULE, person_alias, Data).

-spec from_json_person_new_age(term()) ->
                                  {ok, person_new_age()} | {error, [erldantic:error()]}.
from_json_person_new_age(Data) ->
    erldantic_json:type_from_json(?MODULE, person_new_age, Data).

-spec from_json_person_t(term()) -> {ok, person_t()} | {error, [erldantic:error()]}.
from_json_person_t(Data) ->
    erldantic_json:type_from_json(?MODULE, person_t, Data).

-spec from_json_person_alias(term()) ->
                                {ok, person_alias()} | {error, [erldantic:error()]}.
from_json_person_alias(Data) ->
    erldantic_json:type_from_json(?MODULE, person_alias, Data).

-spec to_json_address_alias(term()) ->
                               {ok, address_alias()} | {error, [erldantic:error()]}.
to_json_address_alias(Data) ->
    erldantic_json:type_to_json(?MODULE, address_alias, Data).

-spec from_json_address_alias(term()) ->
                                 {ok, address_alias()} | {error, [erldantic:error()]}.
from_json_address_alias(Data) ->
    erldantic_json:type_from_json(?MODULE, address_alias, Data).
