-module(record_type_alias_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-record(person, {name :: string(), age :: integer()}).
-record(address, {street :: string(), city :: string()}).

-type person_alias() :: #person{name :: string(), age :: integer()}.
-type address_alias() :: #address{street :: string(), city :: string()}.
-type person_new_age() :: #person{age :: non_neg_integer()}.
-type person_t() :: #person{}.

type_in_form_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    {ok, PersonAliasType} = spectra_type_info:get_type(TypeInfo, person_alias, 0),
    ?assertMatch(
        #sp_rec_ref{
            record_name = person,
            field_types =
                [
                    {name, #sp_simple_type{type = string}},
                    {age, #sp_simple_type{type = integer}}
                ]
        },
        PersonAliasType
    ),

    {ok, AddressAliasType} = spectra_type_info:get_type(TypeInfo, address_alias, 0),
    ?assertMatch(
        #sp_rec_ref{
            record_name = address,
            field_types =
                [
                    {street, #sp_simple_type{type = string}},
                    {city, #sp_simple_type{type = string}}
                ]
        },
        AddressAliasType
    ),

    {ok, PersonNewAgeType} = spectra_type_info:get_type(TypeInfo, person_new_age, 0),
    ?assertMatch(
        #sp_rec_ref{
            record_name = person,
            field_types = [{age, #sp_simple_type{type = non_neg_integer}}]
        },
        PersonNewAgeType
    ),

    {ok, PersonTType} = spectra_type_info:get_type(TypeInfo, person_t, 0),
    ?assertMatch(#sp_rec_ref{record_name = person, field_types = []}, PersonTType).

to_json_person_record_test() ->
    Person = #person{name = "John", age = 30},
    ?assertEqual({ok, #{<<"name">> => <<"John">>, <<"age">> => 30}}, to_json_person(Person)).

to_json_person_record_bad_test() ->
    NotPersonArity = {person, "John"},
    ?assertMatch({error, [#sp_error{type = type_mismatch}]}, to_json_person(NotPersonArity)).

from_json_person_record_test() ->
    Person = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual({ok, #person{name = "John", age = 30}}, from_json_person(Person)).

to_json_person_alias_test() ->
    Person = #person{name = "John", age = -1},
    ?assertEqual({ok, #{<<"name">> => <<"John">>, <<"age">> => -1}}, to_json_person_alias(Person)).

to_json_person_alias_bad_test() ->
    Person = #person{name = "John", age = "not_an_integer"},
    ?assertEqual(
        {error, [
            #sp_error{
                location = [age],
                type = type_mismatch,
                ctx =
                    #{
                        type => #sp_simple_type{type = integer},
                        value => "not_an_integer"
                    }
            }
        ]},
        to_json_person_alias(Person)
    ).

to_json_person_new_age_test() ->
    Person = #person{name = "John", age = 0},
    ?assertEqual({ok, #{<<"name">> => <<"John">>, <<"age">> => 0}}, to_json_person_new_age(Person)).

to_json_person_new_age_bad_test() ->
    Person = #person{name = "John", age = -1},
    ?assertEqual(
        {error, [
            #sp_error{
                location = [age],
                type = type_mismatch,
                ctx =
                    #{type => #sp_simple_type{type = non_neg_integer}, value => -1}
            }
        ]},
        to_json_person_new_age(Person)
    ).

to_json_person_t_test() ->
    Person = #person{name = "John", age = 0},
    ?assertEqual({ok, #{<<"name">> => <<"John">>, <<"age">> => 0}}, to_json_person_t(Person)).

from_json_person_alias_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual({ok, #person{name = "John", age = 30}}, from_json_person_alias(Json)).

from_json_person_alias_bad_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => <<"not_an_integer">>},
    ?assertEqual(
        {error, [
            #sp_error{
                location = [age],
                type = type_mismatch,
                ctx =
                    #{
                        type => #sp_simple_type{type = integer},
                        value => <<"not_an_integer">>
                    }
            }
        ]},
        from_json_person_alias(Json)
    ).

from_json_person_new_age_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual({ok, #person{name = "John", age = 30}}, from_json_person_new_age(Json)).

from_json_person_new_age_bad_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => -1},
    ?assertEqual(
        {error, [
            #sp_error{
                location = [age],
                type = type_mismatch,
                ctx =
                    #{type => #sp_simple_type{type = non_neg_integer}, value => -1}
            }
        ]},
        from_json_person_new_age(Json)
    ).

from_json_person_t_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual({ok, #person{name = "John", age = 30}}, from_json_person_t(Json)).

from_json_person_t_bad_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => <<"not_an_integer">>},
    ?assertEqual(
        {error, [
            #sp_error{
                location = [age],
                type = type_mismatch,
                ctx =
                    #{
                        type => #sp_simple_type{type = integer},
                        value => <<"not_an_integer">>
                    }
            }
        ]},
        from_json_person_t(Json)
    ).

to_json_address_alias_test() ->
    Address = #address{street = "Main St", city = "Boston"},
    ?assertEqual(
        {ok, #{<<"street">> => <<"Main St">>, <<"city">> => <<"Boston">>}},
        to_json_address_alias(Address)
    ).

from_json_address_alias_test() ->
    Json = #{<<"street">> => <<"Main St">>, <<"city">> => <<"Boston">>},
    ?assertEqual(
        {ok, #address{street = "Main St", city = "Boston"}},
        from_json_address_alias(Json)
    ).

-spec to_json_person_new_age(person_new_age()) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
to_json_person_new_age(Data) ->
    spectra_json:to_json(?MODULE, {type, person_new_age, 0}, Data).

-spec to_json_person_t(person_t()) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
to_json_person_t(Data) ->
    spectra_json:to_json(?MODULE, {type, person_t, 0}, Data).

-spec to_json_person(#person{}) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
to_json_person(Person) ->
    spectra_json:to_json(?MODULE, {record, person}, Person).

-spec from_json_person(json:decode_value()) ->
    {ok, #person{}} | {error, [spectra:error()]}.
from_json_person(Person) ->
    spectra_json:from_json(?MODULE, {record, person}, Person).

-spec to_json_person_alias(term()) -> {ok, person_alias()} | {error, [spectra:error()]}.
to_json_person_alias(Data) ->
    spectra_json:to_json(?MODULE, {type, person_alias, 0}, Data).

-spec from_json_person_new_age(term()) ->
    {ok, person_new_age()} | {error, [spectra:error()]}.
from_json_person_new_age(Data) ->
    spectra_json:from_json(?MODULE, {type, person_new_age, 0}, Data).

-spec from_json_person_t(term()) -> {ok, person_t()} | {error, [spectra:error()]}.
from_json_person_t(Data) ->
    spectra_json:from_json(?MODULE, {type, person_t, 0}, Data).

-spec from_json_person_alias(term()) ->
    {ok, person_alias()} | {error, [spectra:error()]}.
from_json_person_alias(Data) ->
    spectra_json:from_json(?MODULE, {type, person_alias, 0}, Data).

-spec to_json_address_alias(term()) ->
    {ok, address_alias()} | {error, [spectra:error()]}.
to_json_address_alias(Data) ->
    spectra_json:to_json(?MODULE, {type, address_alias, 0}, Data).

-spec from_json_address_alias(term()) ->
    {ok, address_alias()} | {error, [spectra:error()]}.
from_json_address_alias(Data) ->
    spectra_json:from_json(?MODULE, {type, address_alias, 0}, Data).
