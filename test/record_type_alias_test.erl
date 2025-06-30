-module(record_type_alias_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/record_type_introspect.hrl").

-record(person, {name :: string(), age :: integer()}).
-record(address, {street :: string(), city :: string()}).

%% Record type alias - this should trigger the uncovered code path
%% in erldantic_abstract_code.erl lines 32-44
-type person_alias() :: #person{name :: string(), age :: integer()}.
-type address_alias() :: #address{street :: string(), city :: string()}.

type_in_form_test() ->
    {ok, Types} = erldantic_abstract_code:types_in_module(?MODULE),

    %% Verify the record type alias is properly parsed
    ?assertMatch(#a_rec{name = person,
                        fields = [{name, {type, string}}, {age, {type, integer}}]},
                 maps:get({type, person_alias, 0}, Types)),

    %% Verify the address record type alias is also parsed
    ?assertMatch(#a_rec{name = address,
                        fields = [{street, {type, string}}, {city, {type, string}}]},
                 maps:get({type, address_alias, 0}, Types)).

to_json_person_alias_test() ->
    Person = #person{name = "John", age = 30},
    ?assertEqual({ok, #{name => <<"John">>, age => 30}}, to_json_person_alias(Person)).

to_json_person_alias_bad_test() ->
    Person = #person{name = "John", age = "not_an_integer"},
    ?assertEqual({error,
                  [#ed_error{location = [age],
                             type = type_mismatch,
                             ctx = #{type => {type, integer}, value => "not_an_integer"}}]},
                 to_json_person_alias(Person)).

from_json_person_alias_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual({ok, #person{name = "John", age = 30}}, from_json_person_alias(Json)).

from_json_person_alias_bad_test() ->
    Json = #{<<"name">> => <<"John">>, <<"age">> => <<"not_an_integer">>},
    ?assertEqual({error,
                  [#ed_error{location = [age],
                             type = type_mismatch,
                             ctx = #{type => {type, integer}, value => <<"not_an_integer">>}}]},
                 from_json_person_alias(Json)).

to_json_address_alias_test() ->
    Address = #address{street = "Main St", city = "Boston"},
    ?assertEqual({ok, #{street => <<"Main St">>, city => <<"Boston">>}},
                 to_json_address_alias(Address)).

from_json_address_alias_test() ->
    Json = #{<<"street">> => <<"Main St">>, <<"city">> => <<"Boston">>},
    ?assertEqual({ok, #address{street = "Main St", city = "Boston"}},
                 from_json_address_alias(Json)).

-spec to_json_person_alias(term()) -> {ok, person_alias()} | {error, [#ed_error{}]}.
to_json_person_alias(Data) ->
    erldantic_json:to_json_no_pt({?MODULE, person_alias, 0}, Data).

-spec from_json_person_alias(term()) -> {ok, person_alias()} | {error, [#ed_error{}]}.
from_json_person_alias(Data) ->
    erldantic_json:from_json_no_pt({?MODULE, person_alias, 0}, Data).

-spec to_json_address_alias(term()) -> {ok, address_alias()} | {error, [#ed_error{}]}.
to_json_address_alias(Data) ->
    erldantic_json:to_json_no_pt({?MODULE, address_alias, 0}, Data).

-spec from_json_address_alias(term()) -> {ok, address_alias()} | {error, [#ed_error{}]}.
from_json_address_alias(Data) ->
    erldantic_json:from_json_no_pt({?MODULE, address_alias, 0}, Data).
