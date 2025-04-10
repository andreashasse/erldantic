-module(record_type_introspect_test).

-include_lib("eunit/include/eunit.hrl").

-include("person.hrl").

person_module_test_() ->
    {setup,
     fun compile_person/0,
     fun noop/1,
     fun(_SetupData) ->
        {inparallel,
         [person_type_is_record(),
          person_person(),
          person_address(),
          person_address_undefined_city(),
          person_address_undefined_city_to_json(),
          person_address_undefined_street(),
          person_address_to_json(),
          name_t()]}
     end}.

compile_person() ->
    {ok, person} = c:c("test/person.erl"),
    true = erlang:function_exported(person, address_from_json, 1),
    %true = erlang:function_exported(person, address_to_json, 1),
    io:format("~p", [c:m(person)]).

noop(_) ->
    ok.

person_type_is_record() ->
    Json = json:decode(<<"{\"street\": \"mojs\", \"city\": \"sollentuna\"}"/utf8>>),
    [?_assertEqual({ok, #address{street = <<"mojs">>, city = <<"sollentuna">>}},
                   person:address_from_json(Json))].

person_person() ->
    Json =
        json:decode(<<"{\"name\": {\"first\": \"Andreas\", \"last\": \"Hasselberg\"}, "
                      "\"age\": 22, \"home\": {\"street\": \"mojs\", \"city\": \"sollentuna\""
                      "}}"/utf8>>),
    {ok,
     #person{name = Name,
             age = Age,
             home = Home}} =
        person:person_from_json(Json),
    [?_assertEqual(#{first => <<"Andreas">>, last => <<"Hasselberg">>}, Name),
     ?_assertEqual(22, Age),
     ?_assertEqual(#address{street = <<"mojs">>, city = <<"sollentuna">>}, Home)].

person_address() ->
    Json = json:decode(<<"{\"street\": \"mojs\", \"city\": \"sollentuna\"}"/utf8>>),
    Expect = {ok, #address{street = <<"mojs">>, city = <<"sollentuna">>}},
    Expr = person:address_from_json(Json),
    [?_assertEqual(Expect, Expr)].

person_address_to_json() ->
    Address = #address{street = <<"mojs">>, city = <<"sollentuna">>},
    {ok, Json} = person:address_to_json(Address),
    [?_assertEqual(#{street => <<"mojs">>, city => <<"sollentuna">>}, Json)].

person_address_undefined_city() ->
    Json = json:decode(<<"{\"street\": \"mojs\"}"/utf8>>),
    Expect = {ok, #address{street = <<"mojs">>, city = undefined}},
    Expr = person:address_from_json(Json),
    [?_assertEqual(Expect, Expr)].

person_address_undefined_city_to_json() ->
    Data = #address{street = <<"mojs">>, city = undefined},
    Expect = {ok, #{street => <<"mojs">>}},
    Expr = person:address_to_json(Data),
    [?_assertEqual(Expect, Expr)].

person_address_undefined_street() ->
    Json = json:decode(<<"{\"city\": \"sollentuna\"}"/utf8>>),
    Expr = person:address_from_json(Json),
    [?_assertMatch({error, _}, Expr)].

name_t() ->
    Json =
        json:decode(<<"{\"first\": \"Andreas\", \"last\": \"Hasselberg\", \"not_present\": "
                      "22}"/utf8>>),
    {ok, M} = person:name_t_from_json(Json),
    Expect = #{first => <<"Andreas">>, last => <<"Hasselberg">>},
    [?_assertEqual(Expect, M), ?_assertEqual([first, last], maps:keys(M))].
