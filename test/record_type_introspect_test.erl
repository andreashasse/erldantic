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
          person_person_to_json(),
          score(),
          score_to_json(),
          weird_union(),
          weird_union_to_json(),
          person_address(),
          person_address_undefined_city(),
          person_address_undefined_city_to_json(),
          person_address_undefined_street(),
          person_address_undefined_street_to_json(),
          person_address_to_json(),
          name_t(),
          name_t_to_json()]}
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

score() ->
    Json =
        json:decode(<<"{\"value\": 5, \"comment\": {\"lang\": \"en\", \"text\": \"ok\"}}"/utf8>>),
    {ok, #{value := Value, comment := Comment}} = person:score_from_json(Json),
    [?_assertEqual(5, Value), ?_assertEqual(#{lang => <<"en">>, text => <<"ok">>}, Comment)].

score_to_json() ->
    Data = #{value => 5, comment => #{lang => <<"en">>, text => <<"ok">>}},
    {ok, Json} = person:score_to_json(Data),
    Expect = #{value => 5, comment => #{lang => <<"en">>, text => <<"ok">>}},
    [?_assertEqual(Expect, Json)].

weird_union() ->
    Json =
        json:decode(<<"{\"city\": \"sollentuna\", \"score\": {\"value\": 5, "
                      "\"comment\": {\"lang\": \"en\", \"text\": \"ok\"}}}"/utf8>>),
    {ok, #{city := City, score := Score}} = person:weird_union_from_json(Json),
    [?_assertEqual(<<"sollentuna">>, City),
     ?_assertEqual(5, maps:get(value, Score)),
     ?_assertEqual(#{lang => <<"en">>, text => <<"ok">>}, maps:get(comment, Score))].

weird_union_to_json() ->
    Data = #{city => <<"sollentuna">>,
             score => #{value => 5, comment => #{lang => <<"en">>, text => <<"ok">>}}},
    {ok, Json} = person:weird_union_to_json(Data),
    Expect = #{city => <<"sollentuna">>,
               score => #{value => 5, comment => #{lang => <<"en">>, text => <<"ok">>}}},
    [?_assertEqual(Expect, Json)].

person_person_to_json() ->
    Address = #address{street = <<"mojs">>, city = <<"sollentuna">>},
    Name = #{first => <<"Andreas">>, last => <<"Hasselberg">>},
    Person =
        #person{name = Name,
                age = 22,
                home = Address},
    {ok, Json} = person:person_to_json(Person),
    Expect =
        #{name => #{first => <<"Andreas">>, last => <<"Hasselberg">>},
          age => 22,
          home => #{street => <<"mojs">>, city => <<"sollentuna">>}},
    [?_assertEqual(Expect, Json)].

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

person_address_undefined_street_to_json() ->
    Data = #address{street = undefined, city = <<"sollentuna">>},
    Expr = person:address_to_json(Data),
    [?_assertMatch({error, _}, Expr)].

name_t() ->
    Json =
        json:decode(<<"{\"first\": \"Andreas\", \"last\": \"Hasselberg\", \"not_present\": "
                      "22}"/utf8>>),
    {ok, M} = person:name_t_from_json(Json),
    Expect = #{first => <<"Andreas">>, last => <<"Hasselberg">>},
    [?_assertEqual(Expect, M), ?_assertEqual([first, last], maps:keys(M))].

name_t_to_json() ->
    Data =
        #{first => <<"Andreas">>,
          last => <<"Hasselberg">>,
          not_present => 22},
    Resp = person:name_t_to_json(Data),
    Expected = {ok, #{first => <<"Andreas">>, last => <<"Hasselberg">>}},
    [?_assertEqual(Resp, Expected)].
