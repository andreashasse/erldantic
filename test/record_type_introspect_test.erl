-module(record_type_introspect_test).

-include_lib("eunit/include/eunit.hrl").

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
          person_address_undefined_street(),
          name_t()]}
     end}.

compile_person() ->
    c:c("person.erl").

noop(_) ->
    ok.

person_type_is_record() ->
    Json = json:decode(<<"{\"street\": \"mojs\", \"city\": \"sollentuna\"}"/utf8>>),
    [?_assertEqual({ok, {address, <<"mojs">>, <<"sollentuna">>}},
                   person:address_from_json(Json))].

person_person() ->
    Json =
        json:decode(<<"{\"name\": {\"first\": \"Andreas\", \"last\": \"Hasselberg\"}, "
                      "\"age\": 22, \"home\": {\"street\": \"mojs\", \"city\": \"sollentuna\""
                      "}}"/utf8>>),
    {ok, {person, Name, Age, Home}} = person:person_from_json(Json),
    [?_assertEqual(#{first => <<"Andreas">>, last => <<"Hasselberg">>}, Name),
     ?_assertEqual(22, Age),
     ?_assertEqual({address, <<"mojs">>, <<"sollentuna">>}, Home)].

person_address() ->
    Json = json:decode(<<"{\"street\": \"mojs\", \"city\": \"sollentuna\"}"/utf8>>),
    Expect = {ok, {address, <<"mojs">>, <<"sollentuna">>}},
    Expr = person:address_from_json(Json),
    [?_assertEqual(Expect, Expr)].

person_address_undefined_city() ->
    Json = json:decode(<<"{\"street\": \"mojs\"}"/utf8>>),
    Expect = {ok, {address, <<"mojs">>, undefined}},
    Expr = person:address_from_json(Json),
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
