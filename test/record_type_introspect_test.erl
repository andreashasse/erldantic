-module(record_type_introspect_test).

-include_lib("eunit/include/eunit.hrl").

-include("person.hrl").
-include("record_type_introspect.hrl").

person_module_test_() ->
    {setup,
     fun compile_person/0,
     fun noop/1,
     fun(_SetupData) ->
        {inparallel,
         [active(), active_bad(), active_to_json(), active_to_json_bad(), age(), age_bad(),
          age_to_json(), person_type_is_record(), person_person(), person_person_bad(),
          person_person_to_json(), score(), score_bad(), score_value_bad(), score_to_json(),
          score_to_json_value_bad(), weird_union(), weird_union_bad(), weird_union_to_json(),
          person_address(), person_address_bad(), person_address_undefined_city(),
          person_address_undefined_city_to_json(), person_address_undefined_street(),
          person_address_undefined_street_to_json(), person_address_to_json(), level(), level_bad(),
          level_to_json(), level_to_json_bad(), negative(), negative_bad(), negative_to_json(),
          negative_to_json_bad(), accesses_test(), accesses_test_to_json(), tup_list_test(),
          tup_list_test_bad(), tup_list_test_to_json(), tup_list_test_to_json_bad(), name_t(),
          name_t_error(), name_t_to_json(), name_t_to_json_error(), temp(), temp_bad(),
          temp_to_json(), temp_to_json_bad(), role(), role_bad(), role_to_json(),
          role_to_json_bad(), non_atom_enum(), non_atom_enum_bad(), non_atom_enum_to_json(),
          non_atom_enum_to_json_bad(), missing(), missing_to_json(), remote(), remote_bad(),
          remote_to_json(), remote_to_json_bad(), binary_data(), binary_data_bad(),
          binary_data_to_json(), binary_data_to_json_bad(), binary_map(), binary_map_bad(),
          binary_map_to_json(), binary_map_to_json_bad(), string_type(), string_type_bad(),
          string_type_to_json(), string_type_to_json_bad(), int_list_map(), int_list_map_bad(),
          int_list_map_to_json(), int_list_map_to_json_bad()]}
     end}.

compile_person() ->
    {ok, person} = c:c("test/person.erl"),
    {ok, other} = c:c("test/other.erl", [debug_info]),
    true = erlang:function_exported(person, address_from_json, 1),
    %true = erlang:function_exported(person, address_to_json, 1),
    io:format("~p", [c:m(person)]).

noop(_) ->
    ok.

active() ->
    Json = json:decode(<<"true"/utf8>>),
    [?_assertEqual({ok, true}, person:active_from_json(Json))].

active_bad() ->
    Json = json:decode(<<"1"/utf8>>),
    [?_assertEqual({error,
                    [{ed_error, [], type_mismatch, #{type => {type, boolean}, value => 1}}]},
                   person:active_from_json(Json))].

active_to_json() ->
    Data = true,
    {ok, Json} = person:active_to_json(Data),
    [?_assertEqual(true, Json)].

active_to_json_bad() ->
    Data = 1,
    [?_assertEqual({error,
                    [{ed_error, [], type_mismatch, #{type => {type, boolean}, value => 1}}]},
                   person:active_to_json(Data))].

age() ->
    Json = json:decode(<<"22"/utf8>>),
    [?_assertEqual({ok, 22}, person:age_from_json(Json))].

age_bad() ->
    Json = json:decode(<<"-12"/utf8>>),
    [?_assertEqual({error,
                    [{ed_error,
                      [],
                      type_mismatch,
                      #{type => {type, non_neg_integer}, value => -12}}]},
                   person:age_from_json(Json))].

age_to_json() ->
    Data = 22,
    {ok, Json} = person:age_to_json(Data),
    Expect = 22,
    [?_assertEqual(Expect, Json)].

person_type_is_record() ->
    Json = json:decode(<<"{\"street\": \"mojs\", \"city\": \"sollentuna\"}"/utf8>>),
    [?_assertEqual({ok, #address{street = "mojs", city = "sollentuna"}},
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
    [?_assertEqual(#{first => "Andreas", last => "Hasselberg"}, Name),
     ?_assertEqual(22, Age),
     ?_assertEqual(#address{street = "mojs", city = "sollentuna"}, Home)].

person_person_bad() ->
    Json =
        json:decode(<<"{\"name\": {\"first\": \"Andreas\", \"last\": \"Hasselberg\"}, "
                      "\"age\": 22, \"home\": {\"city\": \"sollentuna\""
                      "}}"/utf8>>),
    [?_assertEqual({error, [#ed_error{type = missing_data, location = [home, street]}]},
                   person:person_from_json(Json))].

score() ->
    Json =
        json:decode(<<"{\"value\": 5, \"comment\": {\"lang\": \"en\", \"text\": \"ok\"}}"/utf8>>),
    {ok, #{value := Value, comment := Comment}} = person:score_from_json(Json),
    [?_assertEqual(5, Value), ?_assertEqual(#{lang => "en", text => "ok"}, Comment)].

score_bad() ->
    Json =
        json:decode(<<"{\"value\": 5, \"comment\": {\"lang\": \"en\", \"text\": 5}}"/utf8>>),
    [?_assertEqual({error,
                    [#ed_error{type = type_mismatch,
                               location = [comment, text],
                               ctx = #{type => {type, string}, value => 5}}]},
                   person:score_from_json(Json))].

score_value_bad() ->
    Json =
        json:decode(<<"{\"value\": 11, \"comment\": {\"lang\": \"en\", \"text\": \"ok\"}}"/utf8>>),
    [?_assertEqual({error,
                    [{ed_error,
                      [value],
                      type_mismatch,
                      #{type => {range, integer, 1, 10}, value => 11}}]},
                   person:score_from_json(Json))].

score_to_json() ->
    Data = #{value => 5, comment => #{lang => "en", text => "ok"}},
    {ok, Json} = person:score_to_json(Data),
    Expect = #{value => 5, comment => #{lang => <<"en">>, text => <<"ok">>}},
    [?_assertEqual(Expect, Json)].

score_to_json_value_bad() ->
    Data = #{value => 11, comment => #{lang => "en", text => "ok"}},
    [?_assertEqual({error,
                    [{ed_error,
                      [value],
                      type_mismatch,
                      #{type => {range, integer, 1, 10}, value => 11}}]},
                   person:score_to_json(Data))].

weird_union() ->
    Json =
        json:decode(<<"{\"city\": \"sollentuna\", \"score\": {\"value\": 5, "
                      "\"comment\": {\"lang\": \"en\", \"text\": \"ok\"}}}"/utf8>>),
    {ok, #{city := City, score := Score}} = person:weird_union_from_json(Json),
    [?_assertEqual("sollentuna", City),
     ?_assertEqual(5, maps:get(value, Score)),
     ?_assertEqual(#{lang => "en", text => "ok"}, maps:get(comment, Score))].

weird_union_bad() ->
    Json =
        json:decode(<<"{\"city\": \"sollentuna\", \"score\": {\"value\": 5, "
                      "\"comment\": {\"lang\": \"en\", \"text\": 5}}}"/utf8>>),
    [?_assertMatch({error, [#ed_error{type = no_match, location = []}]},
                   person:weird_union_from_json(Json))].

weird_union_to_json() ->
    Data =
        #{city => "sollentuna", score => #{value => 5, comment => #{lang => "en", text => "ok"}}},
    {ok, Json} = person:weird_union_to_json(Data),
    Expect =
        #{city => <<"sollentuna">>,
          score => #{value => 5, comment => #{lang => <<"en">>, text => <<"ok">>}}},
    [?_assertEqual(Expect, Json)].

person_person_to_json() ->
    Address = #address{street = "mojs", city = "sollentuna"},
    Name = #{first => "Andreas", last => "Hasselberg"},
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
    Expect = {ok, #address{street = "mojs", city = "sollentuna"}},
    Expr = person:address_from_json(Json),
    [?_assertEqual(Expect, Expr)].

person_address_bad() ->
    Json = json:decode(<<"{\"street\": \"mojs\", \"city\": 5}"/utf8>>),
    [?_assertEqual({error,
                    [#ed_error{type = no_match,
                               location = [city],
                               ctx =
                                   #{type => [{type, string}, {literal, undefined}], value => 5}}]},
                   person:address_from_json(Json))].

person_address_to_json() ->
    Address = #address{street = "mojs", city = "sollentuna"},
    {ok, Json} = person:address_to_json(Address),
    [?_assertEqual(#{street => <<"mojs">>, city => <<"sollentuna">>}, Json)].

accesses_test() ->
    Json = json:decode(<<"[\"read\", \"write\"]"/utf8>>),
    [?_assertEqual({ok, [read, write]}, person:accesses_from_json(Json))].

accesses_test_to_json() ->
    Data = [read, write],
    {ok, Json} = person:accesses_to_json(Data),
    Expect = [read, write],
    [?_assertEqual(Expect, Json)].

tup_list_test() ->
    Json = json:decode(<<"{\"a\": [1, 2, 3]}"/utf8>>),
    [?_assertEqual({ok, #{a => [1, 2, 3]}}, person:tup_list_from_json(Json))].

tup_list_test_bad() ->
    Json = json:decode(<<"{\"a\": [1, \"p\", 3]}"/utf8>>),
    [?_assertEqual({error,
                    [#ed_error{type = type_mismatch,
                               location = [a, 2],
                               ctx = #{type => {type, integer}, value => <<"p">>}}]},
                   person:tup_list_from_json(Json))].

tup_list_test_to_json() ->
    Data = #{a => [1, 2, 3]},
    {ok, Json} = person:tup_list_to_json(Data),
    Expect = #{a => [1, 2, 3]},
    [?_assertEqual(Expect, Json)].

tup_list_test_to_json_bad() ->
    Data = #{a => [1, <<"p">>, 3]},
    [?_assertEqual({error,
                    [#ed_error{type = type_mismatch,
                               location = [a, 2],
                               ctx = #{type => {type, integer}, value => <<"p">>}}]},
                   person:tup_list_to_json(Data))].

person_address_undefined_city() ->
    Json = json:decode(<<"{\"street\": \"mojs\"}"/utf8>>),
    Expect = {ok, #address{street = "mojs", city = undefined}},
    Expr = person:address_from_json(Json),
    [?_assertEqual(Expect, Expr)].

person_address_undefined_city_to_json() ->
    Data = #address{street = "mojs", city = undefined},
    Expect = {ok, #{street => <<"mojs">>}},
    Expr = person:address_to_json(Data),
    [?_assertEqual(Expect, Expr)].

level() ->
    Json = json:decode(<<"5"/utf8>>),
    [?_assertEqual({ok, 5}, person:level_from_json(Json))].

level_bad() ->
    Json = json:decode(<<"-5"/utf8>>),
    [?_assertEqual({error,
                    [{ed_error, [], type_mismatch, #{type => {type, pos_integer}, value => -5}}]},
                   person:level_from_json(Json))].

level_to_json() ->
    Data = 5,
    {ok, Json} = person:level_to_json(Data),
    Expect = 5,
    [?_assertEqual(Expect, Json)].

level_to_json_bad() ->
    Data = -5,
    [?_assertEqual({error,
                    [{ed_error, [], type_mismatch, #{type => {type, pos_integer}, value => -5}}]},
                   person:level_to_json(Data))].

negative() ->
    Json = json:decode(<<"-5"/utf8>>),
    [?_assertEqual({ok, -5}, person:negative_from_json(Json))].

negative_bad() ->
    Json = json:decode(<<"5"/utf8>>),
    [?_assertEqual({error,
                    [{ed_error, [], type_mismatch, #{type => {type, neg_integer}, value => 5}}]},
                   person:negative_from_json(Json))].

negative_to_json() ->
    Data = -5,
    {ok, Json} = person:negative_to_json(Data),
    Expect = -5,
    [?_assertEqual(Expect, Json)].

negative_to_json_bad() ->
    Data = 5,
    [?_assertEqual({error,
                    [{ed_error, [], type_mismatch, #{type => {type, neg_integer}, value => 5}}]},
                   person:negative_to_json(Data))].

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
    Expect = #{first => "Andreas", last => "Hasselberg"},
    [?_assertEqual(Expect, M), ?_assertEqual([first, last], maps:keys(M))].

name_t_error() ->
    Json = json:decode(<<"{\"first\": \"Andreas\"}"/utf8>>),
    [?_assertEqual({error, [#ed_error{type = missing_data, location = [last]}]},
                   person:name_t_from_json(Json))].

name_t_to_json() ->
    Data =
        #{first => "Andreas",
          last => "Hasselberg",
          not_present => 22},
    Resp = person:name_t_to_json(Data),
    Expected = {ok, #{first => <<"Andreas">>, last => <<"Hasselberg">>}},
    [?_assertEqual(Resp, Expected)].

name_t_to_json_error() ->
    Data = #{first => "Andreas"},
    Resp = person:name_t_to_json(Data),
    Expected = {error, [#ed_error{type = missing_data, location = [last]}]},
    [?_assertEqual(Resp, Expected)].

temp() ->
    Json = json:decode(<<"3.14"/utf8>>),
    [?_assertEqual({ok, 3.14}, person:temp_from_json(Json))].

temp_bad() ->
    Json = json:decode(<<"\"not_a_float\""/utf8>>),
    [?_assertEqual({error,
                    [{ed_error,
                      [],
                      type_mismatch,
                      #{type => {type, float}, value => <<"not_a_float">>}}]},
                   person:temp_from_json(Json))].

temp_to_json() ->
    Data = 3.14,
    {ok, Json} = person:temp_to_json(Data),
    Expect = 3.14,
    [?_assertEqual(Expect, Json)].

temp_to_json_bad() ->
    Data = 42,
    [?_assertEqual({error,
                    [{ed_error, [], type_mismatch, #{type => {type, float}, value => 42}}]},
                   person:temp_to_json(Data))].

role() ->
    Json = json:decode(<<"\"admin\""/utf8>>),
    [?_assertEqual({ok, admin}, person:role_from_json(Json))].

role_bad() ->
    Json = json:decode(<<"\"invalid_role\""/utf8>>),
    [?_assertEqual({error,
                    [{ed_error,
                      [],
                      no_match,
                      #{type => [{literal, admin}, {literal, user}, {literal, guest}],
                        value => <<"invalid_role">>}}]},
                   person:role_from_json(Json))].

role_to_json() ->
    Data = admin,
    {ok, Json} = person:role_to_json(Data),
    Expect = admin,
    [?_assertEqual(Expect, Json)].

role_to_json_bad() ->
    Data = invalid_role,
    [?_assertEqual({error,
                    [{ed_error,
                      [],
                      no_match,
                      #{type => [{literal, admin}, {literal, user}, {literal, guest}],
                        value => invalid_role}}]},
                   person:role_to_json(Data))].

non_atom_enum() ->
    Json = json:decode(<<"1"/utf8>>),
    [?_assertEqual({ok, 1}, person:non_atom_enum_from_json(Json))].

non_atom_enum_bad() ->
    Json = json:decode(<<"2"/utf8>>),
    [?_assertEqual({error,
                    [{ed_error,
                      [],
                      no_match,
                      #{type => [{literal, 1}, {literal, 3}], value => 2}}]},
                   person:non_atom_enum_from_json(Json))].

non_atom_enum_to_json() ->
    Value = 1,
    [?_assertEqual({ok, 1}, person:non_atom_enum_to_json(Value))].

non_atom_enum_to_json_bad() ->
    Value = 2,
    [?_assertEqual({error,
                    [{ed_error,
                      [],
                      no_match,
                      #{type => [{literal, 1}, {literal, 3}], value => 2}}]},
                   person:non_atom_enum_to_json(Value))].

missing() ->
    Json = json:decode(<<"{\"a\": \"1\"}"/utf8>>),
    [?_assertEqual({error,
                    [{ed_error,
                      [a],
                      module_types_not_found,
                      #{error => non_existing, module => pelle}}]},
                   person:missing_from_json(Json))].

missing_to_json() ->
    Json = #{a => a},
    [?_assertEqual({error,
                    [{ed_error,
                      [a],
                      module_types_not_found,
                      #{error => non_existing, module => pelle}}]},
                   person:missing_to_json(Json))].

remote() ->
    Json = json:decode(<<"{\"a\": {\"id\": \"id1\", \"balance\": 100}}"/utf8>>),
    [?_assertEqual({ok, #{a => #{id => "id1", balance => 100}}},
                   person:remote_from_json(Json))].

remote_bad() ->
    Json = json:decode(<<"{\"a\": {\"id\": \"id1\", \"balance\": \"no_value\"}}"/utf8>>),
    [?_assertEqual({error,
                    [{ed_error,
                      [a, balance],
                      type_mismatch,
                      #{type => {type, integer}, value => <<"no_value">>}}]},
                   person:remote_from_json(Json))].

remote_to_json() ->
    Data = #{a => #{id => "id1", balance => 100}},
    [?_assertEqual({ok, #{a => #{id => <<"id1">>, balance => 100}}},
                   person:remote_to_json(Data))].

remote_to_json_bad() ->
    Data = #{a => #{id => "id1", balance => "no_value"}},
    [?_assertEqual({error,
                    [{ed_error,
                      [a, balance],
                      type_mismatch,
                      #{type => {type, integer}, value => "no_value"}}]},
                   person:remote_to_json(Data))].

binary_data() ->
    Json = json:decode(<<"\"hello world\""/utf8>>),
    [?_assertEqual({ok, <<"hello world">>}, person:binary_data_from_json(Json))].

binary_data_bad() ->
    Json = json:decode(<<"123"/utf8>>),
    [?_assertEqual({error,
                    [{ed_error, [], type_mismatch, #{type => {type, binary}, value => 123}}]},
                   person:binary_data_from_json(Json))].

binary_data_to_json() ->
    Data = <<"hello world">>,
    {ok, Json} = person:binary_data_to_json(Data),
    [?_assertEqual(<<"hello world">>, Json)].

binary_data_to_json_bad() ->
    Data = "hello world", % String instead of binary
    [?_assertEqual({error,
                    [{ed_error,
                      [],
                      type_mismatch,
                      #{type => {type, binary}, value => "hello world"}}]},
                   person:binary_data_to_json(Data))].

binary_map() ->
    Json =
        json:decode(<<"{\"data\": \"hello world\", \"description\": \"test data\"}"/utf8>>),
    {ok, Result} = person:binary_map_from_json(Json),
    [?_assertEqual(<<"hello world">>, maps:get(data, Result)),
     ?_assertEqual("test data", maps:get(description, Result))].

binary_map_bad() ->
    Json = json:decode(<<"{\"data\": 123, \"description\": \"test data\"}"/utf8>>),
    [?_assertEqual({error,
                    [{ed_error, [data], type_mismatch, #{type => {type, binary}, value => 123}}]},
                   person:binary_map_from_json(Json))].

binary_map_to_json() ->
    Data = #{data => <<"hello world">>, description => "test data"},
    {ok, Json} = person:binary_map_to_json(Data),
    [?_assertEqual(#{data => <<"hello world">>, description => <<"test data">>}, Json)].

binary_map_to_json_bad() ->
    Data = #{data => "hello world", description => "test data"}, % String instead of binary
    [?_assertEqual({error,
                    [{ed_error,
                      [data],
                      type_mismatch,
                      #{type => {type, binary}, value => "hello world"}}]},
                   person:binary_map_to_json(Data))].

string_type() ->
    Json = json:decode(<<"\"hello world\""/utf8>>),
    [?_assertEqual({ok, "hello world"}, person:string_type_from_json(Json))].

% Test passing an integer list when a string is expected
% NOTE: This is a surprising but intended feature of the library.
% FIXME: Document.
string_type_bad() ->
    Json =
        json:decode(<<"[104, 101, 108, 108, 111]"/utf8>>), % JSON array of integers (ASCII for "hello")
    [?_assertEqual({ok, "hello"}, person:string_type_from_json(Json))].

string_type_to_json() ->
    Data = "hello world",
    {ok, Json} = person:string_type_to_json(Data),
    [?_assertEqual(<<"hello world">>, Json)].

% Test passing an integer list when a string is expected
% NOTE: This is a surprising but intended feature of the library.
string_type_to_json_bad() ->
    Data = [104, 101, 108, 108, 111], % "hello" as int list (but still valid as a string)
    [?_assertEqual({ok, <<"hello">>}, person:string_type_to_json(Data))].

% Test for int_list_map
int_list_map() ->
    Json = json:decode(<<"{\"text\": \"example\", \"numbers\": [1, 2, 3]}"/utf8>>),
    {ok, Result} = person:int_list_map_from_json(Json),
    [?_assertEqual("example", maps:get(text, Result)),
     ?_assertEqual([1, 2, 3], maps:get(numbers, Result))].

% Test with a string instead of int list
% This expects to fail since we want to distinguish between a binary "123"
% and an actual list of integers
int_list_map_bad() ->
    Json = json:decode(<<"{\"text\": \"example\", \"numbers\": \"123\"}"/utf8>>),
    % Expect the function to handle this error gracefully
    Result = person:int_list_map_from_json(Json),
    [?_assertMatch({error,
                    [{ed_error,
                      [numbers],
                      type_mismatch,
                      #{type := {type, integer}, value := <<"123">>}}]},
                   Result)].

int_list_map_to_json() ->
    Data = #{text => "example", numbers => [1, 2, 3]},
    {ok, Json} = person:int_list_map_to_json(Data),
    [?_assertEqual(#{text => <<"example">>, numbers => [1, 2, 3]}, Json)].

% Test with a string instead of int list for encoding
% NOTE: This is a surprising but intended feature of the library.
% FIXME: Document.
int_list_map_to_json_bad() ->
    Data =
        #{text => "example",
          numbers => "123"}, % String instead of int list, but valid as list of integers
    {ok, Json} = person:int_list_map_to_json(Data),
    % The ASCII values for "123" are [49, 50, 51]
    [?_assertEqual(#{text => <<"example">>, numbers => [49, 50, 51]}, Json)].
