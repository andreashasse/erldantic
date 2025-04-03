-module(record_type_introspect_test).

-include_lib("eunit/include/eunit.hrl").

type_info() ->
    % FIXME: I copy the output I get when compiling person.erl here
    #{{record, address} =>
          {a_rec, address, [{street, {type, string}}, {city, {type, string}}]},
      {record, person} =>
          {a_rec,
           person,
           [{name, {user_type_ref, name_t}},
            {age, {type, integer}},
            {home, {record_ref, address}}]},
      {type, address_t} => {a_rec, address, [{street, string}, {city, string}]},
      {type, name_t} =>
          {map, [{map_field_assoc, first, string}, {map_field_exact, last, string}]}}.

person_test() ->
    Json =
        json:decode(<<"{\"name\": {\"first\": \"Andreas\", \"last\": \"Hasselberg\"}, "
                      "\"age\": 22, \"home\": {\"street\": \"mojs\", \"city\": \"sollentuna\""
                      "}}"/utf8>>),
    {person, Name, Age, Home} =
        Person = record_type_introspect:record_from_json(type_info(), person, Json),
    #{first := <<"Andreas">>, last := <<"Hasselberg">>} = Name,
    22 = Age,
    {address, <<"mojs">>, <<"sollentuna">>} = Home,
    Person.

address_test() ->
    Json = json:decode(<<"{\"street\": \"mojs\", \"city\": \"sollentuna\"}"/utf8>>),
    {address, <<"mojs">>, <<"sollentuna">>} =
        record_type_introspect:from_json(type_info(), {record, address}, Json).

name_t_test() ->
    Json =
        json:decode(<<"{\"first\": \"Andreas\", \"last\": \"Hasselberg\", \"not_present\": "
                      "22}"/utf8>>),
    #{first := <<"Andreas">>, last := <<"Hasselberg">>} =
        M = record_type_introspect:from_json(type_info(), {type, name_t}, Json),
    [first, last] = maps:keys(M),
    M.

type_in_form_test() ->
    %% -type address_t() :: #address{street :: string(), postcode :: string()}.
    Form =
        {attribute,
         {13, 2},
         type,
         {address_t,
          {type,
           {13, 22},
           record,
           [{atom, {13, 23}, address},
            {type, {13, 31}, field_type, [{atom, {13, 31}, street}, {type, {13, 41}, string, []}]},
            {type,
             {13, 51},
             field_type,
             [{atom, {13, 51}, postcode}, {type, {13, 63}, string, []}]}]},
          []}},
    record_type_introspect:type_in_form(Form).
