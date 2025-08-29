-module(record_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-record(person, {name :: string(), age = 1 :: pos_integer()}).

-type person_alias() :: #person{name :: string(), age :: pos_integer()}.

missing_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),
    %% arity
    {ok, PersonRecord} = erldantic_type_info:get_record(TypeInfo, person),
    ?assertEqual(#ed_rec{name = person,
                         fields =
                             [{name, #ed_simple_type{type = string}},
                              {age, #ed_simple_type{type = pos_integer}}],
                         arity = 3},
                 PersonRecord),
    ?assertEqual({ok, #{name => <<"John">>, age => 1}},
                 erldantic_json:record_to_json(?MODULE, person, #person{name = "John"}),
                 "Default value for age picked up when constructing the record, no change needed for to_json"),
    ?assertMatch({error, [#ed_error{location = [age], type = missing_data}]},
                 erldantic_json:record_from_json(?MODULE, person, #{<<"name">> => <<"John">>}),
                 "Default value not picked up here, should it?").
