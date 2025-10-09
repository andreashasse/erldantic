-module(record_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

-include("../include/impala.hrl").
-include("../include/impala_internal.hrl").

-record(person, {name :: string(), age = 1 :: pos_integer()}).

-type person_alias() :: #person{name :: string(), age :: pos_integer()}.

missing_test() ->
    TypeInfo = impala_abstract_code:types_in_module(?MODULE),
    %% arity
    {ok, PersonRecord} = impala_type_info:get_record(TypeInfo, person),
    ?assertEqual(#im_rec{name = person,
                         fields =
                             [{name, #im_simple_type{type = string}},
                              {age, #im_simple_type{type = pos_integer}}],
                         arity = 3},
                 PersonRecord),
    ?assertEqual({ok, #{name => <<"John">>, age => 1}},
                 impala_json:to_json(?MODULE, {record, person}, #person{name = "John"}),
                 "Default value for age picked up when constructing the record, no change needed for to_json"),
    ?assertMatch({error, [#im_error{location = [age], type = missing_data}]},
                 impala_json:from_json(?MODULE, {record, person}, #{<<"name">> => <<"John">>}),
                 "Default value not picked up here, should it?").
