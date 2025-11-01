-module(record_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_unused_type).

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-record(person, {name :: string(), age = 1 :: pos_integer()}).

-type person_alias() :: #person{name :: string(), age :: pos_integer()}.

missing_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    %% arity
    {ok, PersonRecord} = spectra_type_info:get_record(TypeInfo, person),
    ?assertEqual(
        #sp_rec{
            name = person,
            fields =
                [
                    #sp_rec_field{
                        name = name, binary_name = <<"name">>, type = #sp_simple_type{type = string}
                    },
                    #sp_rec_field{
                        name = age,
                        binary_name = <<"age">>,
                        type = #sp_simple_type{type = pos_integer}
                    }
                ],
            arity = 3
        },
        PersonRecord
    ),
    ?assertEqual(
        {ok, #{<<"name">> => <<"John">>, <<"age">> => 1}},
        spectra_json:to_json(?MODULE, {record, person}, #person{name = "John"}),
        "Default value for age picked up when constructing the record, no change needed for to_json"
    ),
    ?assertMatch(
        {error, [#sp_error{location = [age], type = missing_data}]},
        spectra_json:from_json(?MODULE, {record, person}, #{<<"name">> => <<"John">>}),
        "Default value not picked up here, should it?"
    ).
