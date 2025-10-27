-module(nominal_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic_internal.hrl").

-compile(nowarn_unused_type).

%% Regular type (not nominal)
-type user_id() :: pos_integer().

%% Use -nominal for OTP 28+, -type for older versions
-if(?OTP_RELEASE >= 28).
-nominal person() :: #{id := user_id(), name => binary(), age => pos_integer()}.
-else.
-type person() :: #{id := user_id(), name => binary(), age => pos_integer()}.
-endif.

-record(my_rec, {id :: user_id(), data}).

-if(?OTP_RELEASE >= 28).
-nominal my_rec_t() :: #my_rec{data :: person()}.
-else.
-type my_rec_t() :: #my_rec{data :: person()}.
-endif.

-export_type([user_id/0, person/0, my_rec_t/0]).

simple_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),

    {ok, UserIdType} = erldantic_type_info:get_type(TypeInfo, user_id, 0),
    ?assertEqual(#ed_simple_type{type = pos_integer}, UserIdType),

    {ok, PersonType} = erldantic_type_info:get_type(TypeInfo, person, 0),
    ?assertEqual(
        #ed_map{
            fields =
                [
                    {map_field_exact, id, #ed_user_type_ref{type_name = user_id, variables = []}},
                    {map_field_assoc, name, #ed_simple_type{type = binary}},
                    {map_field_assoc, age, #ed_simple_type{type = pos_integer}}
                ]
        },
        PersonType
    ),
    Person = #{id => 1, name => <<"John">>, age => 42},
    ?assertEqual({ok, Person}, erldantic_json:to_json(?MODULE, {type, person, 0}, Person)),
    ?assertEqual(
        {ok, Person},
        erldantic_json:from_json(
            ?MODULE,
            {type, person, 0},
            #{<<"id">> => 1, <<"name">> => <<"John">>, <<"age">> => 42}
        )
    ),

    {ok, MyRecRecord} = erldantic_type_info:get_record(TypeInfo, my_rec),
    ?assertEqual(
        #ed_rec{
            name = my_rec,
            fields =
                [
                    {id, #ed_user_type_ref{type_name = user_id, variables = []}},
                    {data, #ed_simple_type{type = term}}
                ],
            arity = 3
        },
        MyRecRecord
    ),
    {ok, MyRecTType} = erldantic_type_info:get_type(TypeInfo, my_rec_t, 0),
    ?assertEqual(
        #ed_rec_ref{
            record_name = my_rec,
            field_types =
                [{data, #ed_user_type_ref{type_name = person, variables = []}}]
        },
        MyRecTType
    ),

    ?assertEqual(
        {ok, #{id => 1, data => #{id => 2, name => <<"John">>, age => 42}}},
        erldantic_json:to_json(
            ?MODULE,
            {type, my_rec_t, 0},
            #my_rec{id = 1, data = #{id => 2, name => <<"John">>, age => 42}}
        )
    ),
    ?assertEqual(
        {ok, #my_rec{id = 1, data = #{id => 2, name => <<"John">>, age => 42}}},
        erldantic_json:from_json(
            ?MODULE,
            {type, my_rec_t, 0},
            #{
                <<"id">> => 1,
                <<"data">> =>
                    #{<<"id">> => 2, <<"name">> => <<"John">>, <<"age">> => 42}
            }
        )
    ).
