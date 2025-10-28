-module(nominal_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra_internal.hrl").

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
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    {ok, UserIdType} = spectra_type_info:get_type(TypeInfo, user_id, 0),
    ?assertEqual(#sp_simple_type{type = pos_integer}, UserIdType),

    {ok, PersonType} = spectra_type_info:get_type(TypeInfo, person, 0),
    ?assertEqual(
        #sp_map{
            fields =
                [
                    {map_field_exact, id, #sp_user_type_ref{type_name = user_id, variables = []}},
                    {map_field_assoc, name, #sp_simple_type{type = binary}},
                    {map_field_assoc, age, #sp_simple_type{type = pos_integer}}
                ]
        },
        PersonType
    ),
    Person = #{id => 1, name => <<"John">>, age => 42},
    ?assertEqual({ok, Person}, spectra_json:to_json(?MODULE, {type, person, 0}, Person)),
    ?assertEqual(
        {ok, Person},
        spectra_json:from_json(
            ?MODULE,
            {type, person, 0},
            #{<<"id">> => 1, <<"name">> => <<"John">>, <<"age">> => 42}
        )
    ),

    {ok, MyRecRecord} = spectra_type_info:get_record(TypeInfo, my_rec),
    ?assertEqual(
        #sp_rec{
            name = my_rec,
            fields =
                [
                    {id, #sp_user_type_ref{type_name = user_id, variables = []}},
                    {data, #sp_simple_type{type = term}}
                ],
            arity = 3
        },
        MyRecRecord
    ),
    {ok, MyRecTType} = spectra_type_info:get_type(TypeInfo, my_rec_t, 0),
    ?assertEqual(
        #sp_rec_ref{
            record_name = my_rec,
            field_types =
                [{data, #sp_user_type_ref{type_name = person, variables = []}}]
        },
        MyRecTType
    ),

    ?assertEqual(
        {ok, #{id => 1, data => #{id => 2, name => <<"John">>, age => 42}}},
        spectra_json:to_json(
            ?MODULE,
            {type, my_rec_t, 0},
            #my_rec{id = 1, data = #{id => 2, name => <<"John">>, age => 42}}
        )
    ),
    ?assertEqual(
        {ok, #my_rec{id = 1, data = #{id => 2, name => <<"John">>, age => 42}}},
        spectra_json:from_json(
            ?MODULE,
            {type, my_rec_t, 0},
            #{
                <<"id">> => 1,
                <<"data">> =>
                    #{<<"id">> => 2, <<"name">> => <<"John">>, <<"age">> => 42}
            }
        )
    ).
