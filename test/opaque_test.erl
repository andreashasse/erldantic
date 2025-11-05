-module(opaque_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra_internal.hrl").

-compile(nowarn_unused_type).

-opaque person() :: #{name => binary(), age => pos_integer()}.

-record(my_rec, {id :: integer(), data}).

-opaque my_rec_t() :: #my_rec{data :: person()}.

-export_type([person/0, my_rec_t/0]).

simple_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),

    %% Normal opaque type
    {ok, PersonType} = spectra_type_info:get_type(TypeInfo, person, 0),
    ?assertEqual(
        #sp_map{
            fields =
                [
                    #literal_map_field{
                        kind = assoc,
                        name = name,
                        binary_name = <<"name">>,
                        val_type = #sp_simple_type{type = binary},
                        can_be_missing = false
                    },
                    #literal_map_field{
                        kind = assoc,
                        name = age,
                        binary_name = <<"age">>,
                        val_type = #sp_simple_type{type = pos_integer},
                        can_be_missing = false
                    }
                ]
        },
        PersonType
    ),
    Person = #{name => <<"John">>, age => 42},
    ?assertEqual(
        {ok, #{<<"name">> => <<"John">>, <<"age">> => 42}},
        spectra_json:to_json(?MODULE, {type, person, 0}, Person)
    ),
    ?assertEqual(
        {ok, Person},
        spectra_json:from_json(
            ?MODULE,
            {type, person, 0},
            #{<<"name">> => <<"John">>, <<"age">> => 42}
        )
    ),

    %% Opaque record type
    {ok, MyRecRecord} = spectra_type_info:get_record(TypeInfo, my_rec),
    ?assertEqual(
        #sp_rec{
            name = my_rec,
            fields =
                [
                    #sp_rec_field{
                        name = id,
                        binary_name = <<"id">>,
                        type = #sp_simple_type{type = integer},
                        can_be_missing = false
                    },
                    #sp_rec_field{
                        name = data,
                        binary_name = <<"data">>,
                        type = #sp_simple_type{type = term},
                        can_be_missing = false
                    }
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
        {ok, #{<<"id">> => 1, <<"data">> => #{<<"name">> => <<"John">>, <<"age">> => 42}}},
        spectra_json:to_json(
            ?MODULE,
            {type, my_rec_t, 0},
            #my_rec{id = 1, data = #{name => <<"John">>, age => 42}}
        )
    ),
    ?assertEqual(
        {ok, #my_rec{id = 1, data = #{name => <<"John">>, age => 42}}},
        spectra_json:from_json(
            ?MODULE,
            {type, my_rec_t, 0},
            #{
                <<"id">> => 1,
                <<"data">> =>
                    #{<<"name">> => <<"John">>, <<"age">> => 42}
            }
        )
    ).
