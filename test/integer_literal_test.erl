-module(integer_literal_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-type one() :: 1.
-type courses() :: one() | 2 | 5.
-type bor_t() :: 2 bor 5.
-type game_state() ::
    #{
        player := string(),
        lives := 1..3,
        level := courses()
    }.

bor_t_abstract_code_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    {ok, BorTType} = spectra_type_info:get_type(TypeInfo, bor_t, 0),
    ?assertEqual(#sp_literal{value = 2 bor 5, binary_value = <<"7">>}, BorTType).

bor_t_to_json_test() ->
    ValidBor = 2 bor 5,
    InvalidBor = 2 bor 4,

    % Test with valid bor_t type
    ?assertEqual({ok, 2 bor 5}, to_json_bor_t(ValidBor)),

    % Test with invalid bor_t type
    {error, Errors} = to_json_bor_t(InvalidBor),
    ?assertMatch(
        [
            #sp_error{
                type = type_mismatch,
                ctx = #{type := #sp_literal{value = 7}, value := 6}
            }
        ],
        Errors
    ).

bor_t_from_json_test() ->
    ValidBorJson = 2 bor 5,
    InvalidBorJson = 2 bor 4,

    % Test from_json with valid bor_t
    ?assertEqual({ok, 2 bor 5}, from_json_bor_t(ValidBorJson)),

    % Test from_json with invalid bor_t
    {error, Errors} = from_json_bor_t(InvalidBorJson),
    ?assertMatch(
        [
            #sp_error{
                type = type_mismatch,
                ctx = #{type := #sp_literal{value = 7}, value := 6}
            }
        ],
        Errors
    ).

validate_integer_literal_test() ->
    % Test JSON conversion using to_json
    ValidOneData = 1,
    InvalidOneData = 2,

    ValidCourses = 2,
    InvalidCourses = 3,

    ValidGame =
        #{
            player => "John",
            lives => 2,
            level => 5
        },
    InvalidLivesGame =
        #{
            player => "John",
            lives => 4,
            level => 2
        },
    InvalidLevelGame =
        #{
            player => "John",
            lives => 3,
            level => 4
        },

    % Test with valid one() type
    ?assertEqual({ok, 1}, to_json_one(ValidOneData)),

    % Test with invalid one() type
    {error, OneErrors} = to_json_one(InvalidOneData),
    ?assertMatch(
        [
            #sp_error{
                type = type_mismatch,
                ctx = #{type := #sp_literal{value = 1}, value := 2}
            }
        ],
        OneErrors
    ),

    % Test with valid courses() type
    ?assertEqual({ok, 2}, to_json_courses(ValidCourses)),

    % Test with invalid courses() type
    {error, CoursesErrors} = to_json_courses(InvalidCourses),
    ?assertMatch(
        [
            #sp_error{
                type = no_match,
                ctx =
                    #{
                        type :=
                            #sp_union{
                                types =
                                    [
                                        #sp_user_type_ref{
                                            type_name = one,
                                            variables = []
                                        },
                                        #sp_literal{value = 2},
                                        #sp_literal{value = 5}
                                    ]
                            },
                        value := 3
                    }
            }
        ],
        CoursesErrors
    ),

    % Test with valid game_state()
    ?assertEqual(
        {ok, #{
            <<"player">> => <<"John">>,
            <<"lives">> => 2,
            <<"level">> => 5
        }},
        to_json_game(ValidGame)
    ),

    % Test with invalid lives in game_state()
    {error, LivesErrors} = to_json_game(InvalidLivesGame),
    ?assertMatch(
        [
            #sp_error{
                location = [lives],
                type = type_mismatch,
                ctx =
                    #{
                        type :=
                            #sp_range{
                                type = integer,
                                lower_bound = 1,
                                upper_bound = 3
                            },
                        value := 4
                    }
            }
        ],
        LivesErrors
    ),

    % Test with invalid level in game_state()
    {error, LevelErrors} = to_json_game(InvalidLevelGame),
    ?assertMatch(
        [
            #sp_error{
                location = [level],
                type = no_match,
                ctx =
                    #{
                        type :=
                            #sp_union{
                                types =
                                    [
                                        #sp_user_type_ref{
                                            type_name = one,
                                            variables = []
                                        },
                                        #sp_literal{value = 2},
                                        #sp_literal{value = 5}
                                    ]
                            },
                        value := 4
                    }
            }
        ],
        LevelErrors
    ),

    % Test JSON conversion using from_json
    ValidOneJson = 1,
    InvalidOneJson = 2,

    ValidGameJson =
        #{
            <<"player">> => <<"Jane">>,
            <<"lives">> => 3,
            <<"level">> => 1
        },
    InvalidGameJson =
        #{
            <<"player">> => <<"Jane">>,
            <<"lives">> => 2,
            <<"level">> => 6
        },

    % Test from_json with valid one()
    ?assertEqual({ok, 1}, from_json_one(ValidOneJson)),

    % Test from_json with invalid one()
    {error, OneFromErrors} = from_json_one(InvalidOneJson),
    ?assertMatch(
        [
            #sp_error{
                type = type_mismatch,
                ctx = #{type := #sp_literal{value = 1}, value := 2}
            }
        ],
        OneFromErrors
    ),

    % Test from_json with valid courses()
    ValidCoursesJson = 5,
    ?assertEqual({ok, 5}, from_json_courses(ValidCoursesJson)),

    % Test from_json with invalid courses()
    InvalidCoursesJson = 3,
    {error, CoursesFromErrors} = from_json_courses(InvalidCoursesJson),
    ?assertMatch(
        [
            #sp_error{
                type = no_match,
                ctx = #{type := #sp_union{types = [_, _, _]}, value := 3}
            }
        ],
        CoursesFromErrors
    ),

    % Test from_json with valid game_state()
    {ok, Game} = from_json_game(ValidGameJson),
    ?assertEqual(
        #{
            player => "Jane",
            lives => 3,
            level => 1
        },
        Game
    ),

    % Test from_json with invalid game_state()
    {error, GameFromErrors} = from_json_game(InvalidGameJson),
    ?assertMatch(
        [
            #sp_error{
                location = [level],
                type = no_match,
                ctx = #{type := #sp_union{types = [_, _, _]}, value := 6}
            }
        ],
        GameFromErrors
    ).

-spec to_json_one(one()) -> {ok, json:encode_value()} | {error, [spectra:error()]}.
to_json_one(Data) ->
    spectra_json:to_json(?MODULE, {type, one, 0}, Data).

-spec to_json_courses(courses()) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
to_json_courses(Data) ->
    spectra_json:to_json(?MODULE, {type, courses, 0}, Data).

-spec to_json_game(game_state()) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
to_json_game(Data) ->
    spectra_json:to_json(?MODULE, {type, game_state, 0}, Data).

-spec to_json_bor_t(bor_t()) -> {ok, json:encode_value()} | {error, [spectra:error()]}.
to_json_bor_t(Data) ->
    spectra_json:to_json(?MODULE, {type, bor_t, 0}, Data).

-spec from_json_bor_t(json:encode_value()) ->
    {ok, bor_t()} | {error, [spectra:error()]}.
from_json_bor_t(Json) ->
    spectra_json:from_json(?MODULE, {type, bor_t, 0}, Json).

-spec from_json_one(json:encode_value()) -> {ok, one()} | {error, [spectra:error()]}.
from_json_one(Json) ->
    spectra_json:from_json(?MODULE, {type, one, 0}, Json).

-spec from_json_courses(json:encode_value()) ->
    {ok, courses()} | {error, [spectra:error()]}.
from_json_courses(Json) ->
    spectra_json:from_json(?MODULE, {type, courses, 0}, Json).

-spec from_json_game(json:encode_value()) ->
    {ok, game_state()} | {error, [spectra:error()]}.
from_json_game(Json) ->
    spectra_json:from_json(?MODULE, {type, game_state, 0}, Json).
