-module(integer_literal_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-type one() :: 1.
-type courses() :: one() | 2 | 5.
-type bor_t() :: 2 bor 5.
-type game_state() ::
    #{player := string(),
      lives := 1..3,
      level := courses()}.

bor_t_abstract_code_test() ->
    Types = erldantic_abstract_code:types_in_module(?MODULE),
    ?assertEqual(#ed_literal{value = 2 bor 5}, maps:get({type, bor_t, 0}, Types)).

bor_t_to_json_test() ->
    ValidBor = 2 bor 5,
    InvalidBor = 2 bor 4,

    % Test with valid bor_t type
    ?assertEqual({ok, 2 bor 5}, to_json_bor_t(ValidBor)),

    % Test with invalid bor_t type
    {error, Errors} = to_json_bor_t(InvalidBor),
    ?assertMatch([#ed_error{type = type_mismatch,
                            ctx = #{type := #ed_literal{value = 7}, value := 6}}],
                 Errors).

bor_t_from_json_test() ->
    ValidBorJson = 2 bor 5,
    InvalidBorJson = 2 bor 4,

    % Test from_json with valid bor_t
    ?assertEqual({ok, 2 bor 5}, from_json_bor_t(ValidBorJson)),

    % Test from_json with invalid bor_t
    {error, Errors} = from_json_bor_t(InvalidBorJson),
    ?assertMatch([#ed_error{type = type_mismatch,
                            ctx = #{type := #ed_literal{value = 7}, value := 6}}],
                 Errors).

validate_integer_literal_test() ->
    % Test JSON conversion using to_json
    ValidOneData = 1,
    InvalidOneData = 2,

    ValidCourses = 2,
    InvalidCourses = 3,

    ValidGame =
        #{player => "John",
          lives => 2,
          level => 5},
    InvalidLivesGame =
        #{player => "John",
          lives => 4,
          level => 2},
    InvalidLevelGame =
        #{player => "John",
          lives => 3,
          level => 4},

    % Test with valid one() type
    ?assertEqual({ok, 1}, to_json_one(ValidOneData)),

    % Test with invalid one() type
    {error, OneErrors} = to_json_one(InvalidOneData),
    ?assertMatch([#ed_error{type = type_mismatch,
                            ctx = #{type := #ed_literal{value = 1}, value := 2}}],
                 OneErrors),

    % Test with valid courses() type
    ?assertEqual({ok, 2}, to_json_courses(ValidCourses)),

    % Test with invalid courses() type
    {error, CoursesErrors} = to_json_courses(InvalidCourses),
    ?assertMatch([#ed_error{type = no_match,
                            ctx =
                                #{type :=
                                      #ed_union{types =
                                                    [#ed_user_type_ref{type_name = one,
                                                                       variables = []},
                                                     #ed_literal{value = 2},
                                                     #ed_literal{value = 5}]},
                                  value := 3}}],
                 CoursesErrors),

    % Test with valid game_state()
    ?assertEqual({ok,
                  #{player => <<"John">>,
                    lives => 2,
                    level => 5}},
                 to_json_game(ValidGame)),

    % Test with invalid lives in game_state()
    {error, LivesErrors} = to_json_game(InvalidLivesGame),
    ?assertMatch([#ed_error{location = [lives],
                            type = type_mismatch,
                            ctx =
                                #{type :=
                                      #ed_range{type = integer,
                                                lower_bound = 1,
                                                upper_bound = 3},
                                  value := 4}}],
                 LivesErrors),

    % Test with invalid level in game_state()
    {error, LevelErrors} = to_json_game(InvalidLevelGame),
    ?assertMatch([#ed_error{location = [level],
                            type = no_match,
                            ctx =
                                #{type :=
                                      #ed_union{types =
                                                    [#ed_user_type_ref{type_name = one,
                                                                       variables = []},
                                                     #ed_literal{value = 2},
                                                     #ed_literal{value = 5}]},
                                  value := 4}}],
                 LevelErrors),

    % Test JSON conversion using from_json
    ValidOneJson = 1,
    InvalidOneJson = 2,

    ValidGameJson =
        #{<<"player">> => <<"Jane">>,
          <<"lives">> => 3,
          <<"level">> => 1},
    InvalidGameJson =
        #{<<"player">> => <<"Jane">>,
          <<"lives">> => 2,
          <<"level">> => 6},

    % Test from_json with valid one()
    ?assertEqual({ok, 1}, from_json_one(ValidOneJson)),

    % Test from_json with invalid one()
    {error, OneFromErrors} = from_json_one(InvalidOneJson),
    ?assertMatch([#ed_error{type = type_mismatch,
                            ctx = #{type := #ed_literal{value = 1}, value := 2}}],
                 OneFromErrors),

    % Test from_json with valid courses()
    ValidCoursesJson = 5,
    ?assertEqual({ok, 5}, from_json_courses(ValidCoursesJson)),

    % Test from_json with invalid courses()
    InvalidCoursesJson = 3,
    {error, CoursesFromErrors} = from_json_courses(InvalidCoursesJson),
    ?assertMatch([#ed_error{type = no_match,
                            ctx = #{type := #ed_union{types = [_, _, _]}, value := 3}}],
                 CoursesFromErrors),

    % Test from_json with valid game_state()
    {ok, Game} = from_json_game(ValidGameJson),
    ?assertEqual(#{player => "Jane",
                   lives => 3,
                   level => 1},
                 Game),

    % Test from_json with invalid game_state()
    {error, GameFromErrors} = from_json_game(InvalidGameJson),
    ?assertMatch([#ed_error{location = [level],
                            type = no_match,
                            ctx = #{type := #ed_union{types = [_, _, _]}, value := 6}}],
                 GameFromErrors).

-spec to_json_one(one()) -> {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json_one(Data) ->
    erldantic_json:type_to_json(?MODULE, one, Data).

-spec to_json_courses(courses()) ->
                         {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json_courses(Data) ->
    erldantic_json:type_to_json(?MODULE, courses, Data).

-spec to_json_game(game_state()) ->
                      {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json_game(Data) ->
    erldantic_json:type_to_json(?MODULE, game_state, Data).

-spec to_json_bor_t(bor_t()) -> {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json_bor_t(Data) ->
    erldantic_json:type_to_json(?MODULE, bor_t, Data).

-spec from_json_bor_t(json:encode_value()) ->
                         {ok, bor_t()} | {error, [erldantic:error()]}.
from_json_bor_t(Json) ->
    erldantic_json:type_from_json(?MODULE, bor_t, Json).

-spec from_json_one(json:encode_value()) -> {ok, one()} | {error, [erldantic:error()]}.
from_json_one(Json) ->
    erldantic_json:type_from_json(?MODULE, one, Json).

-spec from_json_courses(json:encode_value()) ->
                           {ok, courses()} | {error, [erldantic:error()]}.
from_json_courses(Json) ->
    erldantic_json:type_from_json(?MODULE, courses, Json).

-spec from_json_game(json:encode_value()) ->
                        {ok, game_state()} | {error, [erldantic:error()]}.
from_json_game(Json) ->
    erldantic_json:type_from_json(?MODULE, game_state, Json).
