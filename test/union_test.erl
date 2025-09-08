-module(union_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").

-record(address, {street :: string(), city :: string() | undefined}).

-type score() :: #{value := 1..10, comment => #{lang := string(), text := string()}}.
-type weird_union() :: #address{} | #{city => string(), score => score()}.

%% Test function to validate weird_union type
validate_weird_union_test() ->
    % Test JSON conversion using to_json
    ValidRecord = #address{street = "Main St", city = "New York"},
    ValidMap =
        #{city => "Boston", score => #{value => 8, comment => #{lang => "en", text => "Great"}}},
    InvalidData = #{invalid => "data"},

    % Test with valid record
    ?assertEqual({ok, #{street => <<"Main St">>, city => <<"New York">>}},
                 to_json_weird_union(ValidRecord)),

    % Test with valid map
    ?assertEqual({ok,
                  #{city => <<"Boston">>,
                    score => #{value => 8, comment => #{lang => <<"en">>, text => <<"Great">>}}}},
                 to_json_weird_union(ValidMap)),

    % Test with invalid data
    {error, Errors} = to_json_weird_union(InvalidData),
    ?assertMatch([#ed_error{type = no_match}], Errors),

    % Test JSON conversion using from_json
    ValidRecordJson = #{<<"street">> => <<"Main St">>, <<"city">> => <<"New York">>},
    ValidMapJson =
        #{<<"city">> => <<"Boston">>,
          <<"score">> =>
              #{<<"value">> => 8,
                <<"comment">> => #{<<"lang">> => <<"en">>, <<"text">> => <<"Great">>}}},
    InvalidJson = #{<<"invalid">> => <<"data">>},

    % Test from_json with valid record
    ?assertEqual({ok, #address{street = "Main St", city = "New York"}},
                 from_json_weird_union(ValidRecordJson)),

    % Test from_json with valid map
    ?assertEqual({ok,
                  #{city => "Boston",
                    score => #{value => 8, comment => #{lang => "en", text => "Great"}}}},
                 from_json_weird_union(ValidMapJson)),

    % Test from_json with invalid data
    {error, FromErrors} = from_json_weird_union(InvalidJson),
    ?assertMatch([#ed_error{type = no_match}], FromErrors).

-spec to_json_weird_union(weird_union()) ->
                             {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json_weird_union(Data) ->
    erldantic_json:to_json(?MODULE, {type, weird_union, 0}, Data).

-spec from_json_weird_union(json:encode_value()) ->
                               {ok, weird_union()} | {error, [erldantic:error()]}.
from_json_weird_union(Json) ->
    erldantic_json:from_json(?MODULE, {type, weird_union, 0}, Json).
