-module(elixir_struct_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

to_json_excludes_struct_field_test() ->
    %% Create an instance of the Elixir struct using runtime reflection
    EmptyStruct = 'Elixir.TestUserStruct':'__struct__'(),
    StructData =
        maps:merge(EmptyStruct,
                   #{name => <<"John">>,
                     age => 30,
                     email => <<"john@example.com">>}),

    %% Create a type definition based on the struct fields (excluding __struct__)
    StructType =
        #ed_map{fields =
                    [{map_field_exact, name, #ed_simple_type{type = binary}},
                     {map_field_exact, age, #ed_simple_type{type = non_neg_integer}},
                     {map_field_exact, email, #ed_simple_type{type = binary}}],
                struct_name = 'Elixir.TestUserStruct'},

    %% Convert to JSON - should exclude __struct__ field
    {ok, Json} = erldantic_json:to_json(#{}, StructType, StructData),

    %% Verify __struct__ field is not in JSON
    ?assertEqual(false, maps:is_key('__struct__', Json)),
    ?assertEqual(<<"John">>, maps:get(name, Json)),
    ?assertEqual(30, maps:get(age, Json)),
    ?assertEqual(<<"john@example.com">>, maps:get(email, Json)).

from_json_adds_struct_field_test() ->
    %% JSON without __struct__ field (using binary keys as expected from real JSON)
    Json =
        #{<<"name">> => <<"John">>,
          <<"age">> => 30,
          <<"email">> => <<"john@example.com">>},

    %% Create type definition with struct name
    StructType =
        #ed_map{fields =
                    [{map_field_exact, name, #ed_simple_type{type = binary}},
                     {map_field_exact, age, #ed_simple_type{type = non_neg_integer}},
                     {map_field_exact, email, #ed_simple_type{type = binary}}],
                struct_name = 'Elixir.TestUserStruct'},

    %% Convert from JSON - should add back __struct__ field
    {ok, Result} = erldantic_json:from_json(#{}, StructType, Json),

    %% Verify __struct__ field is added back
    ?assertEqual('Elixir.TestUserStruct', maps:get('__struct__', Result)),
    ?assertEqual(<<"John">>, maps:get(name, Result)),
    ?assertEqual(30, maps:get(age, Result)),
    ?assertEqual(<<"john@example.com">>, maps:get(email, Result)).
