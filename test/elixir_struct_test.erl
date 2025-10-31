-module(elixir_struct_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

to_json_excludes_struct_field_test() ->
    %% Skip test if Elixir module is not available
    case code:is_loaded('Elixir.TestUserStruct') of
        false ->
            case code:load_file('Elixir.TestUserStruct') of
                {error, _} ->
                    % Skip test silently
                    ok;
                {module, _} ->
                    run_to_json()
            end;
        {file, _} ->
            run_to_json()
    end.

run_to_json() ->
    %% Create an instance of the Elixir struct using runtime reflection
    EmptyStruct = 'Elixir.TestUserStruct':'__struct__'(),
    StructData =
        maps:merge(
            EmptyStruct,
            #{
                name => <<"John">>,
                age => 30,
                email => <<"john@example.com">>
            }
        ),

    %% Create a type definition based on the struct fields (excluding __struct__)
    %% Note: We manually define the type instead of using spectra_abstract_code:types_in_module/1
    %% because Elixir beam files use a different backend (elixir_erl) that's incompatible
    %% with Erlang's beam_lib:chunks/2 for abstract code extraction
    StructType =
        #sp_map{
            fields =
                [
                    #literal_map_field{
                        kind = exact,
                        name = name,
                        binary_name = <<"name">>,
                        val_type = #sp_simple_type{type = binary}
                    },
                    #literal_map_field{
                        kind = exact,
                        name = age,
                        binary_name = <<"age">>,
                        val_type = #sp_simple_type{type = non_neg_integer}
                    },
                    #literal_map_field{
                        kind = exact,
                        name = email,
                        binary_name = <<"email">>,
                        val_type = #sp_simple_type{type = binary}
                    }
                ],
            struct_name = 'Elixir.TestUserStruct'
        },

    %% Convert to JSON - should exclude __struct__ field
    {ok, Json} = spectra_json:to_json(#{}, StructType, StructData),

    %% Verify __struct__ field is not in JSON
    ?assertEqual(false, maps:is_key('__struct__', Json)),
    ?assertEqual(<<"John">>, maps:get(name, Json)),
    ?assertEqual(30, maps:get(age, Json)),
    ?assertEqual(<<"john@example.com">>, maps:get(email, Json)).

from_json_adds_struct_field_test() ->
    %% Skip test if Elixir module is not available
    case code:is_loaded('Elixir.TestUserStruct') of
        false ->
            case code:load_file('Elixir.TestUserStruct') of
                {error, _} ->
                    % Skip test silently
                    ok;
                {module, _} ->
                    run_from_json()
            end;
        {file, _} ->
            run_from_json()
    end.

run_from_json() ->
    %% JSON without __struct__ field (using binary keys as expected from real JSON)
    Json =
        #{
            <<"name">> => <<"John">>,
            <<"age">> => 30,
            <<"email">> => <<"john@example.com">>
        },

    %% Create type definition with struct name
    %% Note: Manual type definition required due to Elixir/Erlang beam compatibility issues
    StructType =
        #sp_map{
            fields =
                [
                    #literal_map_field{
                        kind = exact,
                        name = name,
                        binary_name = <<"name">>,
                        val_type = #sp_simple_type{type = binary}
                    },
                    #literal_map_field{
                        kind = exact,
                        name = age,
                        binary_name = <<"age">>,
                        val_type = #sp_simple_type{type = non_neg_integer}
                    },
                    #literal_map_field{
                        kind = exact,
                        name = email,
                        binary_name = <<"email">>,
                        val_type = #sp_simple_type{type = binary}
                    }
                ],
            struct_name = 'Elixir.TestUserStruct'
        },

    %% Convert from JSON - should add back __struct__ field
    {ok, Result} = spectra_json:from_json(#{}, StructType, Json),

    %% Verify __struct__ field is added back
    ?assertEqual('Elixir.TestUserStruct', maps:get('__struct__', Result)),
    ?assertEqual(<<"John">>, maps:get(name, Result)),
    ?assertEqual(30, maps:get(age, Result)),
    ?assertEqual(<<"john@example.com">>, maps:get(email, Result)).
