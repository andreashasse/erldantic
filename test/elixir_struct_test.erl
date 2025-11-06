-module(elixir_struct_test).

-include_lib("eunit/include/eunit.hrl").

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
            struct_name = 'Elixir.TestUserStruct',
            missing_value = nil
        },

    %% Convert to JSON - should exclude __struct__ field and contain expected data
    TypeInfo = #{types => #{}, records => #{}},
    {ok, JsonIoList} = spectra:encode(json, TypeInfo, StructType, StructData),
    JsonBinary = iolist_to_binary(JsonIoList),

    %% Decode the JSON to verify it has the expected structure
    Decoded = json:decode(JsonBinary),
    ?assertEqual(
        #{
            <<"name">> => <<"John">>,
            <<"age">> => 30,
            <<"email">> => <<"john@example.com">>
        },
        Decoded
    ).

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
    %% JSON string without __struct__ field
    JsonBinary = <<"{\"name\":\"John\",\"age\":30,\"email\":\"john@example.com\"}">>,

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

    %% Convert from JSON - should add back __struct__ field with all data
    TypeInfo = #{types => #{}, records => #{}},
    {ok, Result} = spectra:decode(json, TypeInfo, StructType, JsonBinary),
    ?assertEqual(
        #{
            '__struct__' => 'Elixir.TestUserStruct',
            name => <<"John">>,
            age => 30,
            email => <<"john@example.com">>
        },
        Result
    ).

to_json_with_nil_email_test() ->
    %% Skip test if Elixir module is not available
    case code:is_loaded('Elixir.TestUserStruct') of
        false ->
            case code:load_file('Elixir.TestUserStruct') of
                {error, _} ->
                    ok;
                {module, _} ->
                    run_to_json_nil_email()
            end;
        {file, _} ->
            run_to_json_nil_email()
    end.

run_to_json_nil_email() ->
    %% Create an instance with email set to nil
    EmptyStruct = 'Elixir.TestUserStruct':'__struct__'(),
    StructData =
        maps:merge(
            EmptyStruct,
            #{
                name => <<"Jane">>,
                age => 25,
                email => nil
            }
        ),

    %% Create type definition with email as optional (union with nil)
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
                        val_type =
                            #sp_union{
                                types = [
                                    #sp_simple_type{type = binary},
                                    #sp_literal{value = nil, binary_value = <<"nil">>}
                                ]
                            }
                    }
                ],
            struct_name = 'Elixir.TestUserStruct',
            missing_value = nil
        },

    %% Convert to JSON - nil email should be omitted from JSON
    TypeInfo = #{types => #{}, records => #{}},
    {ok, JsonIoList} = spectra:encode(json, TypeInfo, StructType, StructData),
    JsonBinary = iolist_to_binary(JsonIoList),

    %% Decode the JSON to verify it has the expected structure (without email)
    Decoded = json:decode(JsonBinary),
    ?assertEqual(
        #{
            <<"name">> => <<"Jane">>,
            <<"age">> => 25
        },
        Decoded
    ).

from_json_with_missing_email_test() ->
    %% Skip test if Elixir module is not available
    case code:is_loaded('Elixir.TestUserStruct') of
        false ->
            case code:load_file('Elixir.TestUserStruct') of
                {error, _} ->
                    ok;
                {module, _} ->
                    run_from_json_missing_email()
            end;
        {file, _} ->
            run_from_json_missing_email()
    end.

run_from_json_missing_email() ->
    %% JSON string without email field
    JsonBinary = <<"{\"name\":\"Jane\",\"age\":25}">>,

    %% Create type definition with email as optional
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
                        val_type =
                            #sp_union{
                                types = [
                                    #sp_simple_type{type = binary},
                                    #sp_literal{value = nil, binary_value = <<"nil">>}
                                ]
                            }
                    }
                ],
            struct_name = 'Elixir.TestUserStruct',
            missing_value = nil
        },

    %% Convert from JSON - missing email should become nil
    TypeInfo = #{types => #{}, records => #{}},
    {ok, Result} = spectra:decode(json, TypeInfo, StructType, JsonBinary),
    ?assertEqual(
        #{
            '__struct__' => 'Elixir.TestUserStruct',
            name => <<"Jane">>,
            age => 25,
            email => nil
        },
        Result
    ).
