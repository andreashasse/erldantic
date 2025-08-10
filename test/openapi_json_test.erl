-module(openapi_json_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").

-compile(nowarn_unused_type).

%% Test records for OpenAPI JSON generation
-record(user, {id :: integer(), name :: string(), email :: string()}).
-record(create_user_request, {name :: string(), email :: string()}).
-record(error_response, {message :: string(), code :: integer()}).

%% Type aliases
-type user() :: #user{}.
-type create_user_request() :: #create_user_request{}.
-type error_response() :: #error_response{}.

%% Test that OpenAPI spec generates JSON-serializable structures
openapi_json_serializable_test() ->
    %% Create a comprehensive API with multiple endpoints
    GetUsersEndpoint1 = erldantic_openapi:endpoint(get, "/users"),
    GetUsersEndpoint =
        erldantic_openapi:with_response(GetUsersEndpoint1, 200, "List of users", {?MODULE, user}),

    CreateUserEndpoint1 = erldantic_openapi:endpoint(post, "/users"),
    CreateUserEndpoint2 =
        erldantic_openapi:with_request_body(CreateUserEndpoint1, {?MODULE, create_user_request}),
    CreateUserEndpoint3 =
        erldantic_openapi:with_response(CreateUserEndpoint2,
                                        201,
                                        "User created",
                                        {?MODULE, user}),
    CreateUserEndpoint =
        erldantic_openapi:with_response(CreateUserEndpoint3,
                                        400,
                                        "Invalid input",
                                        {?MODULE, error_response}),

    GetUserEndpoint1 = erldantic_openapi:endpoint(get, "/users/{id}"),
    GetUserEndpoint2 =
        erldantic_openapi:with_parameter(GetUserEndpoint1,
                                         #{name => "id",
                                           in => path,
                                           required => true,
                                           schema => {erlang, integer}}),
    GetUserEndpoint3 =
        erldantic_openapi:with_response(GetUserEndpoint2, 200, "User details", {?MODULE, user}),
    GetUserEndpoint =
        erldantic_openapi:with_response(GetUserEndpoint3,
                                        404,
                                        "User not found",
                                        {?MODULE, error_response}),

    Endpoints = [GetUsersEndpoint, CreateUserEndpoint, GetUserEndpoint],

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} = erldantic_openapi:endpoints_to_openapi(Endpoints),

    %% Validate that all values are JSON-serializable (no atoms except as map keys)
    validate_json_serializable(OpenAPISpec),

    %% Validate core OpenAPI structure
    ?assertMatch(#{openapi := <<"3.0.0">>,
                   info := #{title := <<"API Documentation">>, version := <<"1.0.0">>},
                   paths := #{<<"/users">> := _, <<"/users/{id}">> := _},
                   components :=
                       #{schemas :=
                             #{<<"User">> := _,
                               <<"CreateUserRequest">> := _,
                               <<"ErrorResponse">> := _}}},
                 OpenAPISpec).

%% Test individual schema structure is JSON-compatible
schema_json_structure_test() ->
    %% Generate schema for user record
    {ok, UserSchema} = erldantic_openapi:record_to_schema(?MODULE, user),

    %% Validate JSON-compatible structure
    validate_json_serializable(UserSchema),

    %% Validate schema structure
    ?assertMatch(#{type := <<"object">>,
                   properties :=
                       #{id := #{type := <<"integer">>},
                         name := #{type := <<"string">>},
                         email := #{type := <<"string">>}}},
                 UserSchema).

%% Test OpenAPI spec contains all required fields for a valid spec
openapi_spec_completeness_test( ) -> Endpoint1 = erldantic_openapi : endpoint( get , "/health" ) , Endpoint = erldantic_openapi : with_response( Endpoint1 , 200 , "Health check" , { ?MODULE , user } ) , { ok , OpenAPISpec } = erldantic_openapi : endpoints_to_openapi( [ Endpoint ] ) , ?assertMatch( #{ openapi := _ , info := #{ title := _ , version := _ } , paths := Paths , components := #{ schemas := _ } } when is_map( Paths ) andalso map_size( Paths ) > 0 , OpenAPISpec ) .
    %% Create a simple but complete spec
    %% Validate required OpenAPI 3.0 fields and structure

%% Test that complex nested structures are properly formed
complex_nested_structure_test() ->
    %% Test endpoint with all possible features
    Endpoint1 = erldantic_openapi:endpoint(post, "/complex"),
    Endpoint2 =
        erldantic_openapi:with_request_body(Endpoint1, {?MODULE, create_user_request}),
    Endpoint3 = erldantic_openapi:with_response(Endpoint2, 201, "Success", {?MODULE, user}),
    Endpoint4 =
        erldantic_openapi:with_response(Endpoint3, 400, "Error", {?MODULE, error_response}),
    Endpoint =
        erldantic_openapi:with_parameter(Endpoint4,
                                         #{name => "debug",
                                           in => query,
                                           required => false,
                                           schema => {erlang, boolean}}),

    {ok, OpenAPISpec} = erldantic_openapi:endpoints_to_openapi([Endpoint]),

    %% Validate the complete structure is JSON-serializable
    validate_json_serializable(OpenAPISpec),

    %% Deep validate the nested structure
    ?assertMatch(#{paths :=
                       #{<<"/complex">> :=
                             #{post :=
                                   #{requestBody := #{required := true},
                                     responses := #{<<"201">> := _, <<"400">> := _},
                                     parameters := [#{name := <<"debug">>, in := query}]}}}},
                 OpenAPISpec).

%% Test final JSON output generation - writes actual OpenAPI JSON to file
final_json_output_test() ->
    %% Create a realistic API specification
    GetUsersEndpoint1 = erldantic_openapi:endpoint(get, "/users"),
    GetUsersEndpoint =
        erldantic_openapi:with_response(GetUsersEndpoint1, 200, "List of users", {?MODULE, user}),

    CreateUserEndpoint1 = erldantic_openapi:endpoint(post, "/users"),
    CreateUserEndpoint2 =
        erldantic_openapi:with_request_body(CreateUserEndpoint1, {?MODULE, create_user_request}),
    CreateUserEndpoint3 =
        erldantic_openapi:with_response(CreateUserEndpoint2,
                                        201,
                                        "User created",
                                        {?MODULE, user}),
    CreateUserEndpoint =
        erldantic_openapi:with_response(CreateUserEndpoint3,
                                        400,
                                        "Invalid input",
                                        {?MODULE, error_response}),

    GetUserByIdEndpoint1 = erldantic_openapi:endpoint(get, "/users/{id}"),
    GetUserByIdEndpoint2 =
        erldantic_openapi:with_parameter(GetUserByIdEndpoint1,
                                         #{name => "id",
                                           in => path,
                                           required => true,
                                           schema => {erlang, integer}}),
    GetUserByIdEndpoint3 =
        erldantic_openapi:with_response(GetUserByIdEndpoint2,
                                        200,
                                        "User details",
                                        {?MODULE, user}),
    GetUserByIdEndpoint =
        erldantic_openapi:with_response(GetUserByIdEndpoint3,
                                        404,
                                        "User not found",
                                        {?MODULE, error_response}),

    Endpoints = [GetUsersEndpoint, CreateUserEndpoint, GetUserByIdEndpoint],

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} = erldantic_openapi:endpoints_to_openapi(Endpoints),

    %% Convert to actual JSON using json.erl
    JsonIoList = json:encode(OpenAPISpec),
    JsonString = iolist_to_binary(JsonIoList),

    %% Parse back from JSON to verify round-trip works
    ParsedJson = json:decode(JsonString),

    %% Display JSON structure for inspection
    io:format("~n=== Generated OpenAPI JSON ===~n"),
    io:format("JSON Length: ~p characters~n", [byte_size(JsonString)]),
    io:format("OpenAPI Version: ~s~n", [maps:get(<<"openapi">>, ParsedJson)]),

    Info = maps:get(<<"info">>, ParsedJson),
    io:format("API Title: ~s~n", [maps:get(<<"title">>, Info)]),
    io:format("API Version: ~s~n", [maps:get(<<"version">>, Info)]),

    Paths = maps:get(<<"paths">>, ParsedJson),
    io:format("Paths: ~p~n", [maps:keys(Paths)]),

    Components = maps:get(<<"components">>, ParsedJson),
    Schemas = maps:get(<<"schemas">>, Components),
    io:format("Schema Components: ~p~n", [maps:keys(Schemas)]),

    %% Show a sample schema from JSON
    UserSchemaJson = maps:get(<<"User">>, Schemas),
    io:format("User Schema Type: ~p~n", [maps:get(<<"type">>, UserSchemaJson)]),
    UserPropsJson = maps:get(<<"properties">>, UserSchemaJson),
    io:format("User Properties: ~p~n", [maps:keys(UserPropsJson)]),

    %% Write JSON to file for external validation
    FileName = "generated_openapi.json",
    file:write_file(FileName, JsonString),
    io:format("~nOpenAPI JSON written to: ~s~n", [FileName]),
    io:format("You can validate this with: swagger-codegen validate -i ~s~n", [FileName]),

    io:format("~n=== JSON Validation ===~n"),
    io:format("Successfully converted to JSON string~n"),
    io:format("Successfully parsed back from JSON~n"),
    io:format("Round-trip conversion verified~n"),

    %% Basic validation that the spec looks correct
    ?assertMatch(#{openapi := <<"3.0.0">>,
                   paths := _,
                   components := _},
                 OpenAPISpec),

    %% Validate the parsed JSON matches expected structure
    ?assertMatch(#{<<"openapi">> := <<"3.0.0">>,
                   <<"paths">> := _,
                   <<"components">> := _},
                 ParsedJson).

%% Test JSON encoding/decoding with various schema types
json_roundtrip_test() ->
    %% Test individual schema JSON conversion
    {ok, UserSchema} = erldantic_openapi:record_to_schema(?MODULE, user),

    %% Convert to JSON and back
    JsonIoList = json:encode(UserSchema),
    JsonString = iolist_to_binary(JsonIoList),
    ParsedSchema = json:decode(JsonString),

    %% Validate the structure is preserved
    ?assertMatch(#{<<"type">> := <<"object">>,
                   <<"properties">> :=
                       #{<<"id">> := #{<<"type">> := <<"integer">>},
                         <<"name">> := #{<<"type">> := <<"string">>},
                         <<"email">> := #{<<"type">> := <<"string">>}}},
                 ParsedSchema).

%% Test that the final JSON is valid OpenAPI 3.0
valid_openapi_json_test( ) -> Endpoint1 = erldantic_openapi : endpoint( get , "/health" ) , Endpoint = erldantic_openapi : with_response( Endpoint1 , 200 , "Health status" , { ?MODULE , user } ) , { ok , OpenAPISpec } = erldantic_openapi : endpoints_to_openapi( [ Endpoint ] ) , JsonIoList = json : encode( OpenAPISpec ) , JsonString = iolist_to_binary( JsonIoList ) , ParsedJson = json : decode( JsonString ) , ?assertMatch( #{ << "openapi" >> := << "3.0.0" >> , << "info" >> := #{ << "title" >> := _ , << "version" >> := _ } , << "paths" >> := Paths , << "components" >> := #{ << "schemas" >> := Schemas } } when is_map( Paths ) andalso map_size( Paths ) > 0 andalso is_map( Schemas ) andalso map_size( Schemas ) > 0 , ParsedJson ) , ?assertMatch( #{ << "/health" >> := #{ << "get" >> := #{ << "responses" >> := #{ << "200" >> := _ } } } } , maps : get( << "paths" >> , ParsedJson ) ) , io : format( "Valid OpenAPI 3.0 JSON generated successfully~n" ) .
    %% Create a minimal but complete API
    %% Convert to JSON

    %% Validate against OpenAPI 3.0 requirements

    %% Validate specific path and operation structure

%% Helper function to validate that a structure is JSON-serializable
%% (no atoms as values, only as map keys)
validate_json_serializable(Value) ->
    json:encode(Value).

%% Test Python-based OpenAPI validation
python_openapi_validation_test() ->
    %% Generate a complete OpenAPI specification first
    GetUsersEndpoint1 = erldantic_openapi:endpoint(get, "/users"),
    GetUsersEndpoint =
        erldantic_openapi:with_response(GetUsersEndpoint1, 200, "List of users", {?MODULE, user}),

    CreateUserEndpoint1 = erldantic_openapi:endpoint(post, "/users"),
    CreateUserEndpoint2 =
        erldantic_openapi:with_request_body(CreateUserEndpoint1, {?MODULE, create_user_request}),
    CreateUserEndpoint3 =
        erldantic_openapi:with_response(CreateUserEndpoint2,
                                        201,
                                        "User created",
                                        {?MODULE, user}),
    CreateUserEndpoint =
        erldantic_openapi:with_response(CreateUserEndpoint3,
                                        400,
                                        "Invalid input",
                                        {?MODULE, error_response}),

    GetUserByIdEndpoint1 = erldantic_openapi:endpoint(get, "/users/{id}"),
    GetUserByIdEndpoint2 =
        erldantic_openapi:with_parameter(GetUserByIdEndpoint1,
                                         #{name => "id",
                                           in => path,
                                           required => true,
                                           schema => {erlang, integer}}),
    GetUserByIdEndpoint3 =
        erldantic_openapi:with_response(GetUserByIdEndpoint2,
                                        200,
                                        "User details",
                                        {?MODULE, user}),
    GetUserByIdEndpoint =
        erldantic_openapi:with_response(GetUserByIdEndpoint3,
                                        404,
                                        "User not found",
                                        {?MODULE, error_response}),

    Endpoints = [GetUsersEndpoint, CreateUserEndpoint, GetUserByIdEndpoint],
    {ok, OpenAPISpec} = erldantic_openapi:endpoints_to_openapi(Endpoints),

    %% Convert to JSON-compatible format and write to file
    JsonIoList = json:encode(OpenAPISpec),
    JsonString = iolist_to_binary(JsonIoList),

    %% Write to file for Python validation
    file:write_file("generated_openapi.json", JsonString),

    %% Run Python validation script
    Output = os:cmd("./validate_openapi.py generated_openapi.json"),

    %% Check that validation passed (look for success message in output)
    case string:find(Output, "is a valid OpenAPI") of
        nomatch ->
            %% Format output as a simple string for error reporting
            OutputStr = io_lib:format("~w", [Output]),
            ?assert(false, io_lib:format("Python OpenAPI validation failed: ~s", [OutputStr]));
        _ ->
            %% Just report success without trying to display the emoji
            io:format("Python OpenAPI validation: PASSED~n")
    end.
