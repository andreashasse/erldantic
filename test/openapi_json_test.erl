-module(openapi_json_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic_internal.hrl").

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
    GetUsersEndpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    GetUsersEndpoint =
        erldantic_openapi:with_response(GetUsersEndpoint1,
                                        200,
                                        <<"List of users">>,
                                        ?MODULE,
                                        {record, user}),

    CreateUserEndpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    CreateUserEndpoint2 =
        erldantic_openapi:with_request_body(CreateUserEndpoint1,
                                            ?MODULE,
                                            {record, create_user_request}),
    CreateUserEndpoint3 =
        erldantic_openapi:with_response(CreateUserEndpoint2,
                                        201,
                                        <<"User created">>,
                                        ?MODULE,
                                        {record, user}),
    CreateUserEndpoint =
        erldantic_openapi:with_response(CreateUserEndpoint3,
                                        400,
                                        <<"Invalid input">>,
                                        ?MODULE,
                                        {record, error_response}),

    GetUserEndpoint1 = erldantic_openapi:endpoint(get, <<"/users/{id}">>),
    GetUserEndpoint2 =
        erldantic_openapi:with_parameter(GetUserEndpoint1,
                                         ?MODULE,
                                         #{name => <<"id">>,
                                           in => path,
                                           required => true,
                                           schema => #ed_simple_type{type = integer}}),
    GetUserEndpoint3 =
        erldantic_openapi:with_response(GetUserEndpoint2,
                                        200,
                                        <<"User details">>,
                                        ?MODULE,
                                        {record, user}),
    GetUserEndpoint =
        erldantic_openapi:with_response(GetUserEndpoint3,
                                        404,
                                        <<"User not found">>,
                                        ?MODULE,
                                        {record, error_response}),

    Endpoints = [GetUsersEndpoint, CreateUserEndpoint, GetUserEndpoint],

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               Endpoints),

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
                 OpenAPISpec),

    %% Extract paths for detailed validation
    #{paths := #{<<"/users">> := UsersPath, <<"/users/{id}">> := UsersByIdPath}} =
        OpenAPISpec,

    %% Validate /users GET endpoint
    #{get := #{responses := #{<<"200">> := GetUsersResponse}}} = UsersPath,
    ?assertMatch(#{description := <<"List of users">>,
                   content :=
                       #{<<"application/json">> :=
                             #{schema := #{'$ref' := <<"#/components/schemas/User">>}}}},
                 GetUsersResponse),

    %% Validate /users POST endpoint
    #{post := #{requestBody := PostRequestBody, responses := PostResponses}} = UsersPath,
    ?assertMatch(#{required := true,
                   content :=
                       #{<<"application/json">> :=
                             #{schema :=
                                   #{'$ref' := <<"#/components/schemas/CreateUserRequest">>}}}},
                 PostRequestBody),

    #{<<"201">> := Post201Response, <<"400">> := Post400Response} = PostResponses,
    ?assertMatch(#{description := <<"User created">>,
                   content :=
                       #{<<"application/json">> :=
                             #{schema := #{'$ref' := <<"#/components/schemas/User">>}}}},
                 Post201Response),
    ?assertMatch(#{description := <<"Invalid input">>,
                   content :=
                       #{<<"application/json">> :=
                             #{schema := #{'$ref' := <<"#/components/schemas/ErrorResponse">>}}}},
                 Post400Response),

    %% Validate /users/{id} GET endpoint
    #{get := #{parameters := GetByIdParameters, responses := GetByIdResponses}} =
        UsersByIdPath,
    ?assertMatch([#{name := <<"id">>,
                    in := path,
                    required := true,
                    schema := #{type := <<"integer">>}}],
                 GetByIdParameters),

    #{<<"200">> := GetById200Response, <<"404">> := GetById404Response} = GetByIdResponses,
    ?assertMatch(#{description := <<"User details">>,
                   content :=
                       #{<<"application/json">> :=
                             #{schema := #{'$ref' := <<"#/components/schemas/User">>}}}},
                 GetById200Response),
    ?assertMatch(#{description := <<"User not found">>,
                   content :=
                       #{<<"application/json">> :=
                             #{schema := #{'$ref' := <<"#/components/schemas/ErrorResponse">>}}}},
                 GetById404Response).

%% Test individual schema structure is JSON-compatible
schema_json_structure_test() ->
    %% Generate schema for user record
    {ok, UserSchema} = erldantic_json_schema:to_schema(?MODULE, {record, user}),

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
openapi_spec_completeness_test() ->
    %% Create a simple but complete spec
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/health">>),
    Endpoint =
        erldantic_openapi:with_response(Endpoint1,
                                        200,
                                        <<"Health check">>,
                                        ?MODULE,
                                        {record, user}),

    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Validate required OpenAPI 3.0 fields and structure
    #{paths := Paths} = OpenAPISpec,
    ?assertMatch(#{openapi := _,
                   info := #{title := _, version := _},
                   paths := _,
                   components := #{schemas := _}},
                 OpenAPISpec),
    ?assert(is_map(Paths)),
    ?assert(map_size(Paths) > 0).

%% Test that complex nested structures are properly formed
complex_nested_structure_test() ->
    %% Test endpoint with all possible features
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/complex">>),
    Endpoint2 =
        erldantic_openapi:with_request_body(Endpoint1, ?MODULE, {record, create_user_request}),
    Endpoint3 =
        erldantic_openapi:with_response(Endpoint2, 201, <<"Success">>, ?MODULE, {record, user}),
    Endpoint4 =
        erldantic_openapi:with_response(Endpoint3,
                                        400,
                                        <<"Error">>,
                                        ?MODULE,
                                        {record, error_response}),
    Endpoint =
        erldantic_openapi:with_parameter(Endpoint4,
                                         ?MODULE,
                                         #{name => <<"debug">>,
                                           in => query,
                                           required => false,
                                           schema => #ed_simple_type{type = boolean}}),

    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

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
    GetUsersEndpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    GetUsersEndpoint =
        erldantic_openapi:with_response(GetUsersEndpoint1,
                                        200,
                                        <<"List of users">>,
                                        ?MODULE,
                                        {record, user}),

    CreateUserEndpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    CreateUserEndpoint2 =
        erldantic_openapi:with_request_body(CreateUserEndpoint1,
                                            ?MODULE,
                                            {record, create_user_request}),
    CreateUserEndpoint3 =
        erldantic_openapi:with_response(CreateUserEndpoint2,
                                        201,
                                        <<"User created">>,
                                        ?MODULE,
                                        {record, user}),
    CreateUserEndpoint =
        erldantic_openapi:with_response(CreateUserEndpoint3,
                                        400,
                                        <<"Invalid input">>,
                                        ?MODULE,
                                        {record, error_response}),

    GetUserByIdEndpoint1 = erldantic_openapi:endpoint(get, <<"/users/{id}">>),
    GetUserByIdEndpoint2 =
        erldantic_openapi:with_parameter(GetUserByIdEndpoint1,
                                         ?MODULE,
                                         #{name => <<"id">>,
                                           in => path,
                                           required => true,
                                           schema => #ed_simple_type{type = integer}}),
    GetUserByIdEndpoint3 =
        erldantic_openapi:with_response(GetUserByIdEndpoint2,
                                        200,
                                        <<"User details">>,
                                        ?MODULE,
                                        {record, user}),
    GetUserByIdEndpoint =
        erldantic_openapi:with_response(GetUserByIdEndpoint3,
                                        404,
                                        <<"User not found">>,
                                        ?MODULE,
                                        {record, error_response}),

    Endpoints = [GetUsersEndpoint, CreateUserEndpoint, GetUserByIdEndpoint],

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               Endpoints),

    %% Convert to actual JSON using json.erl
    JsonIoList = json:encode(OpenAPISpec),
    JsonString = iolist_to_binary(JsonIoList),

    %% Write JSON to file for external validation
    FileName = "generated_openapi.json",
    file:write_file(FileName, JsonString),

    %% Basic validation that the spec looks correct
    ?assertMatch(#{openapi := <<"3.0.0">>,
                   paths := _,
                   components := _},
                 OpenAPISpec).

%% Test JSON encoding with various schema types
json_encoding_test() ->
    %% Test individual schema JSON encoding
    {ok, UserSchema} = erldantic_json_schema:to_schema(?MODULE, {record, user}),

    %% Validate that the schema can be encoded to JSON (this validates JSON compatibility)
    validate_json_serializable(UserSchema),

    %% Validate the original schema structure
    ?assertMatch(#{type := <<"object">>,
                   properties :=
                       #{id := #{type := <<"integer">>},
                         name := #{type := <<"string">>},
                         email := #{type := <<"string">>}}},
                 UserSchema).

%% Test that the OpenAPI spec is valid and can be encoded to JSON
valid_openapi_test() ->
    %% Create a minimal but complete API
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/health">>),
    Endpoint =
        erldantic_openapi:with_response(Endpoint1,
                                        200,
                                        <<"Health status">>,
                                        ?MODULE,
                                        {record, user}),

    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Validate that the spec can be encoded to JSON (this validates JSON compatibility)
    validate_json_serializable(OpenAPISpec),

    %% Validate against OpenAPI 3.0 requirements using original data
    #{paths := Paths, components := #{schemas := Schemas}} = OpenAPISpec,
    ?assertMatch(#{openapi := <<"3.0.0">>,
                   info := #{title := <<"API Documentation">>, version := <<"1.0.0">>},
                   paths := _,
                   components := #{schemas := _}},
                 OpenAPISpec),
    ?assert(is_map(Paths)),
    ?assert(map_size(Paths) > 0),
    ?assert(is_map(Schemas)),
    ?assert(map_size(Schemas) > 0),

    %% Validate specific path and operation structure
    ?assertMatch(#{<<"/health">> := #{get := #{responses := #{<<"200">> := _}}}}, Paths).

%% Helper function to validate that a structure is JSON-serializable
%% (no atoms as values, only as map keys)
validate_json_serializable(Value) ->
    json:encode(Value).

%% Test Python-based OpenAPI validation
python_openapi_validation_test() ->
    %% Check if uv is available first
    case os:cmd("which uv") of
        "" ->
            %% uv not found, skip test
            ok;
        _ ->
            %% uv is available, run the validation test
            run_python_openapi_validation()
    end.

run_python_openapi_validation() ->
    %% Generate a complete OpenAPI specification first
    GetUsersEndpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    GetUsersEndpoint =
        erldantic_openapi:with_response(GetUsersEndpoint1,
                                        200,
                                        <<"List of users">>,
                                        ?MODULE,
                                        {record, user}),

    CreateUserEndpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    CreateUserEndpoint2 =
        erldantic_openapi:with_request_body(CreateUserEndpoint1,
                                            ?MODULE,
                                            {record, create_user_request}),
    CreateUserEndpoint3 =
        erldantic_openapi:with_response(CreateUserEndpoint2,
                                        201,
                                        <<"User created">>,
                                        ?MODULE,
                                        {record, user}),
    CreateUserEndpoint =
        erldantic_openapi:with_response(CreateUserEndpoint3,
                                        400,
                                        <<"Invalid input">>,
                                        ?MODULE,
                                        {record, error_response}),

    GetUserByIdEndpoint1 = erldantic_openapi:endpoint(get, <<"/users/{id}">>),
    GetUserByIdEndpoint2 =
        erldantic_openapi:with_parameter(GetUserByIdEndpoint1,
                                         ?MODULE,
                                         #{name => <<"id">>,
                                           in => path,
                                           required => true,
                                           schema => #ed_simple_type{type = integer}}),
    GetUserByIdEndpoint3 =
        erldantic_openapi:with_response(GetUserByIdEndpoint2,
                                        200,
                                        <<"User details">>,
                                        ?MODULE,
                                        {record, user}),
    GetUserByIdEndpoint =
        erldantic_openapi:with_response(GetUserByIdEndpoint3,
                                        404,
                                        <<"User not found">>,
                                        ?MODULE,
                                        {record, error_response}),

    Endpoints = [GetUsersEndpoint, CreateUserEndpoint, GetUserByIdEndpoint],
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               Endpoints),

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
            %% Validation passed
            ok
    end.

%% Test that custom content types appear in generated OpenAPI JSON for responses
custom_response_content_type_json_test() ->
    %% Create endpoint with custom response content type
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint =
        erldantic_openapi:with_response(Endpoint1,
                                        200,
                                        <<"List of users">>,
                                        ?MODULE,
                                        {record, user},
                                        <<"application/xml">>),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Validate that the generated spec uses the custom content type
    #{paths := #{<<"/users">> := #{get := #{responses := #{<<"200">> := Response}}}}} =
        OpenAPISpec,

    %% Check that the response has application/xml content type
    ?assertMatch(#{content := #{<<"application/xml">> := #{schema := _}}}, Response),

    %% Ensure it does NOT have application/json
    #{content := Content} = Response,
    ?assertNot(maps:is_key(<<"application/json">>, Content)).

%% Test that custom content types appear in generated OpenAPI JSON for request bodies
custom_request_body_content_type_json_test() ->
    %% Create endpoint with custom request body content type
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_request_body(Endpoint1,
                                            ?MODULE,
                                            {record, create_user_request},
                                            <<"application/xml">>),
    Endpoint =
        erldantic_openapi:with_response(Endpoint2,
                                        201,
                                        <<"User created">>,
                                        ?MODULE,
                                        {record, user}),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Validate that the generated spec uses the custom content type
    #{paths := #{<<"/users">> := #{post := #{requestBody := RequestBody}}}} = OpenAPISpec,

    %% Check that the request body has application/xml content type
    ?assertMatch(#{content := #{<<"application/xml">> := #{schema := _}}}, RequestBody),

    %% Ensure it does NOT have application/json for request body
    #{content := Content} = RequestBody,
    ?assertNot(maps:is_key(<<"application/json">>, Content)).

%% Test that default content type (application/json) is used when not specified
default_content_type_json_test() ->
    %% Create endpoint without specifying content type (using old API)
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_request_body(Endpoint1, ?MODULE, {record, create_user_request}),
    Endpoint =
        erldantic_openapi:with_response(Endpoint2,
                                        201,
                                        <<"User created">>,
                                        ?MODULE,
                                        {record, user}),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Validate that the generated spec defaults to application/json
    #{paths := #{<<"/users">> := UsersPath}} = OpenAPISpec,
    #{post := #{requestBody := RequestBody, responses := #{<<"201">> := Response}}} =
        UsersPath,

    %% Check that default content type is application/json
    ?assertMatch(#{content := #{<<"application/json">> := #{schema := _}}}, RequestBody),
    ?assertMatch(#{content := #{<<"application/json">> := #{schema := _}}}, Response).

%% Test mixed content types - different content types for request and response
mixed_content_types_json_test() ->
    %% Create endpoint with different content types for request and response
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_request_body(Endpoint1,
                                            ?MODULE,
                                            {record, create_user_request},
                                            <<"application/xml">>),
    Endpoint3 =
        erldantic_openapi:with_response(Endpoint2,
                                        201,
                                        <<"User created">>,
                                        ?MODULE,
                                        {record, user},
                                        <<"text/plain">>),
    Endpoint =
        erldantic_openapi:with_response(Endpoint3,
                                        400,
                                        <<"Invalid input">>,
                                        ?MODULE,
                                        {record, error_response},
                                        <<"application/json">>),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Validate that each part has its specified content type
    #{paths := #{<<"/users">> := UsersPath}} = OpenAPISpec,
    #{post :=
          #{requestBody := RequestBody,
            responses := #{<<"201">> := Response201, <<"400">> := Response400}}} =
        UsersPath,

    %% Check that each part has the correct content type
    ?assertMatch(#{content := #{<<"application/xml">> := #{schema := _}}}, RequestBody),
    ?assertMatch(#{content := #{<<"text/plain">> := #{schema := _}}}, Response201),
    ?assertMatch(#{content := #{<<"application/json">> := #{schema := _}}}, Response400),

    %% Ensure they don't have other content types
    #{content := ReqContent} = RequestBody,
    #{content := Resp201Content} = Response201,
    ?assertNot(maps:is_key(<<"application/json">>, ReqContent)),
    ?assertNot(maps:is_key(<<"application/json">>, Resp201Content)),
    ?assertNot(maps:is_key(<<"application/xml">>, Resp201Content)).

%% Test that response headers appear in generated OpenAPI JSON
response_headers_in_json_test() ->
    %% Create endpoint with response headers
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_response(Endpoint1,
                                        200,
                                        <<"List of users">>,
                                        ?MODULE,
                                        {record, user}),
    Endpoint3 =
        erldantic_openapi:with_response_header(Endpoint2,
                                               200,
                                               <<"X-Rate-Limit">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = integer},
                                                 description => <<"Request limit">>,
                                                 required => false}),
    Endpoint =
        erldantic_openapi:with_response_header(Endpoint3,
                                               200,
                                               <<"X-Request-ID">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = string},
                                                 required => true}),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Validate that headers appear in the generated spec
    #{paths := #{<<"/users">> := #{get := #{responses := #{<<"200">> := Response}}}}} =
        OpenAPISpec,

    %% Check that headers are present
    ?assertMatch(#{headers := #{<<"X-Rate-Limit">> := _, <<"X-Request-ID">> := _}}, Response),

    %% Validate header structure
    #{headers :=
          #{<<"X-Rate-Limit">> := RateLimitHeader, <<"X-Request-ID">> := RequestIDHeader}} =
        Response,
    ?assertMatch(#{schema := #{type := <<"integer">>},
                   description := <<"Request limit">>,
                   required := false},
                 RateLimitHeader),
    ?assertMatch(#{schema := #{type := <<"string">>}, required := true}, RequestIDHeader).

%% Test response without headers doesn't have headers field
response_without_headers_test() ->
    %% Create endpoint without headers
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint =
        erldantic_openapi:with_response(Endpoint1,
                                        200,
                                        <<"List of users">>,
                                        ?MODULE,
                                        {record, user}),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Validate that headers field is not present
    #{paths := #{<<"/users">> := #{get := #{responses := #{<<"200">> := Response}}}}} =
        OpenAPISpec,

    %% Should not have headers field
    ?assertNot(maps:is_key(headers, Response)).

%% Test headers on different response status codes
headers_on_different_responses_test() ->
    %% Create endpoint with headers on different responses
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_request_body(Endpoint1, ?MODULE, {record, create_user_request}),
    Endpoint3 =
        erldantic_openapi:with_response(Endpoint2,
                                        201,
                                        <<"User created">>,
                                        ?MODULE,
                                        {record, user}),
    Endpoint4 =
        erldantic_openapi:with_response(Endpoint3,
                                        429,
                                        <<"Too many requests">>,
                                        ?MODULE,
                                        {record, error_response}),
    Endpoint5 =
        erldantic_openapi:with_response_header(Endpoint4,
                                               201,
                                               <<"Location">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = string}}),
    Endpoint =
        erldantic_openapi:with_response_header(Endpoint5,
                                               429,
                                               <<"Retry-After">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = integer}}),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Validate that headers appear on correct responses
    #{paths := #{<<"/users">> := #{post := #{responses := Responses}}}} = OpenAPISpec,
    #{<<"201">> := Response201, <<"429">> := Response429} = Responses,

    %% Check 201 response has Location header
    ?assertMatch(#{headers := #{<<"Location">> := _}}, Response201),
    #{headers := #{<<"Location">> := LocationHeader}} = Response201,
    ?assertMatch(#{schema := #{type := <<"string">>}}, LocationHeader),

    %% Check 429 response has Retry-After header
    ?assertMatch(#{headers := #{<<"Retry-After">> := _}}, Response429),
    #{headers := #{<<"Retry-After">> := RetryAfterHeader}} = Response429,
    ?assertMatch(#{schema := #{type := <<"integer">>}}, RetryAfterHeader).
