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
    GetUsersResp = erldantic_openapi:response(200, <<"List of users">>),
    GetUsersRespBody =
        erldantic_openapi:response_with_body(GetUsersResp, ?MODULE, {record, user}),
    GetUsersEndpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    GetUsersEndpoint = erldantic_openapi:add_response(GetUsersEndpoint1, GetUsersRespBody),

    CreateUserResp201 = erldantic_openapi:response(201, <<"User created">>),
    CreateUserResp201Body =
        erldantic_openapi:response_with_body(CreateUserResp201, ?MODULE, {record, user}),
    CreateUserResp400 = erldantic_openapi:response(400, <<"Invalid input">>),
    CreateUserResp400Body =
        erldantic_openapi:response_with_body(CreateUserResp400,
                                             ?MODULE,
                                             {record, error_response}),
    CreateUserEndpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    CreateUserEndpoint2 =
        erldantic_openapi:with_request_body(CreateUserEndpoint1,
                                            ?MODULE,
                                            {record, create_user_request}),
    CreateUserEndpoint3 =
        erldantic_openapi:add_response(CreateUserEndpoint2, CreateUserResp201Body),
    CreateUserEndpoint =
        erldantic_openapi:add_response(CreateUserEndpoint3, CreateUserResp400Body),

    GetUserResp200 = erldantic_openapi:response(200, <<"User details">>),
    GetUserResp200Body =
        erldantic_openapi:response_with_body(GetUserResp200, ?MODULE, {record, user}),
    GetUserResp404 = erldantic_openapi:response(404, <<"User not found">>),
    GetUserResp404Body =
        erldantic_openapi:response_with_body(GetUserResp404, ?MODULE, {record, error_response}),
    GetUserEndpoint1 = erldantic_openapi:endpoint(get, <<"/users/{id}">>),
    GetUserEndpoint2 =
        erldantic_openapi:with_parameter(GetUserEndpoint1,
                                         ?MODULE,
                                         #{name => <<"id">>,
                                           in => path,
                                           required => true,
                                           schema => #ed_simple_type{type = integer}}),
    GetUserEndpoint3 = erldantic_openapi:add_response(GetUserEndpoint2, GetUserResp200Body),
    GetUserEndpoint = erldantic_openapi:add_response(GetUserEndpoint3, GetUserResp404Body),

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
    Response = erldantic_openapi:response(200, <<"Health check">>),
    ResponseWithBody =
        erldantic_openapi:response_with_body(Response, ?MODULE, {record, user}),
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/health">>),
    Endpoint = erldantic_openapi:add_response(Endpoint1, ResponseWithBody),

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
    Response201 = erldantic_openapi:response(201, <<"Success">>),
    Response201WithBody =
        erldantic_openapi:response_with_body(Response201, ?MODULE, {record, user}),
    Response400 = erldantic_openapi:response(400, <<"Error">>),
    Response400WithBody =
        erldantic_openapi:response_with_body(Response400, ?MODULE, {record, error_response}),

    Endpoint1 = erldantic_openapi:endpoint(post, <<"/complex">>),
    Endpoint2 =
        erldantic_openapi:with_request_body(Endpoint1, ?MODULE, {record, create_user_request}),
    Endpoint3 = erldantic_openapi:add_response(Endpoint2, Response201WithBody),
    Endpoint4 = erldantic_openapi:add_response(Endpoint3, Response400WithBody),
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
    GetUsersResp = erldantic_openapi:response(200, <<"List of users">>),
    GetUsersRespBody =
        erldantic_openapi:response_with_body(GetUsersResp, ?MODULE, {record, user}),
    GetUsersEndpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    GetUsersEndpoint = erldantic_openapi:add_response(GetUsersEndpoint1, GetUsersRespBody),

    CreateUserResp201 = erldantic_openapi:response(201, <<"User created">>),
    CreateUserResp201Body =
        erldantic_openapi:response_with_body(CreateUserResp201, ?MODULE, {record, user}),
    CreateUserResp400 = erldantic_openapi:response(400, <<"Invalid input">>),
    CreateUserResp400Body =
        erldantic_openapi:response_with_body(CreateUserResp400,
                                             ?MODULE,
                                             {record, error_response}),
    CreateUserEndpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    CreateUserEndpoint2 =
        erldantic_openapi:with_request_body(CreateUserEndpoint1,
                                            ?MODULE,
                                            {record, create_user_request}),
    CreateUserEndpoint3 =
        erldantic_openapi:add_response(CreateUserEndpoint2, CreateUserResp201Body),
    CreateUserEndpoint =
        erldantic_openapi:add_response(CreateUserEndpoint3, CreateUserResp400Body),

    GetUserResp200 = erldantic_openapi:response(200, <<"User details">>),
    GetUserResp200Body =
        erldantic_openapi:response_with_body(GetUserResp200, ?MODULE, {record, user}),
    GetUserResp404 = erldantic_openapi:response(404, <<"User not found">>),
    GetUserResp404Body =
        erldantic_openapi:response_with_body(GetUserResp404, ?MODULE, {record, error_response}),
    GetUserByIdEndpoint1 = erldantic_openapi:endpoint(get, <<"/users/{id}">>),
    GetUserByIdEndpoint2 =
        erldantic_openapi:with_parameter(GetUserByIdEndpoint1,
                                         ?MODULE,
                                         #{name => <<"id">>,
                                           in => path,
                                           required => true,
                                           schema => #ed_simple_type{type = integer}}),
    GetUserByIdEndpoint3 =
        erldantic_openapi:add_response(GetUserByIdEndpoint2, GetUserResp200Body),
    GetUserByIdEndpoint =
        erldantic_openapi:add_response(GetUserByIdEndpoint3, GetUserResp404Body),

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
    GetUsersResp = erldantic_openapi:response(200, <<"List of users">>),
    GetUsersRespBody =
        erldantic_openapi:response_with_body(GetUsersResp, ?MODULE, {record, user}),
    GetUsersEndpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    GetUsersEndpoint = erldantic_openapi:add_response(GetUsersEndpoint1, GetUsersRespBody),

    CreateUserResp201 = erldantic_openapi:response(201, <<"User created">>),
    CreateUserResp201Body =
        erldantic_openapi:response_with_body(CreateUserResp201, ?MODULE, {record, user}),
    CreateUserResp400 = erldantic_openapi:response(400, <<"Invalid input">>),
    CreateUserResp400Body =
        erldantic_openapi:response_with_body(CreateUserResp400,
                                             ?MODULE,
                                             {record, error_response}),
    CreateUserEndpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    CreateUserEndpoint2 =
        erldantic_openapi:with_request_body(CreateUserEndpoint1,
                                            ?MODULE,
                                            {record, create_user_request}),
    CreateUserEndpoint3 =
        erldantic_openapi:add_response(CreateUserEndpoint2, CreateUserResp201Body),
    CreateUserEndpoint =
        erldantic_openapi:add_response(CreateUserEndpoint3, CreateUserResp400Body),

    GetUserResp200 = erldantic_openapi:response(200, <<"User details">>),
    GetUserResp200Body =
        erldantic_openapi:response_with_body(GetUserResp200, ?MODULE, {record, user}),
    GetUserResp404 = erldantic_openapi:response(404, <<"User not found">>),
    GetUserResp404Body =
        erldantic_openapi:response_with_body(GetUserResp404, ?MODULE, {record, error_response}),
    GetUserByIdEndpoint1 = erldantic_openapi:endpoint(get, <<"/users/{id}">>),
    GetUserByIdEndpoint2 =
        erldantic_openapi:with_parameter(GetUserByIdEndpoint1,
                                         ?MODULE,
                                         #{name => <<"id">>,
                                           in => path,
                                           required => true,
                                           schema => #ed_simple_type{type = integer}}),
    GetUserByIdEndpoint3 =
        erldantic_openapi:add_response(GetUserByIdEndpoint2, GetUserResp200Body),
    GetUserByIdEndpoint =
        erldantic_openapi:add_response(GetUserByIdEndpoint3, GetUserResp404Body),

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
    Response1 = erldantic_openapi:response(200, <<"List of users">>),
    Response2 =
        erldantic_openapi:response_with_body(Response1,
                                             ?MODULE,
                                             {record, user},
                                             <<"application/xml">>),
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint = erldantic_openapi:add_response(Endpoint1, Response2),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Validate that the generated spec uses the custom content type
    #{paths := #{<<"/users">> := #{get := #{responses := #{<<"200">> := ResponseInSpec}}}}} =
        OpenAPISpec,

    %% Check that the response has application/xml content type
    ?assertMatch(#{content := #{<<"application/xml">> := #{schema := _}}}, ResponseInSpec),

    %% Ensure it does NOT have application/json
    #{content := Content} = ResponseInSpec,
    ?assertNot(maps:is_key(<<"application/json">>, Content)).

%% Test that custom content types appear in generated OpenAPI JSON for request bodies
custom_request_body_content_type_json_test() ->
    %% Create endpoint with custom request body content type
    Response = erldantic_openapi:response(201, <<"User created">>),
    ResponseWithBody =
        erldantic_openapi:response_with_body(Response, ?MODULE, {record, user}),
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_request_body(Endpoint1,
                                            ?MODULE,
                                            {record, create_user_request},
                                            <<"application/xml">>),
    Endpoint = erldantic_openapi:add_response(Endpoint2, ResponseWithBody),

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
    %% Create endpoint without specifying content type
    Response1 = erldantic_openapi:response(201, <<"User created">>),
    Response2 = erldantic_openapi:response_with_body(Response1, ?MODULE, {record, user}),
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_request_body(Endpoint1, ?MODULE, {record, create_user_request}),
    Endpoint = erldantic_openapi:add_response(Endpoint2, Response2),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Validate that the generated spec defaults to application/json
    #{paths := #{<<"/users">> := UsersPath}} = OpenAPISpec,
    #{post := #{requestBody := RequestBody, responses := #{<<"201">> := ResponseInSpec}}} =
        UsersPath,

    %% Check that default content type is application/json
    ?assertMatch(#{content := #{<<"application/json">> := #{schema := _}}}, RequestBody),
    ?assertMatch(#{content := #{<<"application/json">> := #{schema := _}}}, ResponseInSpec).

%% Test mixed content types - different content types for request and response
mixed_content_types_json_test() ->
    %% Create endpoint with different content types for request and response
    Response201_1 = erldantic_openapi:response(201, <<"User created">>),
    Response201 =
        erldantic_openapi:response_with_body(Response201_1,
                                             ?MODULE,
                                             {record, user},
                                             <<"text/plain">>),
    Response400_1 = erldantic_openapi:response(400, <<"Invalid input">>),
    Response400 =
        erldantic_openapi:response_with_body(Response400_1,
                                             ?MODULE,
                                             {record, error_response},
                                             <<"application/json">>),
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_request_body(Endpoint1,
                                            ?MODULE,
                                            {record, create_user_request},
                                            <<"application/xml">>),
    Endpoint3 = erldantic_openapi:add_response(Endpoint2, Response201),
    Endpoint = erldantic_openapi:add_response(Endpoint3, Response400),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Validate that each part has its specified content type
    #{paths := #{<<"/users">> := UsersPath}} = OpenAPISpec,
    #{post :=
          #{requestBody := RequestBody,
            responses := #{<<"201">> := Response201InSpec, <<"400">> := Response400InSpec}}} =
        UsersPath,

    %% Check that each part has the correct content type
    ?assertMatch(#{content := #{<<"application/xml">> := #{schema := _}}}, RequestBody),
    ?assertMatch(#{content := #{<<"text/plain">> := #{schema := _}}}, Response201InSpec),
    ?assertMatch(#{content := #{<<"application/json">> := #{schema := _}}},
                 Response400InSpec),

    %% Ensure they don't have other content types
    #{content := ReqContent} = RequestBody,
    #{content := Resp201Content} = Response201InSpec,
    ?assertNot(maps:is_key(<<"application/json">>, ReqContent)),
    ?assertNot(maps:is_key(<<"application/json">>, Resp201Content)),
    ?assertNot(maps:is_key(<<"application/xml">>, Resp201Content)).

%% Test that response headers appear in generated OpenAPI JSON
response_headers_in_json_test() ->
    %% Create response with headers
    Response1 = erldantic_openapi:response(200, <<"List of users">>),
    Response2 = erldantic_openapi:response_with_body(Response1, ?MODULE, {record, user}),
    Response3 =
        erldantic_openapi:response_with_header(Response2,
                                               <<"X-Rate-Limit">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = integer},
                                                 description => <<"Request limit">>,
                                                 required => false}),
    Response =
        erldantic_openapi:response_with_header(Response3,
                                               <<"X-Request-ID">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = string},
                                                 required => true}),
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint = erldantic_openapi:add_response(Endpoint1, Response),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Validate that headers appear in the generated spec
    #{paths := #{<<"/users">> := #{get := #{responses := #{<<"200">> := ResponseInSpec}}}}} =
        OpenAPISpec,

    %% Check that headers are present
    ?assertMatch(#{headers := #{<<"X-Rate-Limit">> := _, <<"X-Request-ID">> := _}},
                 ResponseInSpec),

    %% Validate header structure
    #{headers :=
          #{<<"X-Rate-Limit">> := RateLimitHeader, <<"X-Request-ID">> := RequestIDHeader}} =
        ResponseInSpec,
    ?assertMatch(#{schema := #{type := <<"integer">>},
                   description := <<"Request limit">>,
                   required := false},
                 RateLimitHeader),
    ?assertMatch(#{schema := #{type := <<"string">>}, required := true}, RequestIDHeader).

%% Test response without headers doesn't have headers field
response_without_headers_test() ->
    %% Create response without headers
    Response1 = erldantic_openapi:response(200, <<"List of users">>),
    Response2 = erldantic_openapi:response_with_body(Response1, ?MODULE, {record, user}),
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint = erldantic_openapi:add_response(Endpoint1, Response2),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Validate that headers field is not present
    #{paths := #{<<"/users">> := #{get := #{responses := #{<<"200">> := ResponseInSpec}}}}} =
        OpenAPISpec,

    %% Should not have headers field
    ?assertNot(maps:is_key(headers, ResponseInSpec)).

%% Test headers on different response status codes
headers_on_different_responses_test() ->
    %% Create responses with headers
    Response201_1 = erldantic_openapi:response(201, <<"User created">>),
    Response201_2 =
        erldantic_openapi:response_with_body(Response201_1, ?MODULE, {record, user}),
    Response201 =
        erldantic_openapi:response_with_header(Response201_2,
                                               <<"Location">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = string}}),

    Response429_1 = erldantic_openapi:response(429, <<"Too many requests">>),
    Response429_2 =
        erldantic_openapi:response_with_body(Response429_1, ?MODULE, {record, error_response}),
    Response429 =
        erldantic_openapi:response_with_header(Response429_2,
                                               <<"Retry-After">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = integer}}),

    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_request_body(Endpoint1, ?MODULE, {record, create_user_request}),
    Endpoint3 = erldantic_openapi:add_response(Endpoint2, Response201),
    Endpoint = erldantic_openapi:add_response(Endpoint3, Response429),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Validate that headers appear on correct responses
    #{paths := #{<<"/users">> := #{post := #{responses := Responses}}}} = OpenAPISpec,
    #{<<"201">> := Response201InSpec, <<"429">> := Response429InSpec} = Responses,

    %% Check 201 response has Location header
    ?assertMatch(#{headers := #{<<"Location">> := _}}, Response201InSpec),
    #{headers := #{<<"Location">> := LocationHeader}} = Response201InSpec,
    ?assertMatch(#{schema := #{type := <<"string">>}}, LocationHeader),

    %% Check 429 response has Retry-After header
    ?assertMatch(#{headers := #{<<"Retry-After">> := _}}, Response429InSpec),
    #{headers := #{<<"Retry-After">> := RetryAfterHeader}} = Response429InSpec,
    ?assertMatch(#{schema := #{type := <<"integer">>}}, RetryAfterHeader).

%% Test response builder pattern generates correct OpenAPI JSON
response_builder_json_generation_test() ->
    %% Build response
    Response1 = erldantic_openapi:response(200, <<"Success">>),
    Response2 = erldantic_openapi:response_with_body(Response1, ?MODULE, {record, user}),
    Response =
        erldantic_openapi:response_with_header(Response2,
                                               <<"X-Rate-Limit">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = integer},
                                                 description => <<"Requests remaining">>,
                                                 required => false}),

    %% Create endpoint with builder pattern response
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint = erldantic_openapi:add_response(Endpoint1, Response),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Validate the generated JSON structure
    #{paths := #{<<"/users">> := #{get := #{responses := #{<<"200">> := Response200}}}}} =
        OpenAPISpec,

    %% Verify response body
    ?assertMatch(#{description := <<"Success">>, content := #{<<"application/json">> := _}},
                 Response200),

    %% Verify header
    ?assertMatch(#{headers := #{<<"X-Rate-Limit">> := _}}, Response200),
    #{headers := #{<<"X-Rate-Limit">> := RateLimitHeader}} = Response200,
    ?assertMatch(#{schema := #{type := <<"integer">>},
                   description := <<"Requests remaining">>,
                   required := false},
                 RateLimitHeader).

%% Test response builder with custom content type in JSON
response_builder_custom_content_type_json_test() ->
    %% Build response with custom content type
    Response1 = erldantic_openapi:response(200, <<"XML Response">>),
    Response =
        erldantic_openapi:response_with_body(Response1,
                                             ?MODULE,
                                             {record, user},
                                             <<"application/xml">>),

    %% Create endpoint
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint = erldantic_openapi:add_response(Endpoint1, Response),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API">>, version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Verify custom content type in JSON
    #{paths := #{<<"/users">> := #{get := #{responses := #{<<"200">> := Response200}}}}} =
        OpenAPISpec,
    ?assertMatch(#{content := #{<<"application/xml">> := _}}, Response200),
    #{content := Content} = Response200,
    ?assertNot(maps:is_key(<<"application/json">>, Content)).

%% Test complete endpoint with response builder
response_builder_complete_endpoint_test() ->
    %% Build multiple responses
    Success = erldantic_openapi:response(200, <<"Success">>),
    Success2 = erldantic_openapi:response_with_body(Success, ?MODULE, {record, user}),

    Created = erldantic_openapi:response(201, <<"Created">>),
    Created2 = erldantic_openapi:response_with_body(Created, ?MODULE, {record, user}),
    Created3 =
        erldantic_openapi:response_with_header(Created2,
                                               <<"Location">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = string}}),

    Error = erldantic_openapi:response(400, <<"Bad Request">>),
    Error2 = erldantic_openapi:response_with_body(Error, ?MODULE, {record, error_response}),

    %% Build endpoint
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_request_body(Endpoint1, ?MODULE, {record, create_user_request}),
    Endpoint3 = erldantic_openapi:add_response(Endpoint2, Success2),
    Endpoint4 = erldantic_openapi:add_response(Endpoint3, Created3),
    Endpoint = erldantic_openapi:add_response(Endpoint4, Error2),

    %% Generate and validate
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API">>, version => <<"1.0.0">>},
                                               [Endpoint]),

    #{paths := #{<<"/users">> := #{post := Operation}}} = OpenAPISpec,

    %% Verify request body
    ?assertMatch(#{requestBody := #{content := #{<<"application/json">> := _}}}, Operation),

    %% Verify all responses
    #{responses := Responses} = Operation,
    ?assertMatch(#{<<"200">> := _,
                   <<"201">> := _,
                   <<"400">> := _},
                 Responses),

    %% Verify 201 has Location header
    #{<<"201">> := Response201} = Responses,
    ?assertMatch(#{headers := #{<<"Location">> := _}}, Response201).
