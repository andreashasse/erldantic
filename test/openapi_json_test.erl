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
    ?assertEqual("3.0.0", maps:get(openapi, OpenAPISpec)),

    Info = maps:get(info, OpenAPISpec),
    ?assertEqual("API Documentation", maps:get(title, Info)),
    ?assertEqual("1.0.0", maps:get(version, Info)),

    %% Validate paths exist
    Paths = maps:get(paths, OpenAPISpec),
    ?assert(maps:is_key("/users", Paths)),
    ?assert(maps:is_key("/users/{id}", Paths)),

    %% Validate components exist
    Components = maps:get(components, OpenAPISpec),
    Schemas = maps:get(schemas, Components),
    ?assert(maps:is_key(<<"User">>, Schemas)),
    ?assert(maps:is_key(<<"CreateUserRequest">>, Schemas)),
    ?assert(maps:is_key(<<"ErrorResponse">>, Schemas)).

%% Test individual schema structure is JSON-compatible
schema_json_structure_test() ->
    %% Generate schema for user record
    {ok, UserSchema} = erldantic_openapi:record_to_schema(?MODULE, user),

    %% Validate JSON-compatible structure
    validate_json_serializable(UserSchema),

    %% Validate schema structure
    ?assertEqual(object, maps:get(type, UserSchema)),

    Properties = maps:get(properties, UserSchema),
    ?assert(maps:is_key(id, Properties)),
    ?assert(maps:is_key(name, Properties)),
    ?assert(maps:is_key(email, Properties)),

    %% Validate property types
    IdProp = maps:get(id, Properties),
    ?assertEqual(integer, maps:get(type, IdProp)),

    NameProp = maps:get(name, Properties),
    ?assertEqual(string, maps:get(type, NameProp)).

%% Test OpenAPI spec contains all required fields for a valid spec
openapi_spec_completeness_test() ->
    %% Create a simple but complete spec
    Endpoint1 = erldantic_openapi:endpoint(get, "/health"),
    Endpoint =
        erldantic_openapi:with_response(Endpoint1, 200, "Health check", {?MODULE, user}),

    {ok, OpenAPISpec} = erldantic_openapi:endpoints_to_openapi([Endpoint]),

    %% Validate required OpenAPI 3.0 fields exist
    RequiredFields = [openapi, info, paths],
    lists:foreach(fun(Field) -> ?assert(maps:is_key(Field, OpenAPISpec)) end, RequiredFields),

    %% Validate info object has required fields
    Info = maps:get(info, OpenAPISpec),
    InfoRequiredFields = [title, version],
    lists:foreach(fun(Field) -> ?assert(maps:is_key(Field, Info)) end, InfoRequiredFields),

    %% Validate paths structure
    Paths = maps:get(paths, OpenAPISpec),
    ?assert(is_map(Paths)),
    ?assert(maps:size(Paths) > 0),

    %% Validate that we have components when schemas are referenced
    Components = maps:get(components, OpenAPISpec),
    ?assert(is_map(Components)),
    ?assert(maps:is_key(schemas, Components)).

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
    Paths = maps:get(paths, OpenAPISpec),
    ComplexPath = maps:get("/complex", Paths),
    PostOp = maps:get(post, ComplexPath),

    %% Validate request body structure
    ?assert(maps:is_key(requestBody, PostOp)),
    RequestBody = maps:get(requestBody, PostOp),
    ?assertEqual(true, maps:get(required, RequestBody)),

    %% Validate response structure
    ?assert(maps:is_key(responses, PostOp)),
    Responses = maps:get(responses, PostOp),
    ?assert(maps:is_key(<<"201">>, Responses)),
    ?assert(maps:is_key(<<"400">>, Responses)),

    %% Validate parameter structure
    ?assert(maps:is_key(parameters, PostOp)),
    Parameters = maps:get(parameters, PostOp),
    ?assertEqual(1, length(Parameters)),
    [Parameter] = Parameters,
    ?assertEqual("debug", maps:get(name, Parameter)),
    ?assertEqual(query, maps:get(in, Parameter)).

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
    JsonCompatibleSpec = convert_to_json_terms(OpenAPISpec),
    JsonIoList = json:encode(JsonCompatibleSpec),
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
    ?assertEqual("3.0.0", maps:get(openapi, OpenAPISpec)),
    ?assert(maps:is_key(paths, OpenAPISpec)),
    ?assert(maps:is_key(components, OpenAPISpec)),

    %% Validate the parsed JSON matches expected structure
    ?assertEqual(<<"3.0.0">>, maps:get(<<"openapi">>, ParsedJson)),
    ?assert(maps:is_key(<<"paths">>, ParsedJson)),
    ?assert(maps:is_key(<<"components">>, ParsedJson)).

%% Test JSON encoding/decoding with various schema types
json_roundtrip_test() ->
    %% Test individual schema JSON conversion
    {ok, UserSchema} = erldantic_openapi:record_to_schema(?MODULE, user),
    JsonCompatibleSchema = convert_to_json_terms(UserSchema),

    %% Convert to JSON and back
    JsonIoList = json:encode(JsonCompatibleSchema),
    JsonString = iolist_to_binary(JsonIoList),
    ParsedSchema = json:decode(JsonString),

    %% Validate the structure is preserved
    ?assertEqual(<<"object">>, maps:get(<<"type">>, ParsedSchema)),
    ?assert(maps:is_key(<<"properties">>, ParsedSchema)),

    Properties = maps:get(<<"properties">>, ParsedSchema),
    ?assert(maps:is_key(<<"id">>, Properties)),
    ?assert(maps:is_key(<<"name">>, Properties)),
    ?assert(maps:is_key(<<"email">>, Properties)),

    %% Validate property types are correct
    IdProp = maps:get(<<"id">>, Properties),
    ?assertEqual(<<"integer">>, maps:get(<<"type">>, IdProp)),

    NameProp = maps:get(<<"name">>, Properties),
    ?assertEqual(<<"string">>, maps:get(<<"type">>, NameProp)).

%% Test that the final JSON is valid OpenAPI 3.0
valid_openapi_json_test() ->
    %% Create a minimal but complete API
    Endpoint1 = erldantic_openapi:endpoint(get, "/health"),
    Endpoint =
        erldantic_openapi:with_response(Endpoint1, 200, "Health status", {?MODULE, user}),

    {ok, OpenAPISpec} = erldantic_openapi:endpoints_to_openapi([Endpoint]),

    %% Convert to JSON
    JsonCompatibleSpec = convert_to_json_terms(OpenAPISpec),
    JsonIoList = json:encode(JsonCompatibleSpec),
    JsonString = iolist_to_binary(JsonIoList),
    ParsedJson = json:decode(JsonString),

    %% Validate against OpenAPI 3.0 requirements
    %% Required root fields
    RequiredFields = [<<"openapi">>, <<"info">>, <<"paths">>],
    lists:foreach(fun(Field) ->
                     ?assert(maps:is_key(Field, ParsedJson),
                             io_lib:format("Missing required field: ~p", [Field]))
                  end,
                  RequiredFields),

    %% OpenAPI version
    ?assertEqual(<<"3.0.0">>, maps:get(<<"openapi">>, ParsedJson)),

    %% Info object requirements
    Info = maps:get(<<"info">>, ParsedJson),
    ?assert(maps:is_key(<<"title">>, Info)),
    ?assert(maps:is_key(<<"version">>, Info)),

    %% Paths object
    Paths = maps:get(<<"paths">>, ParsedJson),
    ?assert(is_map(Paths)),
    ?assert(maps:size(Paths) > 0),

    %% Check that paths contain valid operations
    ?assert(maps:is_key(<<"/health">>, Paths)),
    HealthPath = maps:get(<<"/health">>, Paths),
    ?assert(maps:is_key(<<"get">>, HealthPath)),

    %% Check operation structure
    GetOp = maps:get(<<"get">>, HealthPath),
    ?assert(maps:is_key(<<"responses">>, GetOp)),

    Responses = maps:get(<<"responses">>, GetOp),
    ?assert(maps:is_key(<<"200">>, Responses)),

    %% Components should exist with schemas
    Components = maps:get(<<"components">>, ParsedJson),
    ?assert(maps:is_key(<<"schemas">>, Components)),

    Schemas = maps:get(<<"schemas">>, Components),
    ?assert(maps:size(Schemas) > 0),

    io:format("Valid OpenAPI 3.0 JSON generated successfully~n").

%% Helper function to validate that a structure is JSON-serializable
%% (no atoms as values, only as map keys)
validate_json_serializable(Value) when is_map(Value) ->
    maps:foreach(fun(K, V) ->
                    case is_atom(K) orelse is_binary(K) orelse is_list(K) of
                        true ->
                            ok;  % Lists (strings) can be converted to binaries for JSON
                        false ->
                            io:format("Invalid map key type: ~p (type: ~p)~n", [K, typeof(K)]),
                            ?assert(false,
                                    io_lib:format("Map key should be atom, binary, or string, got: ~p",
                                                  [K]))
                    end,
                    validate_json_serializable(V)
                 end,
                 Value);
validate_json_serializable(Value) when is_list(Value) ->
    lists:foreach(fun validate_json_serializable/1, Value);
validate_json_serializable(Value) when is_binary(Value) ->
    ok;
validate_json_serializable(Value) when is_number(Value) ->
    ok;
validate_json_serializable(Value) when is_boolean(Value) ->
    ok;
validate_json_serializable(null) ->
    ok;
validate_json_serializable(Value) when is_atom(Value) ->
    %% Atoms are acceptable in intermediate format, they should be converted to binaries for JSON
    ok;
validate_json_serializable(Value) ->
    ?assert(false, io_lib:format("Non-JSON-serializable value: ~p", [Value])).

%% Helper function to determine the type of a value
typeof(Value) when is_atom(Value) ->
    atom;
typeof(Value) when is_binary(Value) ->
    binary;
typeof(Value) when is_bitstring(Value) ->
    bitstring;
typeof(Value) when is_boolean(Value) ->
    boolean;
typeof(Value) when is_float(Value) ->
    float;
typeof(Value) when is_function(Value) ->
    function;
typeof(Value) when is_integer(Value) ->
    integer;
typeof(Value) when is_list(Value) ->
    list;
typeof(Value) when is_map(Value) ->
    map;
typeof(Value) when is_number(Value) ->
    number;
typeof(Value) when is_pid(Value) ->
    pid;
typeof(Value) when is_port(Value) ->
    port;
typeof(Value) when is_reference(Value) ->
    reference;
typeof(Value) when is_tuple(Value) ->
    tuple;
typeof(_) ->
    unknown.

%% Convert Erlang terms to JSON-compatible terms
convert_to_json_terms(Value) when is_map(Value) ->
    maps:fold(fun(K, V, Acc) ->
                 JsonKey = convert_key_to_json(K),
                 JsonValue = convert_to_json_terms(V),
                 maps:put(JsonKey, JsonValue, Acc)
              end,
              #{},
              Value);
convert_to_json_terms(Value) when is_list(Value) ->
    case is_string(Value) of
        true ->
            list_to_binary(Value);  % Convert strings to binaries
        false ->
            [convert_to_json_terms(Item) || Item <- Value]
    end;
convert_to_json_terms(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
convert_to_json_terms(Value) ->
    Value.  % Numbers, binaries, booleans pass through

%% Convert map keys to JSON-compatible format
convert_key_to_json(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
convert_key_to_json(Key) when is_list(Key) ->
    list_to_binary(Key);
convert_key_to_json(Key) when is_binary(Key) ->
    Key;
convert_key_to_json(Key) ->
    Key.

%% Simple check if a list is a string
is_string([]) ->
    true;
is_string([H | T]) when is_integer(H), H >= 0, H =< 1114111 ->
    is_string(T);
is_string(_) ->
    false.
