-module(openapi_endpoint_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-compile(nowarn_unused_type).

%% Test types for endpoint specifications
-record(user, {id :: integer(), name :: string(), email :: string()}).
-record(create_user_request, {name :: string(), email :: string()}).
-record(user_list, {users :: [#user{}], total :: integer()}).
-record(error_response, {message :: string(), code :: integer()}).

%% Type aliases to avoid unused warnings
-type user() :: #user{}.
-type create_user_request() :: #create_user_request{}.
-type user_list() :: #user_list{}.
-type error_response() :: #error_response{}.
-type user_id() :: integer().

%% Test basic endpoint creation
basic_endpoint_test() ->
    %% Create a simple GET endpoint
    Endpoint = erldantic_openapi:endpoint(get, "/users"),

    %% Should return a basic endpoint structure
    ?assertEqual(get, maps:get(method, Endpoint)),
    ?assertEqual("/users", maps:get(path, Endpoint)),
    ?assertEqual(#{}, maps:get(responses, Endpoint, #{})),
    ?assertEqual([], maps:get(parameters, Endpoint, [])).

%% Test endpoint with response
endpoint_with_response_test() ->
    %% Create endpoint with a response
    Endpoint1 = erldantic_openapi:endpoint(get, "/users"),
    Endpoint =
        erldantic_openapi:with_response(Endpoint1, 200, "List of users", {?MODULE, user_list}),

    %% Should have the response in the responses map
    Responses = maps:get(responses, Endpoint),
    ?assertMatch(#{200 := _}, Responses),

    Response200 = maps:get(200, Responses),
    ?assertEqual("List of users", maps:get(description, Response200)),
    ?assertEqual({?MODULE, user_list}, maps:get(schema, Response200)).

%% Test endpoint with multiple responses
endpoint_with_multiple_responses_test() ->
    %% Create endpoint with multiple responses
    Endpoint1 = erldantic_openapi:endpoint(post, "/users"),
    Endpoint2 =
        erldantic_openapi:with_response(Endpoint1, 201, "User created", {?MODULE, user}),
    Endpoint =
        erldantic_openapi:with_response(Endpoint2,
                                        400,
                                        "Invalid input",
                                        {?MODULE, error_response}),

    %% Should have both responses
    Responses = maps:get(responses, Endpoint),
    ?assertMatch(#{201 := _, 400 := _}, Responses),

    Response201 = maps:get(201, Responses),
    ?assertEqual("User created", maps:get(description, Response201)),
    ?assertEqual({?MODULE, user}, maps:get(schema, Response201)),

    Response400 = maps:get(400, Responses),
    ?assertEqual("Invalid input", maps:get(description, Response400)),
    ?assertEqual({?MODULE, error_response}, maps:get(schema, Response400)).

%% Test endpoint with request body
endpoint_with_request_body_test() ->
    %% Create endpoint with request body
    Endpoint1 = erldantic_openapi:endpoint(post, "/users"),
    Endpoint = erldantic_openapi:with_request_body(Endpoint1, {?MODULE, create_user_request}),

    %% Should have request body
    ?assertEqual({?MODULE, create_user_request}, maps:get(request_body, Endpoint)).

%% Test endpoint with path parameter
endpoint_with_path_parameter_test() ->
    %% Create endpoint with path parameter
    PathParam =
        #{name => "id",
          in => path,
          required => true,
          schema => {?MODULE, user_id}},
    Endpoint1 = erldantic_openapi:endpoint(get, "/users/{id}"),
    Endpoint = erldantic_openapi:with_parameter(Endpoint1, PathParam),

    %% Should have the parameter
    Parameters = maps:get(parameters, Endpoint),
    ?assertEqual(1, length(Parameters)),

    [Parameter] = Parameters,
    ?assertEqual("id", maps:get(name, Parameter)),
    ?assertEqual(path, maps:get(in, Parameter)),
    ?assertEqual(true, maps:get(required, Parameter)),
    ?assertEqual({?MODULE, user_id}, maps:get(schema, Parameter)).

%% Test endpoint with query parameter
endpoint_with_query_parameter_test() ->
    %% Create endpoint with query parameter
    QueryParam =
        #{name => "limit",
          in => query,
          required => false,
          schema => {erlang, integer}},
    Endpoint1 = erldantic_openapi:endpoint(get, "/users"),
    Endpoint = erldantic_openapi:with_parameter(Endpoint1, QueryParam),

    %% Should have the parameter
    Parameters = maps:get(parameters, Endpoint),
    ?assertEqual(1, length(Parameters)),

    [Parameter] = Parameters,
    ?assertEqual("limit", maps:get(name, Parameter)),
    ?assertEqual(query, maps:get(in, Parameter)),
    ?assertEqual(false, maps:get(required, Parameter, false)),
    ?assertEqual({erlang, integer}, maps:get(schema, Parameter)).

%% Test generating OpenAPI spec from single endpoint
single_endpoint_to_openapi_test() ->
    %% Create a simple endpoint
    Endpoint1 = erldantic_openapi:endpoint(get, "/users"),
    Endpoint =
        erldantic_openapi:with_response(Endpoint1, 200, "List of users", {?MODULE, user_list}),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} = erldantic_openapi:endpoints_to_openapi([Endpoint]),

    %% Should be valid OpenAPI 3.0 structure with complete path and operation
    ?assertMatch(#{openapi := <<"3.0.0">>,
                   info := #{title := _, version := _},
                   paths := #{<<"/users">> := #{get := #{responses := #{<<"200">> := _}}}}},
                 OpenAPISpec).

%% Test generating OpenAPI spec from multiple endpoints
multiple_endpoints_to_openapi_test( ) -> Endpoint1 = erldantic_openapi : endpoint( get , "/users" ) , Endpoint1WithResp = erldantic_openapi : with_response( Endpoint1 , 200 , "List of users" , { ?MODULE , user_list } ) , Endpoint2 = erldantic_openapi : endpoint( post , "/users" ) , Endpoint2WithBody = erldantic_openapi : with_request_body( Endpoint2 , { ?MODULE , create_user_request } ) , Endpoint2WithResp = erldantic_openapi : with_response( Endpoint2WithBody , 201 , "User created" , { ?MODULE , user } ) , PathParam = #{ name => "id" , in => path , required => true , schema => { ?MODULE , user_id } } , Endpoint3 = erldantic_openapi : endpoint( get , "/users/{id}" ) , Endpoint3WithParam = erldantic_openapi : with_parameter( Endpoint3 , PathParam ) , Endpoint3WithResp1 = erldantic_openapi : with_response( Endpoint3WithParam , 200 , "User details" , { ?MODULE , user } ) , Endpoint3WithResp2 = erldantic_openapi : with_response( Endpoint3WithResp1 , 404 , "User not found" , { ?MODULE , error_response } ) , Endpoints = [ Endpoint1WithResp , Endpoint2WithResp , Endpoint3WithResp2 ] , { ok , OpenAPISpec } = erldantic_openapi : endpoints_to_openapi( Endpoints ) , ?assertMatch( #{ paths := #{ << "/users" >> := #{ get := _ , post := _ } , << "/users/{id}" >> := UsersIdPath } } when not is_map_key( post , UsersIdPath ) , OpenAPISpec ) .
    %% Create multiple endpoints
    %% Generate OpenAPI spec

    %% Should have all paths with correct operations

%% Test OpenAPI spec includes component schemas
openapi_with_components_test() ->
    %% Create endpoint that references schemas
    Endpoint1 = erldantic_openapi:endpoint(post, "/users"),
    Endpoint2 =
        erldantic_openapi:with_request_body(Endpoint1, {?MODULE, create_user_request}),
    Endpoint =
        erldantic_openapi:with_response(Endpoint2, 201, "User created", {?MODULE, user}),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} = erldantic_openapi:endpoints_to_openapi([Endpoint]),

    %% Should have components section with schemas
    ?assertMatch(#{components := #{schemas := _}}, OpenAPISpec),

    Components = maps:get(components, OpenAPISpec),
    Schemas = maps:get(schemas, Components),

    %% Should have schemas for the referenced types
    ?assert(maps:is_key(<<"User">>, Schemas) orelse maps:is_key(<<"user">>, Schemas)),
    ?assert(maps:is_key(<<"CreateUserRequest">>, Schemas)
            orelse maps:is_key(<<"create_user_request">>, Schemas)).

%% Test error handling for invalid endpoints
error_handling_test() ->
    %% Test with non-existent schema reference
    Endpoint1 = erldantic_openapi:endpoint(get, "/users"),
    Endpoint =
        erldantic_openapi:with_response(Endpoint1,
                                        200,
                                        "List of users",
                                        {?MODULE, non_existent_type}),

    %% Should handle error gracefully
    Result = erldantic_openapi:endpoints_to_openapi([Endpoint]),
    ?assertMatch({error, _}, Result).
