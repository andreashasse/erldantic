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
    Endpoint = erldantic_openapi:endpoint(get, <<"/users">>),

    %% Should return a basic endpoint structure
    ?assertEqual(#{method => get,
                   path => <<"/users">>,
                   responses => #{},
                   parameters => []},
                 Endpoint).

%% Test endpoint with response
endpoint_with_response_test() ->
    %% Create endpoint with a response
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint =
        erldantic_openapi:with_response(Endpoint1,
                                        200,
                                        <<"List of users">>,
                                        ?MODULE,
                                        {record, user_list}),

    %% Should have the response in the responses map
    ?assertMatch(#{responses :=
                       #{200 :=
                             #{description := <<"List of users">>, schema := {record, user_list}}}},
                 Endpoint).

%% Test endpoint with multiple responses
endpoint_with_multiple_responses_test() ->
    %% Create endpoint with multiple responses
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_response(Endpoint1,
                                        201,
                                        <<"User created">>,
                                        ?MODULE,
                                        {record, user}),
    Endpoint =
        erldantic_openapi:with_response(Endpoint2,
                                        400,
                                        <<"Invalid input">>,
                                        ?MODULE,
                                        {record, error_response}),

    %% Should have both responses
    ?assertMatch(#{responses :=
                       #{201 := #{description := <<"User created">>, schema := {record, user}},
                         400 :=
                             #{description := <<"Invalid input">>,
                               schema := {record, error_response}}}},
                 Endpoint).

%% Test endpoint with request body
endpoint_with_request_body_test() ->
    %% Create endpoint with request body
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint =
        erldantic_openapi:with_request_body(Endpoint1, ?MODULE, {record, create_user_request}),

    %% Should have request body
    ?assertMatch(#{request_body :=
                       #{schema := {record, create_user_request}, module := ?MODULE}},
                 Endpoint).

%% Test endpoint with path parameter
endpoint_with_path_parameter_test() ->
    %% Create endpoint with path parameter
    PathParam =
        #{name => <<"id">>,
          in => path,
          required => true,
          schema => {type, user_id, 0}},
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users/{id}">>),
    Endpoint = erldantic_openapi:with_parameter(Endpoint1, ?MODULE, PathParam),

    %% Should have the parameter
    ?assertMatch(#{parameters :=
                       [#{name := <<"id">>,
                          in := path,
                          required := true,
                          schema := {type, user_id, 0}}]},
                 Endpoint).

%% Test endpoint with query parameter
endpoint_with_query_parameter_test() ->
    %% Create endpoint with query parameter
    QueryParam =
        #{name => <<"limit">>,
          in => query,
          required => false,
          schema => #ed_simple_type{type = integer}},
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint = erldantic_openapi:with_parameter(Endpoint1, ?MODULE, QueryParam),

    %% Should have the parameter
    ?assertMatch(#{parameters :=
                       [#{name := <<"limit">>,
                          in := query,
                          required := false,
                          schema := #ed_simple_type{type = integer}}]},
                 Endpoint).

%% Test generating OpenAPI spec from single endpoint
single_endpoint_to_openapi_test() ->
    %% Create a simple endpoint
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint =
        erldantic_openapi:with_response(Endpoint1,
                                        200,
                                        <<"List of users">>,
                                        ?MODULE,
                                        {record, user_list}),

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    %% Should be valid OpenAPI 3.0 structure with complete path and operation
    ?assertMatch(#{openapi := <<"3.0.0">>,
                   info := #{title := _, version := _},
                   paths := #{<<"/users">> := #{get := #{responses := #{<<"200">> := _}}}}},
                 OpenAPISpec).

%% Test generating OpenAPI spec from multiple endpoints
multiple_endpoints_to_openapi_test() ->
    %% Create multiple endpoints
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint1WithResp =
        erldantic_openapi:with_response(Endpoint1,
                                        200,
                                        <<"List of users">>,
                                        ?MODULE,
                                        {record, user_list}),

    Endpoint2 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2WithBody =
        erldantic_openapi:with_request_body(Endpoint2, ?MODULE, {record, create_user_request}),
    Endpoint2WithResp =
        erldantic_openapi:with_response(Endpoint2WithBody,
                                        201,
                                        <<"User created">>,
                                        ?MODULE,
                                        {record, user}),

    PathParam =
        #{name => <<"id">>,
          in => path,
          required => true,
          schema => {type, user_id, 0}},
    Endpoint3 = erldantic_openapi:endpoint(get, <<"/users/{id}">>),
    Endpoint3WithParam = erldantic_openapi:with_parameter(Endpoint3, ?MODULE, PathParam),
    Endpoint3WithResp1 =
        erldantic_openapi:with_response(Endpoint3WithParam,
                                        200,
                                        <<"User details">>,
                                        ?MODULE,
                                        {record, user}),
    Endpoint3WithResp2 =
        erldantic_openapi:with_response(Endpoint3WithResp1,
                                        404,
                                        <<"User not found">>,
                                        ?MODULE,
                                        {record, error_response}),

    Endpoints = [Endpoint1WithResp, Endpoint2WithResp, Endpoint3WithResp2],

    %% Generate OpenAPI spec
    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               Endpoints),

    %% Should have all paths with correct operations
    #{paths := #{<<"/users/{id}">> := UsersIdPath}} = OpenAPISpec,
    ?assertMatch(#{paths :=
                       #{<<"/users">> := #{get := _, post := _}, <<"/users/{id}">> := _}},
                 OpenAPISpec),
    ?assertNot(is_map_key(post, UsersIdPath)).

%% Test OpenAPI spec includes component schemas
openapi_with_components_test() ->
    %% Create endpoint that references schemas
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
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint =
        erldantic_openapi:with_response(Endpoint1,
                                        200,
                                        <<"List of users">>,
                                        ?MODULE,
                                        {record, non_existent_type}),

    %% Should handle error gracefully
    Result =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),
    ?assertMatch({error, _}, Result).

%% Test with direct ed_type() values (inline schemas)
endpoint_with_direct_types_test() ->
    %% Create endpoint with direct ed_type values instead of type references
    StringType = #ed_simple_type{type = string},
    IntegerType = #ed_simple_type{type = integer},

    %% Create endpoint with direct types
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/direct-types">>),
    Endpoint2 = erldantic_openapi:with_request_body(Endpoint1, ?MODULE, StringType),
    Endpoint =
        erldantic_openapi:with_response(Endpoint2, 200, <<"Success">>, ?MODULE, IntegerType),

    %% Should work with direct types
    ?assertMatch(#{request_body := #{schema := StringType, module := ?MODULE}}, Endpoint),
    ?assertMatch(#{responses := #{200 := #{schema := IntegerType, module := ?MODULE}}},
                 Endpoint).

%% Test with mixed type references and direct types
endpoint_with_mixed_types_test() ->
    %% Mix of type references and direct types
    DirectStringType = #ed_simple_type{type = string},
    TypeRef = {type, user, 0},

    QueryParam =
        #{name => <<"filter">>,
          in => query,
          required => false,
          schema => DirectStringType},

    Endpoint1 = erldantic_openapi:endpoint(get, <<"/mixed-types">>),
    Endpoint2 =
        erldantic_openapi:with_response(Endpoint1, 200, <<"User data">>, ?MODULE, TypeRef),
    Endpoint = erldantic_openapi:with_parameter(Endpoint2, ?MODULE, QueryParam),

    %% Should handle both types correctly
    ?assertMatch(#{responses := #{200 := #{schema := TypeRef, module := ?MODULE}}}, Endpoint),
    ?assertMatch(#{parameters := [#{schema := DirectStringType, module := ?MODULE}]},
                 Endpoint).

%% Test with complex direct types
endpoint_with_complex_direct_types_test() ->
    %% Create complex direct types
    ListType = #ed_list{type = #ed_simple_type{type = string}},
    MapType =
        #ed_map{fields =
                    [{map_field_exact, name, #ed_simple_type{type = string}},
                     {map_field_exact, age, #ed_simple_type{type = integer}}]},
    UnionType =
        #ed_union{types = [#ed_simple_type{type = string}, #ed_simple_type{type = integer}]},

    Endpoint1 = erldantic_openapi:endpoint(post, <<"/complex-types">>),
    Endpoint2 = erldantic_openapi:with_request_body(Endpoint1, ?MODULE, MapType),
    Endpoint3 =
        erldantic_openapi:with_response(Endpoint2, 200, <<"String list">>, ?MODULE, ListType),
    Endpoint =
        erldantic_openapi:with_response(Endpoint3, 400, <<"Error">>, ?MODULE, UnionType),

    %% Should handle complex types
    ?assertMatch(#{request_body := #{schema := MapType, module := ?MODULE}}, Endpoint),
    ?assertMatch(#{responses :=
                       #{200 := #{schema := ListType, module := ?MODULE},
                         400 := #{schema := UnionType, module := ?MODULE}}},
                 Endpoint).

%% Test endpoint with custom response content type
endpoint_with_custom_response_content_type_test() ->
    %% Create endpoint with custom content type
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint =
        erldantic_openapi:with_response(Endpoint1,
                                        200,
                                        <<"List of users">>,
                                        ?MODULE,
                                        {record, user_list},
                                        <<"application/xml">>),

    %% Should have the response with custom content type
    ?assertMatch(#{responses :=
                       #{200 :=
                             #{description := <<"List of users">>,
                               schema := {record, user_list},
                               content_type := <<"application/xml">>}}},
                 Endpoint).

%% Test endpoint with custom request body content type
endpoint_with_custom_request_body_content_type_test() ->
    %% Create endpoint with custom request body content type
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint =
        erldantic_openapi:with_request_body(Endpoint1,
                                            ?MODULE,
                                            {record, create_user_request},
                                            <<"application/xml">>),

    %% Should have request body with custom content type
    ?assertMatch(#{request_body :=
                       #{schema := {record, create_user_request},
                         module := ?MODULE,
                         content_type := <<"application/xml">>}},
                 Endpoint).

%% Test endpoint with both custom content types
endpoint_with_both_custom_content_types_test() ->
    %% Create endpoint with both custom content types
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
                                        {record, user},
                                        <<"text/plain">>),

    %% Should have both custom content types
    ?assertMatch(#{request_body := #{content_type := <<"application/xml">>},
                   responses := #{201 := #{content_type := <<"text/plain">>}}},
                 Endpoint).

%% Test backward compatibility - endpoints without content type should default to application/json
endpoint_default_content_type_test() ->
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

    %% Should not have content_type field (defaults to application/json in generation)
    #{request_body := RequestBody, responses := Responses} = Endpoint,
    ?assertNot(maps:is_key(content_type, RequestBody)),
    #{201 := Response} = Responses,
    ?assertNot(maps:is_key(content_type, Response)).

%% Test adding response header
endpoint_with_response_header_test() ->
    %% Create endpoint with response and add a header
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_response(Endpoint1,
                                        200,
                                        <<"List of users">>,
                                        ?MODULE,
                                        {record, user_list}),
    Endpoint =
        erldantic_openapi:with_response_header(Endpoint2,
                                               200,
                                               <<"X-Rate-Limit">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = integer},
                                                 description => <<"Request limit">>,
                                                 required => false}),

    %% Should have the header in the response
    #{responses := #{200 := Response}} = Endpoint,
    ?assertMatch(#{headers := #{<<"X-Rate-Limit">> := _}}, Response),
    #{headers := #{<<"X-Rate-Limit">> := Header}} = Response,
    ?assertMatch(#{schema := #ed_simple_type{type = integer},
                   description := <<"Request limit">>,
                   required := false},
                 Header).

%% Test adding multiple response headers
endpoint_with_multiple_response_headers_test() ->
    %% Create endpoint with response and add multiple headers
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_response(Endpoint1,
                                        200,
                                        <<"List of users">>,
                                        ?MODULE,
                                        {record, user_list}),
    Endpoint3 =
        erldantic_openapi:with_response_header(Endpoint2,
                                               200,
                                               <<"X-Rate-Limit">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = integer}}),
    Endpoint =
        erldantic_openapi:with_response_header(Endpoint3,
                                               200,
                                               <<"X-Request-ID">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = string},
                                                 required => true}),

    %% Should have both headers in the response
    #{responses := #{200 := Response}} = Endpoint,
    ?assertMatch(#{headers := #{<<"X-Rate-Limit">> := _, <<"X-Request-ID">> := _}}, Response).

%% Test adding response headers to different status codes
endpoint_with_headers_on_different_responses_test() ->
    %% Create endpoint with multiple responses and different headers
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_response(Endpoint1,
                                        201,
                                        <<"User created">>,
                                        ?MODULE,
                                        {record, user}),
    Endpoint3 =
        erldantic_openapi:with_response(Endpoint2,
                                        429,
                                        <<"Too many requests">>,
                                        ?MODULE,
                                        {record, error_response}),
    Endpoint4 =
        erldantic_openapi:with_response_header(Endpoint3,
                                               201,
                                               <<"Location">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = string}}),
    Endpoint =
        erldantic_openapi:with_response_header(Endpoint4,
                                               429,
                                               <<"Retry-After">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = integer}}),

    %% Should have different headers on different responses
    #{responses := #{201 := Response201, 429 := Response429}} = Endpoint,
    ?assertMatch(#{headers := #{<<"Location">> := _}}, Response201),
    ?assertMatch(#{headers := #{<<"Retry-After">> := _}}, Response429).
