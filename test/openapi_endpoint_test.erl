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
    Endpoint = erldantic_openapi:endpoint(get, <<"/users">>),

    ?assertEqual(#{method => get,
                   path => <<"/users">>,
                   responses => #{},
                   parameters => []},
                 Endpoint).

%% Test endpoint with response
endpoint_with_response_test() ->
    Response = erldantic_openapi:response(200, <<"List of users">>),
    ResponseWithBody =
        erldantic_openapi:response_with_body(Response, ?MODULE, {record, user_list}),
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint = erldantic_openapi:add_response(Endpoint1, ResponseWithBody),

    ?assertMatch(#{responses :=
                       #{200 :=
                             #{description := <<"List of users">>, schema := {record, user_list}}}},
                 Endpoint).

%% Test endpoint with multiple responses
endpoint_with_multiple_responses_test() ->
    Response201 = erldantic_openapi:response(201, <<"User created">>),
    Response201WithBody =
        erldantic_openapi:response_with_body(Response201, ?MODULE, {record, user}),

    Response400 = erldantic_openapi:response(400, <<"Invalid input">>),
    Response400WithBody =
        erldantic_openapi:response_with_body(Response400, ?MODULE, {record, error_response}),

    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2 = erldantic_openapi:add_response(Endpoint1, Response201WithBody),
    Endpoint = erldantic_openapi:add_response(Endpoint2, Response400WithBody),

    ?assertMatch(#{responses :=
                       #{201 := #{description := <<"User created">>, schema := {record, user}},
                         400 :=
                             #{description := <<"Invalid input">>,
                               schema := {record, error_response}}}},
                 Endpoint).

%% Test endpoint with request body
endpoint_with_request_body_test() ->
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint =
        erldantic_openapi:with_request_body(Endpoint1, ?MODULE, {record, create_user_request}),

    ?assertMatch(#{request_body :=
                       #{schema := {record, create_user_request}, module := ?MODULE}},
                 Endpoint).

%% Test endpoint with path parameter
endpoint_with_path_parameter_test() ->
    PathParam =
        #{name => <<"id">>,
          in => path,
          required => true,
          schema => {type, user_id, 0}},
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users/{id}">>),
    Endpoint = erldantic_openapi:with_parameter(Endpoint1, ?MODULE, PathParam),

    ?assertMatch(#{parameters :=
                       [#{name := <<"id">>,
                          in := path,
                          required := true,
                          schema := {type, user_id, 0}}]},
                 Endpoint).

%% Test endpoint with query parameter
endpoint_with_query_parameter_test() ->
    QueryParam =
        #{name => <<"limit">>,
          in => query,
          required => false,
          schema => #ed_simple_type{type = integer}},
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint = erldantic_openapi:with_parameter(Endpoint1, ?MODULE, QueryParam),

    ?assertMatch(#{parameters :=
                       [#{name := <<"limit">>,
                          in := query,
                          required := false,
                          schema := #ed_simple_type{type = integer}}]},
                 Endpoint).

%% Test generating OpenAPI spec from single endpoint
single_endpoint_to_openapi_test() ->
    Response = erldantic_openapi:response(200, <<"List of users">>),
    ResponseWithBody =
        erldantic_openapi:response_with_body(Response, ?MODULE, {record, user_list}),
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint = erldantic_openapi:add_response(Endpoint1, ResponseWithBody),

    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    ?assertMatch(#{openapi := <<"3.0.0">>,
                   info := #{title := _, version := _},
                   paths := #{<<"/users">> := #{get := #{responses := #{<<"200">> := _}}}}},
                 OpenAPISpec).

%% Test generating OpenAPI spec from multiple endpoints
multiple_endpoints_to_openapi_test() ->
    Response1 = erldantic_openapi:response(200, <<"List of users">>),
    Response1WithBody =
        erldantic_openapi:response_with_body(Response1, ?MODULE, {record, user_list}),
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint1WithResp = erldantic_openapi:add_response(Endpoint1, Response1WithBody),

    Response2 = erldantic_openapi:response(201, <<"User created">>),
    Response2WithBody =
        erldantic_openapi:response_with_body(Response2, ?MODULE, {record, user}),
    Endpoint2 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2WithBody =
        erldantic_openapi:with_request_body(Endpoint2, ?MODULE, {record, create_user_request}),
    Endpoint2WithResp = erldantic_openapi:add_response(Endpoint2WithBody, Response2WithBody),

    PathParam =
        #{name => <<"id">>,
          in => path,
          required => true,
          schema => {type, user_id, 0}},
    Response3_200 = erldantic_openapi:response(200, <<"User details">>),
    Response3_200WithBody =
        erldantic_openapi:response_with_body(Response3_200, ?MODULE, {record, user}),
    Response3_404 = erldantic_openapi:response(404, <<"User not found">>),
    Response3_404WithBody =
        erldantic_openapi:response_with_body(Response3_404, ?MODULE, {record, error_response}),

    Endpoint3 = erldantic_openapi:endpoint(get, <<"/users/{id}">>),
    Endpoint3WithParam = erldantic_openapi:with_parameter(Endpoint3, ?MODULE, PathParam),
    Endpoint3WithResp1 =
        erldantic_openapi:add_response(Endpoint3WithParam, Response3_200WithBody),
    Endpoint3WithResp2 =
        erldantic_openapi:add_response(Endpoint3WithResp1, Response3_404WithBody),

    Endpoints = [Endpoint1WithResp, Endpoint2WithResp, Endpoint3WithResp2],

    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               Endpoints),

    #{paths := #{<<"/users/{id}">> := UsersIdPath}} = OpenAPISpec,
    ?assertMatch(#{paths :=
                       #{<<"/users">> := #{get := _, post := _}, <<"/users/{id}">> := _}},
                 OpenAPISpec),
    ?assertNot(is_map_key(post, UsersIdPath)).

%% Test OpenAPI spec includes component schemas
openapi_with_components_test() ->
    Response = erldantic_openapi:response(201, <<"User created">>),
    ResponseWithBody =
        erldantic_openapi:response_with_body(Response, ?MODULE, {record, user}),
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_request_body(Endpoint1, ?MODULE, {record, create_user_request}),
    Endpoint = erldantic_openapi:add_response(Endpoint2, ResponseWithBody),

    {ok, OpenAPISpec} =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),

    ?assertMatch(#{components := #{schemas := _}}, OpenAPISpec),

    Components = maps:get(components, OpenAPISpec),
    Schemas = maps:get(schemas, Components),

    ?assert(maps:is_key(<<"User">>, Schemas) orelse maps:is_key(<<"user">>, Schemas)),
    ?assert(maps:is_key(<<"CreateUserRequest">>, Schemas)
            orelse maps:is_key(<<"create_user_request">>, Schemas)).

%% Test error handling for invalid endpoints
error_handling_test() ->
    Response = erldantic_openapi:response(200, <<"List of users">>),
    ResponseWithBody =
        erldantic_openapi:response_with_body(Response, ?MODULE, {record, non_existent_type}),
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint = erldantic_openapi:add_response(Endpoint1, ResponseWithBody),

    Result =
        erldantic_openapi:endpoints_to_openapi(#{title => <<"API Documentation">>,
                                                 version => <<"1.0.0">>},
                                               [Endpoint]),
    ?assertMatch({error, _}, Result).

%% Test with direct ed_type() values (inline schemas)
endpoint_with_direct_types_test() ->
    StringType = #ed_simple_type{type = string},
    IntegerType = #ed_simple_type{type = integer},

    Response = erldantic_openapi:response(200, <<"Success">>),
    ResponseWithBody = erldantic_openapi:response_with_body(Response, ?MODULE, IntegerType),
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/direct-types">>),
    Endpoint2 = erldantic_openapi:with_request_body(Endpoint1, ?MODULE, StringType),
    Endpoint = erldantic_openapi:add_response(Endpoint2, ResponseWithBody),

    ?assertMatch(#{request_body := #{schema := StringType, module := ?MODULE}}, Endpoint),
    ?assertMatch(#{responses := #{200 := #{schema := IntegerType, module := ?MODULE}}},
                 Endpoint).

%% Test with mixed type references and direct types
endpoint_with_mixed_types_test() ->
    DirectStringType = #ed_simple_type{type = string},
    TypeRef = {type, user, 0},

    QueryParam =
        #{name => <<"filter">>,
          in => query,
          required => false,
          schema => DirectStringType},

    Response = erldantic_openapi:response(200, <<"User data">>),
    ResponseWithBody = erldantic_openapi:response_with_body(Response, ?MODULE, TypeRef),
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/mixed-types">>),
    Endpoint2 = erldantic_openapi:add_response(Endpoint1, ResponseWithBody),
    Endpoint = erldantic_openapi:with_parameter(Endpoint2, ?MODULE, QueryParam),

    ?assertMatch(#{responses := #{200 := #{schema := TypeRef, module := ?MODULE}}}, Endpoint),
    ?assertMatch(#{parameters := [#{schema := DirectStringType, module := ?MODULE}]},
                 Endpoint).

%% Test with complex direct types
endpoint_with_complex_direct_types_test() ->
    ListType = #ed_list{type = #ed_simple_type{type = string}},
    MapType =
        #ed_map{fields =
                    [{map_field_exact, name, #ed_simple_type{type = string}},
                     {map_field_exact, age, #ed_simple_type{type = integer}}]},
    UnionType =
        #ed_union{types = [#ed_simple_type{type = string}, #ed_simple_type{type = integer}]},

    Response200 = erldantic_openapi:response(200, <<"String list">>),
    Response200WithBody =
        erldantic_openapi:response_with_body(Response200, ?MODULE, ListType),
    Response400 = erldantic_openapi:response(400, <<"Error">>),
    Response400WithBody =
        erldantic_openapi:response_with_body(Response400, ?MODULE, UnionType),

    Endpoint1 = erldantic_openapi:endpoint(post, <<"/complex-types">>),
    Endpoint2 = erldantic_openapi:with_request_body(Endpoint1, ?MODULE, MapType),
    Endpoint3 = erldantic_openapi:add_response(Endpoint2, Response200WithBody),
    Endpoint = erldantic_openapi:add_response(Endpoint3, Response400WithBody),

    ?assertMatch(#{request_body := #{schema := MapType, module := ?MODULE}}, Endpoint),
    ?assertMatch(#{responses :=
                       #{200 := #{schema := ListType, module := ?MODULE},
                         400 := #{schema := UnionType, module := ?MODULE}}},
                 Endpoint).

%% Test endpoint with custom response content type
endpoint_with_custom_response_content_type_test() ->
    Response = erldantic_openapi:response(200, <<"List of users">>),
    ResponseWithBody =
        erldantic_openapi:response_with_body(Response,
                                             ?MODULE,
                                             {record, user_list},
                                             <<"application/xml">>),
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint = erldantic_openapi:add_response(Endpoint1, ResponseWithBody),

    ?assertMatch(#{responses :=
                       #{200 :=
                             #{description := <<"List of users">>,
                               schema := {record, user_list},
                               content_type := <<"application/xml">>}}},
                 Endpoint).

%% Test endpoint with custom request body content type
endpoint_with_custom_request_body_content_type_test() ->
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint =
        erldantic_openapi:with_request_body(Endpoint1,
                                            ?MODULE,
                                            {record, create_user_request},
                                            <<"application/xml">>),

    ?assertMatch(#{request_body :=
                       #{schema := {record, create_user_request},
                         module := ?MODULE,
                         content_type := <<"application/xml">>}},
                 Endpoint).

%% Test endpoint with both custom content types
endpoint_with_both_custom_content_types_test() ->
    Response = erldantic_openapi:response(201, <<"User created">>),
    ResponseWithBody =
        erldantic_openapi:response_with_body(Response, ?MODULE, {record, user}, <<"text/plain">>),
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_request_body(Endpoint1,
                                            ?MODULE,
                                            {record, create_user_request},
                                            <<"application/xml">>),
    Endpoint = erldantic_openapi:add_response(Endpoint2, ResponseWithBody),

    ?assertMatch(#{request_body := #{content_type := <<"application/xml">>},
                   responses := #{201 := #{content_type := <<"text/plain">>}}},
                 Endpoint).

%% Test backward compatibility - endpoints without content type should default to application/json
endpoint_default_content_type_test() ->
    Response = erldantic_openapi:response(201, <<"User created">>),
    ResponseWithBody =
        erldantic_openapi:response_with_body(Response, ?MODULE, {record, user}),
    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint2 =
        erldantic_openapi:with_request_body(Endpoint1, ?MODULE, {record, create_user_request}),
    Endpoint = erldantic_openapi:add_response(Endpoint2, ResponseWithBody),

    #{request_body := RequestBody, responses := #{201 := Response201}} = Endpoint,
    ?assertNot(maps:is_key(content_type, RequestBody)),
    ?assertNot(maps:is_key(content_type, Response201)).

%% Test adding response header
endpoint_with_response_header_test() ->
    Response1 = erldantic_openapi:response(200, <<"List of users">>),
    Response2 = erldantic_openapi:response_with_body(Response1, ?MODULE, {record, user_list}),
    Response =
        erldantic_openapi:response_with_header(Response2,
                                               <<"X-Rate-Limit">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = integer},
                                                 description => <<"Request limit">>,
                                                 required => false}),
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint = erldantic_openapi:add_response(Endpoint1, Response),

    ?assertMatch(#{responses :=
                       #{200 :=
                             #{headers :=
                                   #{<<"X-Rate-Limit">> :=
                                         #{schema := #ed_simple_type{type = integer},
                                           description := <<"Request limit">>,
                                           required := false}}}}},
                 Endpoint).

%% Test adding multiple response headers
endpoint_with_multiple_response_headers_test() ->
    Response1 = erldantic_openapi:response(200, <<"List of users">>),
    Response2 = erldantic_openapi:response_with_body(Response1, ?MODULE, {record, user_list}),
    Response3 =
        erldantic_openapi:response_with_header(Response2,
                                               <<"X-Rate-Limit">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = integer}}),
    Response =
        erldantic_openapi:response_with_header(Response3,
                                               <<"X-Request-ID">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = string},
                                                 required => true}),
    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint = erldantic_openapi:add_response(Endpoint1, Response),

    ?assertMatch(#{responses :=
                       #{200 := #{headers := #{<<"X-Rate-Limit">> := _, <<"X-Request-ID">> := _}}}},
                 Endpoint).

%% Test adding response headers to different status codes
endpoint_with_headers_on_different_responses_test() ->
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
    Endpoint2 = erldantic_openapi:add_response(Endpoint1, Response201),
    Endpoint = erldantic_openapi:add_response(Endpoint2, Response429),

    ?assertMatch(#{responses :=
                       #{201 := #{headers := #{<<"Location">> := _}},
                         429 := #{headers := #{<<"Retry-After">> := _}}}},
                 Endpoint).

%% Test response builder pattern - basic usage
response_builder_basic_test() ->
    Response1 = erldantic_openapi:response(200, <<"Success">>),
    Response = erldantic_openapi:response_with_body(Response1, ?MODULE, {record, user_list}),

    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint = erldantic_openapi:add_response(Endpoint1, Response),

    #{responses := #{200 := AddedResponse}} = Endpoint,
    ?assertMatch(#{description := <<"Success">>,
                   schema := {record, user_list},
                   module := ?MODULE},
                 AddedResponse).

%% Test response builder with custom content type
response_builder_with_content_type_test() ->
    Response1 = erldantic_openapi:response(200, <<"XML Response">>),
    Response =
        erldantic_openapi:response_with_body(Response1,
                                             ?MODULE,
                                             {record, user_list},
                                             <<"application/xml">>),

    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint = erldantic_openapi:add_response(Endpoint1, Response),

    #{responses := #{200 := AddedResponse}} = Endpoint,
    ?assertMatch(#{content_type := <<"application/xml">>}, AddedResponse).

%% Test response builder with headers
response_builder_with_headers_test() ->
    Response1 = erldantic_openapi:response(200, <<"Success">>),
    Response2 = erldantic_openapi:response_with_body(Response1, ?MODULE, {record, user_list}),
    Response3 =
        erldantic_openapi:response_with_header(Response2,
                                               <<"X-Rate-Limit">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = integer},
                                                 required => false}),
    Response =
        erldantic_openapi:response_with_header(Response3,
                                               <<"X-Request-ID">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = string},
                                                 required => true}),

    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users">>),
    Endpoint = erldantic_openapi:add_response(Endpoint1, Response),

    #{responses := #{200 := AddedResponse}} = Endpoint,
    ?assertMatch(#{headers := #{<<"X-Rate-Limit">> := _, <<"X-Request-ID">> := _}},
                 AddedResponse).

%% Test response builder with everything
response_builder_complete_test() ->
    Response1 = erldantic_openapi:response(201, <<"User created">>),
    Response2 =
        erldantic_openapi:response_with_body(Response1,
                                             ?MODULE,
                                             {record, user},
                                             <<"application/json">>),
    Response3 =
        erldantic_openapi:response_with_header(Response2,
                                               <<"Location">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = string},
                                                 description => <<"URI of created resource">>,
                                                 required => true}),
    Response =
        erldantic_openapi:response_with_header(Response3,
                                               <<"X-Request-ID">>,
                                               ?MODULE,
                                               #{schema => #ed_simple_type{type = string}}),

    Endpoint1 = erldantic_openapi:endpoint(post, <<"/users">>),
    Endpoint = erldantic_openapi:add_response(Endpoint1, Response),

    #{responses := #{201 := AddedResponse}} = Endpoint,
    ?assertMatch(#{description := <<"User created">>,
                   schema := {record, user},
                   module := ?MODULE,
                   content_type := <<"application/json">>,
                   headers := #{<<"Location">> := _, <<"X-Request-ID">> := _}},
                 AddedResponse).

%% Test multiple responses built with builder pattern
response_builder_multiple_responses_test() ->
    SuccessResponse1 = erldantic_openapi:response(200, <<"Success">>),
    SuccessResponse =
        erldantic_openapi:response_with_body(SuccessResponse1, ?MODULE, {record, user}),

    ErrorResponse1 = erldantic_openapi:response(404, <<"Not found">>),
    ErrorResponse =
        erldantic_openapi:response_with_body(ErrorResponse1, ?MODULE, {record, error_response}),

    Endpoint1 = erldantic_openapi:endpoint(get, <<"/users/{id}">>),
    Endpoint2 = erldantic_openapi:add_response(Endpoint1, SuccessResponse),
    Endpoint = erldantic_openapi:add_response(Endpoint2, ErrorResponse),

    ?assertMatch(#{responses :=
                       #{200 := #{description := <<"Success">>, schema := {record, user}},
                         404 :=
                             #{description := <<"Not found">>,
                               schema := {record, error_response}}}},
                 Endpoint).
