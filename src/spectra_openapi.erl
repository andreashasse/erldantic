-module(spectra_openapi).

-export([
    endpoint/2,
    with_request_body/3, with_request_body/4,
    with_parameter/3,
    endpoints_to_openapi/2,
    response/2,
    response_with_body/3, response_with_body/4,
    response_with_header/4,
    add_response/2
]).

-ignore_xref([
    {spectra_openapi, endpoint, 2},
    {spectra_openapi, with_request_body, 3},
    {spectra_openapi, with_request_body, 4},
    {spectra_openapi, with_parameter, 3},
    {spectra_openapi, endpoints_to_openapi, 2},
    {spectra_openapi, response, 2},
    {spectra_openapi, response_with_body, 3},
    {spectra_openapi, response_with_body, 4},
    {spectra_openapi, response_with_header, 4},
    {spectra_openapi, add_response, 2}
]).

-compile(nowarn_unused_type).

-define(DEFAULT_CONTENT_TYPE, <<"application/json">>).

-type http_method() :: get | put | post | delete | options | head | patch | trace.
-type http_status_code() :: 100..599.
-type parameter_location() :: path | query | header | cookie.
-type openapi_schema() :: json:encode_value() | #{'$ref' := binary()}.
-type request_body_spec() ::
    #{
        schema := spectra:sp_type_or_ref(),
        module := module(),
        content_type => binary()
    }.
-type response_header_input_spec() ::
    #{
        description => binary(),
        required => boolean(),
        schema := spectra:sp_type_or_ref()
    }.
-type response_header_spec() ::
    #{
        description => binary(),
        required => boolean(),
        schema := spectra:sp_type_or_ref(),
        module := module()
    }.
-type response_spec() ::
    #{
        description := binary(),
        schema => spectra:sp_type_or_ref(),
        module => module(),
        status_code => http_status_code(),
        content_type => binary(),
        headers => #{binary() => response_header_spec()}
    }.
-type parameter_spec() ::
    #{
        name := binary(),
        in := parameter_location(),
        required := boolean(),
        schema := spectra:sp_type_or_ref(),
        module := module()
    }.
-type openapi_metadata() :: #{title := binary(), version := binary()}.
-type endpoint_spec() ::
    #{
        method := http_method(),
        path := binary(),
        responses := #{http_status_code() => response_spec()},
        parameters := [parameter_spec()],
        request_body => request_body_spec()
    }.
-type path_operations() :: #{http_method() => openapi_operation()}.
-type openapi_operation() ::
    #{
        responses => #{binary() => openapi_response()},
        requestBody => openapi_request_body(),
        parameters => [openapi_parameter()]
    }.
-type openapi_response() ::
    #{
        description := binary(),
        content => #{binary() => #{schema := openapi_schema()}},
        headers => #{binary() => openapi_header()}
    }.
-type openapi_header() ::
    #{
        description => binary(),
        required => boolean(),
        schema := openapi_schema()
    }.
-type openapi_request_body() ::
    #{required := boolean(), content := #{binary() => #{schema := openapi_schema()}}}.
-type openapi_parameter() ::
    #{
        name := binary(),
        in := parameter_location(),
        required := boolean(),
        schema := openapi_schema()
    }.
-type openapi_spec() ::
    #{
        openapi := binary(),
        info := #{title := binary(), version := binary()},
        paths := #{binary() => path_operations()},
        components => #{schemas => #{binary() => openapi_schema()}}
    }.

-doc """
Creates a basic endpoint specification.

This function creates the foundation for an endpoint with the specified HTTP method and path.
Additional details like responses, request body, and parameters can be added using the with_* functions.

### Returns
Endpoint map with method and path set
""".
-doc #{
    params =>
        #{
            "Method" => "HTTP method (get, post, put, delete, patch, head, options)",
            "Path" => "URL path for the endpoint (e.g., \"/users/{id}\")"
        }
}.

-spec endpoint(Method :: http_method(), Path :: binary()) -> endpoint_spec().
endpoint(Method, Path) when is_atom(Method) andalso is_binary(Path) ->
    #{
        method => Method,
        path => Path,
        responses => #{},
        parameters => []
    }.

-doc """
Creates a response builder for constructing response specifications.

This function creates a response builder that can be incrementally configured with body and headers
before being added to an endpoint using add_response/2.

### Example
```erlang
Response = spectra_openapi:response(200, <<"Success">>),
Response2 = spectra_openapi:response_with_body(Response, Module, Schema),
Response3 = spectra_openapi:response_with_header(Response2, <<"X-Rate-Limit">>, Module, HeaderSpec),
Endpoint = spectra_openapi:add_response(Endpoint1, Response3).
```

### Returns
Response builder map with status code and description
""".
-doc #{
    params =>
        #{
            "Description" => "Human-readable description of the response",
            "StatusCode" => "HTTP status code (e.g., 200, 404, 500)"
        }
}.

-spec response(StatusCode :: http_status_code(), Description :: binary()) ->
    response_spec().
response(StatusCode, Description) when
    is_integer(StatusCode) andalso is_binary(Description)
->
    #{status_code => StatusCode, description => Description}.

-doc """
Adds a complete response specification to an endpoint.

This function adds a response that was built using the response builder pattern:
response/2, response_with_body/3-4, and response_with_header/4.

### Example
```erlang
Response = spectra_openapi:response(200, <<"Success">>),
Response2 = spectra_openapi:response_with_body(Response, Module, Schema),
Response3 = spectra_openapi:response_with_header(Response2, <<"X-Rate-Limit">>, Module, HeaderSpec),
Endpoint = spectra_openapi:add_response(Endpoint1, Response3).
```

### Returns
Updated endpoint map with the response added
""".
-doc #{
    params =>
        #{
            "Endpoint" => "Endpoint map to add the response to",
            "Response" => "Response specification built with response/2 and related functions"
        }
}.

-spec add_response(Endpoint :: endpoint_spec(), Response :: response_spec()) ->
    endpoint_spec().
add_response(Endpoint, Response) when is_map(Endpoint) andalso is_map(Response) ->
    {StatusCode, ResponseWithoutStatusCode} = maps:take(status_code, Response),
    Responses = maps:get(responses, Endpoint, #{}),
    Endpoint#{responses => Responses#{StatusCode => ResponseWithoutStatusCode}}.

-doc """
Adds a response body to a response builder.

This function sets the schema and module for the response body.
Use this with response/2 to build up a complete response specification.

### Returns
Updated response builder with body schema added
""".
-doc #{
    params =>
        #{
            "Module" => "Module containing the type definition",
            "Response" => "Response builder created with response/2",
            "Schema" => "Schema reference or direct type (spectra:sp_type_or_ref())"
        }
}.

-spec response_with_body(
    Response :: response_spec(),
    Module :: module(),
    Schema :: spectra:sp_type_or_ref()
) ->
    response_spec().
response_with_body(Response, Module, Schema) when
    is_map(Response) andalso is_atom(Module)
->
    Response#{schema => Schema, module => Module}.

-doc """
Adds a response body with custom content type to a response builder.

This function sets the schema, module, and content type for the response body.
Use this with response/2 to build up a complete response specification.

### Returns
Updated response builder with body schema and content type added
""".
-doc #{
    params =>
        #{
            "ContentType" =>
                "Content type for the response body (e.g., \"application/json\", \"application/xml\")",
            "Module" => "Module containing the type definition",
            "Response" => "Response builder created with response/2",
            "Schema" => "Schema reference or direct type (spectra:sp_type_or_ref())"
        }
}.

-spec response_with_body(
    Response :: response_spec(),
    Module :: module(),
    Schema :: spectra:sp_type_or_ref(),
    ContentType :: binary()
) ->
    response_spec().
response_with_body(Response, Module, Schema, ContentType) when
    is_map(Response) andalso is_atom(Module) andalso is_binary(ContentType)
->
    Response#{
        schema => Schema,
        module => Module,
        content_type => ContentType
    }.

-doc """
Adds a header to a response builder.

This function adds a header specification to the response being built.
Multiple headers can be added by calling this function multiple times.

### Returns
Updated response builder with header added
""".
-doc #{
    params =>
        #{
            "HeaderName" => "Name of the response header (e.g., \"X-Rate-Limit\")",
            "HeaderSpec" => "Header specification (response_header_input_spec map)",
            "Module" => "Module containing the type definition",
            "Response" => "Response builder created with response/2"
        }
}.

-spec response_with_header(
    Response :: response_spec(),
    HeaderName :: binary(),
    Module :: module(),
    HeaderSpec :: response_header_input_spec()
) ->
    response_spec().
response_with_header(Response, HeaderName, Module, HeaderSpec) when
    is_map(Response) andalso
        is_binary(HeaderName) andalso
        is_atom(Module) andalso
        is_map(HeaderSpec)
->
    Headers = maps:get(headers, Response, #{}),
    HeaderSpecWithModule = HeaderSpec#{module => Module},
    Response#{headers => Headers#{HeaderName => HeaderSpecWithModule}}.

-doc """
Adds a request body specification to an endpoint.

This function sets the request body schema for the endpoint.
Typically used with POST, PUT, and PATCH endpoints.

### Returns
Updated endpoint map with request body set
""".
-doc #{
    params =>
        #{
            "Endpoint" => "Endpoint map to add the request body to",
            "Module" => "Module containing the type definition",
            "Schema" => "Schema reference or direct type (spectra:sp_type_or_ref())"
        }
}.

-spec with_request_body(
    Endpoint :: endpoint_spec(),
    Module :: module(),
    Schema :: spectra:sp_type_or_ref()
) ->
    endpoint_spec().
with_request_body(Endpoint, Module, Schema) when
    is_map(Endpoint) andalso is_atom(Module)
->
    Endpoint#{request_body => #{schema => Schema, module => Module}}.

-doc """
Adds a request body specification with custom content type to an endpoint.

This function sets the request body schema and content type for the endpoint.
Typically used with POST, PUT, and PATCH endpoints.

### Returns
Updated endpoint map with request body set
""".
-doc #{
    params =>
        #{
            "ContentType" =>
                "Content type for the request body (e.g., \"application/json\", \"application/xml\")",
            "Endpoint" => "Endpoint map to add the request body to",
            "Module" => "Module containing the type definition",
            "Schema" => "Schema reference or direct type (spectra:sp_type_or_ref())"
        }
}.

-spec with_request_body(
    Endpoint :: endpoint_spec(),
    Module :: module(),
    Schema :: spectra:sp_type_or_ref(),
    ContentType :: binary()
) ->
    endpoint_spec().
with_request_body(Endpoint, Module, Schema, ContentType) when
    is_map(Endpoint) andalso is_atom(Module) andalso is_binary(ContentType)
->
    Endpoint#{
        request_body =>
            #{
                schema => Schema,
                module => Module,
                content_type => ContentType
            }
    }.

-doc """
Adds a parameter specification to an endpoint.

This function adds a parameter (path, query, header, or cookie) to the endpoint.
Multiple parameters can be added by calling this function multiple times.

### Parameter Specification
The parameter spec should be a map with these keys:
- name: Parameter name (binary)
- in: Parameter location (path | query | header | cookie)
- required: Whether the parameter is required (boolean)
- schema: Schema reference or direct type (spectra:sp_type_or_ref())

### Returns
Updated endpoint map with the new parameter added
""".
-doc #{
    params =>
        #{
            "Endpoint" => "Endpoint map to add the parameter to",
            "Module" => "Module containing the type definition",
            "ParameterSpec" => "Parameter specification map"
        }
}.

-spec with_parameter(
    Endpoint :: endpoint_spec(),
    Module :: module(),
    ParameterSpec :: parameter_spec()
) ->
    endpoint_spec().
with_parameter(Endpoint, Module, #{name := Name} = ParameterSpec) when
    is_map(Endpoint) andalso
        is_atom(Module) andalso
        is_map(ParameterSpec) andalso
        is_binary(Name)
->
    Parameters = maps:get(parameters, Endpoint, []),
    ParameterWithModule = ParameterSpec#{module => Module},
    Endpoint#{parameters => [ParameterWithModule | Parameters]}.

-doc """
Generates a complete OpenAPI 3.0 specification from a list of endpoints.

This function takes a list of endpoint specifications and generates a complete OpenAPI document
with paths, operations, and component schemas.

### Returns
{ok, OpenAPISpec} containing the complete OpenAPI 3.0 document, or {error, Errors} if generation fails
""".
-doc #{
    params =>
        #{
            "Endpoints" =>
                "List of endpoint specifications created with endpoint/2 and with_* functions",
            "MetaData" => "OpenAPI metadata map with title and version"
        }
}.

-spec endpoints_to_openapi(
    MetaData :: openapi_metadata(),
    Endpoints :: [endpoint_spec()]
) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
endpoints_to_openapi(MetaData, Endpoints) when is_list(Endpoints) ->
    PathGroups = group_endpoints_by_path(Endpoints),
    Paths =
        maps:fold(
            fun(Path, PathEndpoints, Acc) ->
                PathOps = generate_path_operations(PathEndpoints),
                Acc#{Path => PathOps}
            end,
            #{},
            PathGroups
        ),

    SchemaRefs = collect_schema_refs(Endpoints),
    case generate_components(SchemaRefs) of
        {ok, ComponentsResult} ->
            OpenAPISpec =
                #{
                    openapi => <<"3.0.0">>,
                    info =>
                        #{
                            title => maps:get(title, MetaData),
                            version => maps:get(version, MetaData)
                        },
                    paths => Paths,
                    components => ComponentsResult
                },
            spectra_json:to_json(?MODULE, {type, openapi_spec, 0}, OpenAPISpec);
        {error, _} = Err ->
            Err
    end.

-spec group_endpoints_by_path([endpoint_spec()]) -> #{binary() => [endpoint_spec()]}.
group_endpoints_by_path(Endpoints) ->
    lists:foldl(
        fun(Endpoint, Acc) ->
            Path = maps:get(path, Endpoint),
            PathEndpoints = maps:get(Path, Acc, []),
            maps:put(Path, [Endpoint | PathEndpoints], Acc)
        end,
        #{},
        Endpoints
    ).

-spec generate_path_operations([endpoint_spec()]) -> path_operations().
generate_path_operations(Endpoints) ->
    lists:foldl(
        fun(#{method := Method} = Endpoint, Acc) ->
            Operation = generate_operation(Endpoint),
            Acc#{Method => Operation}
        end,
        #{},
        Endpoints
    ).

-spec generate_operation(endpoint_spec()) -> openapi_operation().
generate_operation(Endpoint) ->
    Operation = #{},

    %% Add responses
    Responses = maps:get(responses, Endpoint, #{}),
    OperationWithResponses =
        case maps:size(Responses) > 0 of
            true ->
                OpenAPIResponses =
                    maps:map(
                        fun(_StatusCode, ResponseSpec) -> generate_response(ResponseSpec) end,
                        Responses
                    ),
                OpenAPIResponsesBinary =
                    maps:fold(
                        fun(K, V, NewAcc) ->
                            BinaryKey = integer_to_binary(K),
                            NewAcc#{BinaryKey => V}
                        end,
                        #{},
                        OpenAPIResponses
                    ),
                Operation#{responses => OpenAPIResponsesBinary};
            false ->
                Operation
        end,

    OperationWithBody =
        case maps:get(request_body, Endpoint, undefined) of
            undefined ->
                OperationWithResponses;
            RequestBodyRef ->
                RequestBody = generate_request_body(RequestBodyRef),
                OperationWithResponses#{requestBody => RequestBody}
        end,

    Parameters = maps:get(parameters, Endpoint, []),
    case Parameters of
        [] ->
            OperationWithBody;
        _ ->
            OpenAPIParameters = lists:map(fun generate_parameter/1, Parameters),
            OperationWithBody#{parameters => OpenAPIParameters}
    end.

-spec generate_response(response_spec()) -> openapi_response().
generate_response(#{description := Description} = ResponseSpec) when
    is_binary(Description)
->
    %% Build base response with optional content
    BaseResponse =
        case
            {maps:get(schema, ResponseSpec, undefined), maps:get(module, ResponseSpec, undefined)}
        of
            {undefined, _} ->
                %% No schema means header-only response (no content)
                #{description => Description};
            {_, undefined} ->
                %% Schema without module should not happen, but handle defensively
                #{description => Description};
            {Schema, Module} ->
                ModuleTypeInfo = spectra_abstract_code:types_in_module(Module),
                SchemaContent =
                    case Schema of
                        {type, Name, Arity} ->
                            SchemaName = type_ref_to_component_name({type, Name, Arity}),
                            #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
                        {record, Name} ->
                            SchemaName = type_ref_to_component_name({record, Name}),
                            #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
                        DirectType ->
                            {ok, InlineSchema} =
                                spectra_json_schema:to_schema(ModuleTypeInfo, DirectType),
                            InlineSchema
                    end,
                ContentType = maps:get(content_type, ResponseSpec, ?DEFAULT_CONTENT_TYPE),
                #{
                    description => Description,
                    content => #{ContentType => #{schema => SchemaContent}}
                }
        end,

    %% Add headers if present
    case maps:get(headers, ResponseSpec, #{}) of
        HeadersSpec when map_size(HeadersSpec) =:= 0 ->
            BaseResponse;
        HeadersSpec ->
            GeneratedHeaders =
                maps:map(
                    fun(_HeaderName, HeaderSpec) -> generate_response_header(HeaderSpec) end,
                    HeadersSpec
                ),
            BaseResponse#{headers => GeneratedHeaders}
    end.

-spec generate_response_header(response_header_spec()) -> openapi_header().
generate_response_header(#{schema := Schema, module := Module} = HeaderSpec) ->
    ModuleTypeInfo = spectra_abstract_code:types_in_module(Module),
    {ok, InlineSchema} = spectra_json_schema:to_schema(ModuleTypeInfo, Schema),

    BaseHeader = #{schema => InlineSchema},

    %% Add optional description
    HeaderWithDesc =
        case maps:get(description, HeaderSpec, undefined) of
            undefined ->
                BaseHeader;
            Description ->
                BaseHeader#{description => Description}
        end,

    %% Add optional required flag
    case maps:get(required, HeaderSpec, undefined) of
        undefined ->
            HeaderWithDesc;
        Required ->
            HeaderWithDesc#{required => Required}
    end.

-spec generate_request_body(request_body_spec()) -> openapi_request_body().
generate_request_body(#{schema := Schema, module := Module} = RequestBodySpec) ->
    ModuleTypeInfo = spectra_abstract_code:types_in_module(Module),
    SchemaContent =
        case Schema of
            {type, Name, Arity} ->
                SchemaName = type_ref_to_component_name({type, Name, Arity}),
                #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
            {record, Name} ->
                SchemaName = type_ref_to_component_name({record, Name}),
                #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
            DirectType ->
                {ok, InlineSchema} = spectra_json_schema:to_schema(ModuleTypeInfo, DirectType),
                InlineSchema
        end,

    ContentType = maps:get(content_type, RequestBodySpec, ?DEFAULT_CONTENT_TYPE),
    #{required => true, content => #{ContentType => #{schema => SchemaContent}}}.

-spec generate_parameter(parameter_spec()) -> openapi_parameter().
generate_parameter(
    #{
        name := Name,
        in := In,
        schema := Schema,
        module := Module
    } =
        ParameterSpec
) when
    is_binary(Name)
->
    ModuleTypeInfo = spectra_abstract_code:types_in_module(Module),
    Required = maps:get(required, ParameterSpec, false),

    {ok, InlineSchema} = spectra_json_schema:to_schema(ModuleTypeInfo, Schema),

    #{
        name => Name,
        in => In,
        required => Required,
        schema => InlineSchema
    }.

-spec collect_schema_refs([endpoint_spec()]) -> [{module(), spectra:sp_type_or_ref()}].
collect_schema_refs(Endpoints) ->
    lists:foldl(
        fun(Endpoint, Acc) ->
            EndpointRefs = collect_endpoint_schema_refs(Endpoint),
            lists:usort(EndpointRefs ++ Acc)
        end,
        [],
        Endpoints
    ).

-spec collect_endpoint_schema_refs(endpoint_spec()) ->
    [{module(), spectra:sp_type_or_ref()}].
collect_endpoint_schema_refs(
    #{responses := Responses, parameters := Parameters} =
        Endpoint
) ->
    ResponseRefs = collect_response_refs(Responses),
    RequestBodyRefs =
        case maps:get(request_body, Endpoint, undefined) of
            undefined ->
                [];
            #{schema := Schema, module := Module} ->
                case spectra_type:is_type_reference(Schema) of
                    true ->
                        [{Module, Schema}];
                    false ->
                        []
                end
        end,
    ParameterRefs = collect_parameter_refs(Parameters),

    ResponseRefs ++ RequestBodyRefs ++ ParameterRefs.

-spec collect_response_refs(#{http_status_code() => response_spec()}) ->
    [{module(), spectra:sp_type_or_ref()}].
collect_response_refs(Responses) ->
    maps:fold(
        fun(_StatusCode, ResponseSpec, Acc) ->
            case
                {
                    maps:get(schema, ResponseSpec, undefined),
                    maps:get(module, ResponseSpec, undefined)
                }
            of
                {undefined, _} ->
                    %% Header-only response, no schema reference
                    Acc;
                {_, undefined} ->
                    %% Schema without module should not happen
                    Acc;
                {Schema, Module} ->
                    case spectra_type:is_type_reference(Schema) of
                        true ->
                            [{Module, Schema} | Acc];
                        false ->
                            Acc
                    end
            end
        end,
        [],
        Responses
    ).

-spec collect_parameter_refs([parameter_spec()]) ->
    [{module(), spectra:sp_type_or_ref()}].
collect_parameter_refs(Parameters) ->
    lists:filtermap(
        fun(#{schema := Schema, module := Module}) ->
            case spectra_type:is_type_reference(Schema) of
                true ->
                    {true, {Module, Schema}};
                false ->
                    false
            end
        end,
        Parameters
    ).

-spec generate_components([{module(), spectra:sp_type_or_ref()}]) ->
    {ok, #{schemas => #{binary() => openapi_schema()}}}
    | {error, [spectra:error()]}.
generate_components(SchemaRefs) ->
    case
        spectra_util:fold_until_error(
            fun({Module, TypeRef}, Acc) ->
                case
                    spectra_json_schema:to_schema(
                        spectra_abstract_code:types_in_module(Module),
                        TypeRef
                    )
                of
                    {ok, Schema} when is_map(Schema) ->
                        SchemaName =
                            type_ref_to_component_name(TypeRef),
                        {ok, Acc#{SchemaName => Schema}};
                    {error, _} = Error ->
                        Error
                end
            end,
            #{},
            SchemaRefs
        )
    of
        {ok, Schemas} ->
            ComponentsMap =
                case maps:size(Schemas) > 0 of
                    true ->
                        #{schemas => Schemas};
                    false ->
                        #{}
                end,
            {ok, ComponentsMap};
        {error, _} = Error ->
            Error
    end.

-spec type_ref_to_component_name(spectra:sp_type_reference()) -> binary().
type_ref_to_component_name({type, TypeName, Arity}) ->
    TypeStr = atom_to_list(TypeName),
    Words = string:split(TypeStr, "_", all),
    PascalCase = lists:map(fun capitalize_word/1, Words),
    ArityStr = integer_to_list(Arity),
    iolist_to_binary([PascalCase, ArityStr]);
type_ref_to_component_name({record, RecordName}) ->
    TypeStr = atom_to_list(RecordName),
    Words = string:split(TypeStr, "_", all),
    PascalCase = lists:map(fun capitalize_word/1, Words),
    iolist_to_binary(PascalCase).

capitalize_word([]) ->
    [];
capitalize_word([First | Rest]) ->
    [string:to_upper(First) | Rest].
