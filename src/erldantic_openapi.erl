-module(erldantic_openapi).

-export([endpoint/2, with_response/5, with_request_body/3, with_parameter/3,
         endpoints_to_openapi/1]).

-ignore_xref([{erldantic_openapi, type_to_schema, 2},
              {erldantic_openapi, record_to_schema, 2},
              {erldantic_openapi, endpoint, 2},
              {erldantic_openapi, with_response, 5},
              {erldantic_openapi, with_request_body, 3},
              {erldantic_openapi, with_parameter, 3},
              {erldantic_openapi, endpoints_to_openapi, 1}]).

-include("../include/erldantic.hrl").

-compile(nowarn_unused_type).

-type http_method() :: get | put | post | delete | options | head | patch | trace.
-type http_status_code() :: 100..599.
-type parameter_location() :: path | query | header | cookie.
-type openapi_schema() :: json:encode_value() | #{'$ref' := binary()}.
-type request_body_spec() :: #{schema := erldantic:ed_type_or_ref(), module := module()}.
-type response_spec() ::
    #{description := binary(),
      schema := erldantic:ed_type_or_ref(),
      module := module()}.
-type parameter_spec() ::
    #{name := binary(),
      in := parameter_location(),
      required := boolean(),
      schema := erldantic:ed_type_or_ref(),
      module := module()}.
-type openapi_metadata() :: #{title := binary(), version := binary()}.
-type endpoint_spec() ::
    #{method := http_method(),
      path := binary(),
      responses := #{http_status_code() => response_spec()},
      parameters := [parameter_spec()],
      request_body => request_body_spec()}.
-type path_operations() :: #{http_method() => openapi_operation()}.
-type openapi_operation() ::
    #{responses => #{binary() => openapi_response()},
      requestBody => openapi_request_body(),
      parameters => [openapi_parameter()]}.
-type openapi_response() ::
    #{description := binary(), content := #{binary() => #{schema := openapi_schema()}}}.
-type openapi_request_body() ::
    #{required := boolean(), content := #{binary() => #{schema := openapi_schema()}}}.
-type openapi_parameter() ::
    #{name := binary(),
      in := parameter_location(),
      required := boolean(),
      schema := openapi_schema()}.
-type openapi_spec() ::
    #{openapi := binary(),
      info := #{title := binary(), version := binary()},
      paths := #{binary() => path_operations()},
      components => #{schemas => #{binary() => openapi_schema()}}}.

-doc("Creates a basic endpoint specification.\nThis function creates the foundation for an endpoint with the specified HTTP method and path.\nAdditional details like responses, request body, and parameters can be added using the with_* functions.\n\n### Returns\nEndpoint map with method and path set").
-doc(#{params =>
           #{"Method" => "HTTP method (get, post, put, delete, patch, head, options)",
             "Path" => "URL path for the endpoint (e.g., \"/users/{id}\")"}}).

-spec endpoint(Method :: http_method(), Path :: binary()) -> endpoint_spec().
endpoint(Method, Path) when is_atom(Method) andalso is_binary(Path) ->
    #{method => Method,
      path => Path,
      responses => #{},
      parameters => []}.

-doc("Adds a response specification to an endpoint.\nThis function adds a response with the specified status code, description, and schema.\nMultiple responses can be added to the same endpoint by calling this function multiple times.\n\n### Returns\nUpdated endpoint map with the new response added").
-doc(#{params =>
           #{"Description" => "Human-readable description of the response",
             "Endpoint" => "Endpoint map to add the response to",
             "Schema" => "Schema reference or direct type (erldantic:ed_type_or_ref())",
             "StatusCode" => "HTTP status code (e.g., 200, 404, 500)"}}).

-spec with_response(Endpoint :: endpoint_spec(),
                    StatusCode :: http_status_code(),
                    Description :: binary(),
                    Module :: module(),
                    Schema :: erldantic:ed_type_or_ref()) ->
                       endpoint_spec().
with_response(Endpoint, StatusCode, Description, Module, Schema)
    when is_map(Endpoint)
         andalso is_integer(StatusCode)
         andalso is_binary(Description)
         andalso is_atom(Module) ->
    ResponseSpec =
        #{description => Description,
          schema => Schema,
          module => Module},
    Responses = maps:get(responses, Endpoint, #{}),
    Endpoint#{responses => Responses#{StatusCode => ResponseSpec}}.

-doc("Adds a request body specification to an endpoint.\nThis function sets the request body schema for the endpoint.\nTypically used with POST, PUT, and PATCH endpoints.\n\n### Returns\nUpdated endpoint map with request body set").
-doc(#{params =>
           #{"Endpoint" => "Endpoint map to add the request body to",
             "Schema" => "Schema reference or direct type (erldantic:ed_type_or_ref())"}}).

-spec with_request_body(Endpoint :: endpoint_spec(),
                        Module :: module(),
                        Schema :: erldantic:ed_type_or_ref()) ->
                           endpoint_spec().
with_request_body(Endpoint, Module, Schema)
    when is_map(Endpoint) andalso is_atom(Module) ->
    Endpoint#{request_body => #{schema => Schema, module => Module}}.

-doc("Adds a parameter specification to an endpoint.\nThis function adds a parameter (path, query, header, or cookie) to the endpoint.\nMultiple parameters can be added by calling this function multiple times.\n\n### Parameter Specification\nThe parameter spec should be a map with these keys:\n- name: Parameter name (binary)\n- in: Parameter location (path | query | header | cookie)\n- required: Whether the parameter is required (boolean)\n- schema: Schema reference or direct type (erldantic:ed_type_or_ref())\n\n### Returns\nUpdated endpoint map with the new parameter added").
-doc(#{params =>
           #{"Endpoint" => "Endpoint map to add the parameter to",
             "ParameterSpec" => "Parameter specification map"}}).

-spec with_parameter(Endpoint :: endpoint_spec(),
                     Module :: module(),
                     ParameterSpec :: parameter_spec()) ->
                        endpoint_spec().
with_parameter(Endpoint, Module, #{name := Name} = ParameterSpec)
    when is_map(Endpoint)
         andalso is_atom(Module)
         andalso is_map(ParameterSpec)
         andalso is_binary(Name) ->
    Parameters = maps:get(parameters, Endpoint, []),
    ParameterWithModule = ParameterSpec#{module => Module},
    Endpoint#{parameters => [ParameterWithModule | Parameters]}.

-doc("Generates a complete OpenAPI 3.0 specification from a list of endpoints.\nThis function takes a list of endpoint specifications and generates a complete OpenAPI document\nwith paths, operations, and component schemas.\n\n### Returns\n{ok, OpenAPISpec} containing the complete OpenAPI 3.0 document, or {error, Errors} if generation fails").
-doc(#{params =>
           #{"Endpoints" =>
                 "List of endpoint specifications created with endpoint/2 and with_* functions"}}).

-spec endpoints_to_openapi(MetaData :: openapi_metadata(), Endpoints :: [endpoint_spec()]) ->
                              {ok, json:encode_value()} | {error, [erldantic:error()]}.
endpoints_to_openapi(MetaData, Endpoints) when is_list(Endpoints) ->
    PathGroups = group_endpoints_by_path(Endpoints),
    Paths =
        maps:fold(fun(Path, PathEndpoints, Acc) ->
                     PathOps = generate_path_operations(PathEndpoints),
                     Acc#{Path => PathOps}
                  end,
                  #{},
                  PathGroups),

    SchemaRefs = collect_schema_refs(Endpoints),
    case generate_components(SchemaRefs) of
        {ok, ComponentsResult} ->
            OpenAPISpec =
                #{openapi => <<"3.0.0">>,
                  info => #{title => maps:get(MetaData, title), version => maps:get(MetaData, version)},
                  paths => Paths,
                  components => ComponentsResult},
            erldantic_json:to_json(?MODULE, {type, openapi_spec, 0}, OpenAPISpec);
        {error, _} = Err ->
            Err
    end.

-spec group_endpoints_by_path([endpoint_spec()]) -> #{binary() => [endpoint_spec()]}.
group_endpoints_by_path(Endpoints) ->
    lists:foldl(fun(Endpoint, Acc) ->
                   Path = maps:get(path, Endpoint),
                   PathEndpoints = maps:get(Path, Acc, []),
                   maps:put(Path, [Endpoint | PathEndpoints], Acc)
                end,
                #{},
                Endpoints).

-spec generate_path_operations([endpoint_spec()]) -> path_operations().
generate_path_operations(Endpoints) ->
    lists:foldl(fun(#{method := Method} = Endpoint, Acc) ->
                   Operation = generate_operation(Endpoint),
                   Acc#{Method => Operation}
                end,
                #{},
                Endpoints).

-spec generate_operation(endpoint_spec()) -> openapi_operation().
generate_operation(Endpoint) ->
    Operation = #{},

    %% Add responses
    Responses = maps:get(responses, Endpoint, #{}),
    OperationWithResponses =
        case maps:size(Responses) > 0 of
            true ->
                OpenAPIResponses =
                    maps:map(fun(_StatusCode, ResponseSpec) -> generate_response(ResponseSpec) end,
                             Responses),
                OpenAPIResponsesBinary =
                    maps:fold(fun(K, V, NewAcc) ->
                                 BinaryKey = integer_to_binary(K),
                                 NewAcc#{BinaryKey => V}
                              end,
                              #{},
                              OpenAPIResponses),
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
generate_response(#{description := Description,
                    schema := Schema,
                    module := Module})
    when is_binary(Description) ->
    ModuleTypeInfo = erldantic_abstract_code:types_in_module(Module),

    SchemaContent =
        case Schema of
            {type, Name, Arity} ->
                SchemaName = type_ref_to_component_name({type, Name, Arity}),
                #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
            {record, Name} ->
                SchemaName = type_ref_to_component_name({record, Name}),
                #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
            DirectType ->
                {ok, InlineSchema} = erldantic_json_schema:to_schema(ModuleTypeInfo, DirectType),
                InlineSchema
        end,

    #{description => Description,
      content => #{<<"application/json">> => #{schema => SchemaContent}}}.

-spec generate_request_body(request_body_spec()) -> openapi_request_body().
generate_request_body(#{schema := Schema, module := Module}) ->
    ModuleTypeInfo = erldantic_abstract_code:types_in_module(Module),
    SchemaContent =
        case Schema of
            {type, Name, Arity} ->
                SchemaName = type_ref_to_component_name({type, Name, Arity}),
                #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
            {record, Name} ->
                SchemaName = type_ref_to_component_name({record, Name}),
                #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
            DirectType ->
                {ok, InlineSchema} = erldantic_json_schema:to_schema(ModuleTypeInfo, DirectType),
                InlineSchema
        end,

    #{required => true, content => #{<<"application/json">> => #{schema => SchemaContent}}}.

-spec generate_parameter(parameter_spec()) -> openapi_parameter().
generate_parameter(#{name := Name,
                     in := In,
                     schema := Schema,
                     module := Module} =
                       ParameterSpec)
    when is_binary(Name) ->
    ModuleTypeInfo = erldantic_abstract_code:types_in_module(Module),
    Required = maps:get(required, ParameterSpec, false),

    {ok, InlineSchema} = erldantic_json_schema:to_schema(ModuleTypeInfo, Schema),

    #{name => Name,
      in => In,
      required => Required,
      schema => InlineSchema}.

-spec collect_schema_refs([endpoint_spec()]) -> [{module(), erldantic:ed_type_or_ref()}].
collect_schema_refs(Endpoints) ->
    lists:foldl(fun(Endpoint, Acc) ->
                   EndpointRefs = collect_endpoint_schema_refs(Endpoint),
                   lists:usort(EndpointRefs ++ Acc)
                end,
                [],
                Endpoints).

-spec collect_endpoint_schema_refs(endpoint_spec()) ->
                                      [{module(), erldantic:ed_type_or_ref()}].
collect_endpoint_schema_refs(#{responses := Responses, parameters := Parameters} =
                                 Endpoint) ->
    ResponseRefs = collect_response_refs(Responses),
    RequestBodyRefs =
        case maps:get(request_body, Endpoint, undefined) of
            undefined ->
                [];
            #{schema := Schema, module := Module} ->
                case erldantic_type:is_type_reference(Schema) of
                    true ->
                        [{Module, Schema}];
                    false ->
                        []
                end
        end,
    ParameterRefs = collect_parameter_refs(Parameters),

    ResponseRefs ++ RequestBodyRefs ++ ParameterRefs.

-spec collect_response_refs(#{http_status_code() => response_spec()}) ->
                               [{module(), erldantic:ed_type_or_ref()}].
collect_response_refs(Responses) ->
    maps:fold(fun(_StatusCode, #{schema := Schema, module := Module}, Acc) ->
                 case erldantic_type:is_type_reference(Schema) of
                     true ->
                         [{Module, Schema} | Acc];
                     false ->
                         Acc
                 end
              end,
              [],
              Responses).

-spec collect_parameter_refs([parameter_spec()]) ->
                                [{module(), erldantic:ed_type_or_ref()}].
collect_parameter_refs(Parameters) ->
    lists:filtermap(fun(#{schema := Schema, module := Module}) ->
                       case erldantic_type:is_type_reference(Schema) of
                           true ->
                               {true, {Module, Schema}};
                           false ->
                               false
                       end
                    end,
                    Parameters).

-spec generate_components([{module(), erldantic:ed_type_or_ref()}]) ->
                             {ok, #{schemas => #{binary() => openapi_schema()}}} |
                             {error, [erldantic:error()]}.
generate_components(SchemaRefs) ->
    case erldantic_util:fold_until_error(fun({Module, TypeRef}, Acc) ->
                                            case erldantic_json_schema:to_schema(
                                                     erldantic_abstract_code:types_in_module(Module),
                                                     TypeRef)
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
                                         SchemaRefs)
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

-spec type_ref_to_component_name(erldantic:ed_type_reference()) -> binary().
type_ref_to_component_name({type, TypeName, Arity}) ->
    TypeStr = atom_to_list(TypeName),
    Words = string:split(TypeStr, "_", all),
    PascalCase = lists:map(fun capitalize_word/1, Words),
    ArityStr = integer_to_list(Arity),
    list_to_binary(lists:flatten(PascalCase ++ [ArityStr]));
type_ref_to_component_name({record, RecordName}) ->
    TypeStr = atom_to_list(RecordName),
    Words = string:split(TypeStr, "_", all),
    PascalCase = lists:map(fun capitalize_word/1, Words),
    list_to_binary(lists:flatten(PascalCase)).

capitalize_word([]) ->
    [];
capitalize_word([First | Rest]) ->
    [string:to_upper(First) | Rest].
