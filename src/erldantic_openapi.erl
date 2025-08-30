-module(erldantic_openapi).

-export([endpoint/2, with_response/4, with_request_body/2, with_parameter/2,
         endpoints_to_openapi/1]).

-ignore_xref([{erldantic_openapi, type_to_schema, 2},
              {erldantic_openapi, record_to_schema, 2},
              {erldantic_openapi, endpoint, 2},
              {erldantic_openapi, with_response, 4},
              {erldantic_openapi, with_request_body, 2},
              {erldantic_openapi, with_parameter, 2},
              {erldantic_openapi, endpoints_to_openapi, 1}]).

-include("../include/erldantic.hrl").

%% API

-doc("Creates a basic endpoint specification.\nThis function creates the foundation for an endpoint with the specified HTTP method and path.\nAdditional details like responses, request body, and parameters can be added using the with_* functions.\n\n### Returns\nEndpoint map with method and path set").
-doc(#{params =>
           #{"Method" => "HTTP method (get, post, put, delete, patch, head, options)",
             "Path" => "URL path for the endpoint (e.g., \"/users/{id}\")"}}).

-spec endpoint(Method :: atom(), Path :: string()) -> map().
endpoint(Method, Path) when is_atom(Method) andalso is_list(Path) ->
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

-spec with_response(Endpoint :: map(),
                    StatusCode :: integer(),
                    Description :: string(),
                    Schema :: erldantic:ed_type_or_ref()) ->
                       map().
with_response(Endpoint, StatusCode, Description, Schema)
    when is_map(Endpoint) andalso is_integer(StatusCode) andalso is_list(Description) ->
    ResponseSpec = #{description => Description, schema => Schema},
    Responses = maps:get(responses, Endpoint, #{}),
    Endpoint#{responses => Responses#{StatusCode => ResponseSpec}}.

-doc("Adds a request body specification to an endpoint.\nThis function sets the request body schema for the endpoint.\nTypically used with POST, PUT, and PATCH endpoints.\n\n### Returns\nUpdated endpoint map with request body set").
-doc(#{params =>
           #{"Endpoint" => "Endpoint map to add the request body to",
             "Schema" => "Schema reference or direct type (erldantic:ed_type_or_ref())"}}).

-spec with_request_body(Endpoint :: map(), Schema :: erldantic:ed_type_or_ref()) -> map().
with_request_body(Endpoint, Schema) when is_map(Endpoint) ->
    Endpoint#{request_body => Schema}.

-doc("Adds a parameter specification to an endpoint.\nThis function adds a parameter (path, query, header, or cookie) to the endpoint.\nMultiple parameters can be added by calling this function multiple times.\n\n### Parameter Specification\nThe parameter spec should be a map with these keys:\n- name: Parameter name (string)\n- in: Parameter location (path | query | header | cookie)\n- required: Whether the parameter is required (boolean)\n- schema: Schema reference or direct type (erldantic:ed_type_or_ref())\n\n### Returns\nUpdated endpoint map with the new parameter added").
-doc(#{params =>
           #{"Endpoint" => "Endpoint map to add the parameter to",
             "ParameterSpec" => "Parameter specification map"}}).

-spec with_parameter(Endpoint :: map(), ParameterSpec :: map()) -> map().
with_parameter(Endpoint, ParameterSpec)
    when is_map(Endpoint) andalso is_map(ParameterSpec) ->
    Parameters = maps:get(parameters, Endpoint, []),
    Endpoint#{parameters => [ParameterSpec | Parameters]}.

-doc("Generates a complete OpenAPI 3.0 specification from a list of endpoints.\nThis function takes a list of endpoint specifications and generates a complete OpenAPI document\nwith paths, operations, and component schemas.\n\n### Returns\n{ok, OpenAPISpec} containing the complete OpenAPI 3.0 document, or {error, Errors} if generation fails").
-doc(#{params =>
           #{"Endpoints" =>
                 "List of endpoint specifications created with endpoint/2 and with_* functions"}}).

-spec endpoints_to_openapi(Endpoints :: [map()]) ->
                              {ok, map()} | {error, [erldantic:error()]}.
endpoints_to_openapi(Endpoints) when is_list(Endpoints) ->
    try
        %% Group endpoints by path
        PathGroups = group_endpoints_by_path(Endpoints),

        %% Generate paths section
        Paths =
            maps:fold(fun(Path, PathEndpoints, Acc) ->
                         BinaryPath = unicode:characters_to_binary(Path),
                         PathOps = generate_path_operations(PathEndpoints),
                         Acc#{BinaryPath => PathOps}
                      end,
                      #{},
                      PathGroups),

        %% Collect all schema references
        SchemaRefs = collect_schema_refs(Endpoints),

        %% Generate component schemas
        Components =
            case generate_components(SchemaRefs) of
                {ok, ComponentsResult} ->
                    ComponentsResult;
                {error, ComponentErrors} ->
                    throw({schema_generation_failed, ComponentErrors})
            end,

        %% Build complete OpenAPI spec
        OpenAPISpec =
            #{openapi => <<"3.0.0">>,
              info => #{title => <<"API Documentation">>, version => <<"1.0.0">>},
              paths => Paths,
              components => Components},

        {ok, OpenAPISpec}
    catch
        {schema_generation_failed, ThrowErrors} ->
            {error, ThrowErrors};
        error:Reason:Stacktrace ->
            {error,
             [#ed_error{type = no_match,
                        location = [endpoints_to_openapi],
                        ctx = #{reason => Reason, stacktrace => Stacktrace}}]}
    end.

%% Helper functions for endpoint processing

-spec group_endpoints_by_path([map()]) -> #{string() => [map()]}.
group_endpoints_by_path(Endpoints) ->
    lists:foldl(fun(Endpoint, Acc) ->
                   Path = maps:get(path, Endpoint),
                   PathEndpoints = maps:get(Path, Acc, []),
                   maps:put(Path, [Endpoint | PathEndpoints], Acc)
                end,
                #{},
                Endpoints).

-spec generate_path_operations([map()]) -> map().
generate_path_operations(Endpoints) ->
    lists:foldl(fun(#{method := Method} = Endpoint, Acc) ->
                   Operation = generate_operation(Endpoint),
                   Acc#{Method => Operation}
                end,
                #{},
                Endpoints).

-spec generate_operation(map()) -> map().
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
                %% Convert integer keys to binary for OpenAPI spec
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

    %% Add request body if present
    OperationWithBody =
        case maps:get(request_body, Endpoint, undefined) of
            undefined ->
                OperationWithResponses;
            RequestBodyRef ->
                RequestBody = generate_request_body(RequestBodyRef),
                OperationWithResponses#{requestBody => RequestBody}
        end,

    %% Add parameters if present
    Parameters = maps:get(parameters, Endpoint, []),
    case Parameters of
        [] ->
            OperationWithBody;
        _ ->
            OpenAPIParameters = lists:map(fun generate_parameter/1, Parameters),
            OperationWithBody#{parameters => OpenAPIParameters}
    end.

-spec generate_response(map()) -> map().
generate_response(#{description := Description, schema := Schema}) ->

    SchemaContent =
        case Schema of
            {type, Name, Arity} ->
                %% ed_type_reference - use component reference
                SchemaName = type_ref_to_component_name({type, Name, Arity}),
                #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
            {record, Name} ->
                %% ed_type_reference - use component reference
                SchemaName = type_ref_to_component_name({record, Name}),
                #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
            DirectType ->
                %% Direct ed_type - generate inline schema
                {ok, InlineSchema} =
                    erldantic_json_schema:to_schema(
                        erldantic_abstract_code:types_in_module(?MODULE), DirectType),
                InlineSchema
        end,

    #{description => unicode:characters_to_binary(Description),
      content => #{<<"application/json">> => #{schema => SchemaContent}}}.

-spec generate_request_body(erldantic:ed_type_or_ref()) -> map().
generate_request_body(Schema) ->
    SchemaContent =
        case Schema of
            {type, Name, Arity} ->
                %% ed_type_reference - use component reference
                SchemaName = type_ref_to_component_name({type, Name, Arity}),
                #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
            {record, Name} ->
                %% ed_type_reference - use component reference
                SchemaName = type_ref_to_component_name({record, Name}),
                #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>};
            DirectType ->
                %% Direct ed_type - generate inline schema
                {ok, InlineSchema} =
                    erldantic_json_schema:to_schema(
                        erldantic_abstract_code:types_in_module(?MODULE), DirectType),
                InlineSchema
        end,

    #{required => true, content => #{<<"application/json">> => #{schema => SchemaContent}}}.

-spec generate_parameter(map()) -> map().
generate_parameter(#{name := Name, in := In, schema := Schema} = ParameterSpec) ->
    Required = maps:get(required, ParameterSpec, false),

    %% For parameters, always generate inline schemas (no component references)
    InlineSchema =
        case Schema of
            {type, TypeName, Arity} ->
                {ok, ValidSchema} =
                    erldantic_json_schema:to_schema(
                        erldantic_abstract_code:types_in_module(?MODULE), {type, TypeName, Arity}),
                ValidSchema;
            {record, Name} ->
                {ok, ValidSchema} =
                    erldantic_json_schema:to_schema(
                        erldantic_abstract_code:types_in_module(?MODULE), {record, Name}),
                ValidSchema;
            DirectType ->
                {ok, ValidSchema} =
                    erldantic_json_schema:to_schema(
                        erldantic_abstract_code:types_in_module(?MODULE), DirectType),
                ValidSchema
        end,

    #{name => unicode:characters_to_binary(Name),
      in => In,
      required => Required,
      schema => InlineSchema}.

-spec collect_schema_refs([map()]) -> [erldantic:ed_type_reference()].
collect_schema_refs(Endpoints) ->
    lists:foldl(fun(Endpoint, Acc) ->
                   EndpointRefs = collect_endpoint_schema_refs(Endpoint),
                   lists:usort(EndpointRefs ++ Acc)
                end,
                [],
                Endpoints).

-spec collect_endpoint_schema_refs(map()) -> [erldantic:ed_type_reference()].
collect_endpoint_schema_refs(#{responses := Responses, parameters := Parameters} = Endpoint) ->
    ResponseRefs = collect_response_refs(Responses),
    RequestBodyRefs =
        case maps:get(request_body, Endpoint, undefined) of
            undefined ->
                [];
            Schema ->
                case erldantic_type:is_type_reference(Schema) of
                    true ->
                        [Schema];
                    false ->
                        []
                end
        end,
    ParameterRefs = collect_parameter_refs(Parameters),

    ResponseRefs ++ RequestBodyRefs ++ ParameterRefs.

-spec collect_response_refs(map()) -> [erldantic:ed_type_reference()].
collect_response_refs(Responses) ->
    maps:fold(fun(_StatusCode, #{schema := Schema}, Acc) ->
                 case erldantic_type:is_type_reference(Schema) of
                     true ->
                         [Schema | Acc];
                     false ->
                         Acc
                 end
              end,
              [],
              Responses).

-spec collect_parameter_refs([map()]) -> [erldantic:ed_type_reference()].
collect_parameter_refs(Parameters) ->
    lists:filtermap(fun(#{schema := Schema}) ->
                       case erldantic_type:is_type_reference(Schema) of
                           true ->
                               {true, Schema};
                           false ->
                               false
                       end
                    end,
                    Parameters).

-spec generate_components([erldantic:ed_type_reference()]) ->
                             {ok, map()} | {error, [erldantic:error()]}.
generate_components(SchemaRefs) ->
    case erldantic_util:fold_until_error(fun(TypeRef, Acc) ->
                                            case erldantic_json_schema:to_schema(
                                                     erldantic_abstract_code:types_in_module(?MODULE),
                                                     TypeRef)
                                            of
                                                {ok, Schema} when is_map(Schema) ->
                                                    SchemaName =
                                                        type_ref_to_component_name(TypeRef),
                                                    {ok, Acc#{SchemaName => Schema}}
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
    %% Convert to PascalCase for OpenAPI convention and include arity
    TypeStr = atom_to_list(TypeName),
    Words = string:split(TypeStr, "_", all),
    PascalCase = lists:map(fun capitalize_word/1, Words),
    ArityStr = integer_to_list(Arity),
    list_to_binary(lists:flatten(PascalCase ++ [ArityStr]));
type_ref_to_component_name({record, RecordName}) ->
    %% Convert to PascalCase for OpenAPI convention
    TypeStr = atom_to_list(RecordName),
    Words = string:split(TypeStr, "_", all),
    PascalCase = lists:map(fun capitalize_word/1, Words),
    list_to_binary(lists:flatten(PascalCase)).

%% Helper function to capitalize the first letter of a word
capitalize_word([]) ->
    [];
capitalize_word([First | Rest]) ->
    [string:to_upper(First) | Rest].
