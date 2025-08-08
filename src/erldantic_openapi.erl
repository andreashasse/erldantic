-module(erldantic_openapi).

-export([type_to_schema/2, record_to_schema/2, endpoint/2, with_response/4,
         with_request_body/2, with_parameter/2, endpoints_to_openapi/1]).

-ignore_xref([{erldantic_openapi, type_to_schema, 2},
              {erldantic_openapi, record_to_schema, 2},
              {erldantic_openapi, endpoint, 2},
              {erldantic_openapi, with_response, 4},
              {erldantic_openapi, with_request_body, 2},
              {erldantic_openapi, with_parameter, 2},
              {erldantic_openapi, endpoints_to_openapi, 1}]).

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

%% API

-doc("Converts an Erlang type definition to an OpenAPI 3.0 schema.\nThis function extracts the type definition from the specified module\nand generates a corresponding OpenAPI schema object.\nThe type must be of arity 0.\n\n### Returns\n{ok, Schema} if conversion succeeds, or {error, Errors} if the type is not found").
-doc(#{params =>
           #{"Module" => "The module containing the type definition",
             "TypeName" => "The name of the type to convert to OpenAPI schema"}}).

-spec type_to_schema(Module :: module(), TypeName :: atom()) ->
                        Schema :: {ok, map()} | {error, [erldantic:error()]}.
type_to_schema(Module, TypeName) when is_atom(Module) andalso is_atom(TypeName) ->
    TypeArity = 0,
    TypeRef = {type, TypeName, TypeArity},
    to_schema_no_pt(Module, TypeRef).

-doc("Converts an Erlang record definition to an OpenAPI 3.0 schema.\nThis function extracts the record definition from the specified module\nand generates a corresponding OpenAPI object schema.\n\n### Returns\n{ok, Schema} if conversion succeeds, or {error, Errors} if the record is not found").
-doc(#{params =>
           #{"Module" => "The module containing the record definition",
             "RecordName" => "The name of the record to convert to OpenAPI schema"}}).

-spec record_to_schema(Module :: module(), RecordName :: atom()) ->
                          {ok, Schema :: map()} | {error, [erldantic:error()]}.
record_to_schema(Module, RecordName) when is_atom(Module) andalso is_atom(RecordName) ->
    to_schema_no_pt(Module, {record, RecordName}).

%% INTERNAL

-spec to_schema_no_pt(Module :: module(), TypeRef :: erldantic:ed_type_reference()) ->
                         {ok, Schema :: map()} | {error, [erldantic:error()]}.
to_schema_no_pt(Module, TypeRef) ->
    try
        TypeInfo = erldantic_module_types:get(Module),
        to_schema(TypeInfo, TypeRef)
    catch
        error:Reason ->
            {error,
             [#ed_error{type = no_match,
                        location = [Module],
                        ctx = #{reason => Reason, error_type => module_extraction_failed}}]}
    end.

-spec to_schema(erldantic:type_info(), erldantic:ed_type_or_ref()) ->
                   {ok, Schema :: map()} | {error, [erldantic:error()]}.
to_schema(TypeInfo, Type) ->
    do_to_schema(TypeInfo, Type).

-spec do_to_schema(TypeInfo :: erldantic:type_info(),
                   Type :: erldantic:ed_type_or_ref()) ->
                      {ok, Schema :: map()} | {error, [erldantic:error()]}.
%% Simple types
do_to_schema(_TypeInfo, #ed_simple_type{type = integer}) ->
    {ok, #{type => integer}};
do_to_schema(_TypeInfo, #ed_simple_type{type = string}) ->
    {ok, #{type => string}};
do_to_schema(_TypeInfo, #ed_simple_type{type = boolean}) ->
    {ok, #{type => boolean}};
do_to_schema(_TypeInfo, #ed_simple_type{type = number}) ->
    {ok, #{type => number}};
do_to_schema(_TypeInfo, #ed_simple_type{type = float}) ->
    {ok, #{type => number, format => float}};
do_to_schema(_TypeInfo, #ed_simple_type{type = atom}) ->
    {ok, #{type => string}};
do_to_schema(_TypeInfo, #ed_simple_type{type = binary}) ->
    {ok, #{type => string, format => binary}};
do_to_schema(_TypeInfo, #ed_simple_type{type = nonempty_binary}) ->
    {ok,
     #{type => string,
       format => binary,
       minLength => 1}};
do_to_schema(_TypeInfo, #ed_simple_type{type = nonempty_string}) ->
    {ok, #{type => string, minLength => 1}};
do_to_schema(_TypeInfo, #ed_simple_type{type = pos_integer}) ->
    {ok, #{type => integer, minimum => 1}};
do_to_schema(_TypeInfo, #ed_simple_type{type = non_neg_integer}) ->
    {ok, #{type => integer, minimum => 0}};
do_to_schema(_TypeInfo, #ed_simple_type{type = neg_integer}) ->
    {ok, #{type => integer, maximum => -1}};
do_to_schema(_TypeInfo, #ed_simple_type{type = term}) ->
    {ok, #{}};  % any type
%% Range types
do_to_schema(_TypeInfo,
             #ed_range{type = integer,
                       lower_bound = Min,
                       upper_bound = Max}) ->
    {ok,
     #{type => integer,
       minimum => Min,
       maximum => Max}};
%% Literal types
do_to_schema(_TypeInfo, #ed_literal{value = undefined}) ->
    {ok, #{enum => [null]}};
do_to_schema(_TypeInfo, #ed_literal{value = Value}) ->
    {ok, #{enum => [Value]}};
%% List types
do_to_schema(TypeInfo, #ed_list{type = ItemType}) ->
    case do_to_schema(TypeInfo, ItemType) of
        {ok, ItemSchema} ->
            {ok, #{type => array, items => ItemSchema}};
        {error, _} = Err ->
            Err
    end;
do_to_schema(TypeInfo, #ed_nonempty_list{type = ItemType}) ->
    case do_to_schema(TypeInfo, ItemType) of
        {ok, ItemSchema} ->
            {ok,
             #{type => array,
               items => ItemSchema,
               minItems => 1}};
        {error, _} = Err ->
            Err
    end;
%% Union types
do_to_schema(TypeInfo, #ed_union{types = Types}) ->
    case lists:foldl(fun (T, {ok, Acc}) ->
                             case do_to_schema(TypeInfo, T) of
                                 {ok, Schema} ->
                                     {ok, [Schema | Acc]};
                                 {error, _} = Err ->
                                     Err
                             end;
                         (_, {error, _} = Err) ->
                             Err
                     end,
                     {ok, []},
                     Types)
    of
        {ok, Schemas} ->
            {ok, #{oneOf => lists:reverse(Schemas)}};
        {error, _} = Err ->
            Err
    end;
%% Map types
do_to_schema(TypeInfo, #ed_map{fields = Fields}) ->
    map_fields_to_schema(TypeInfo, Fields);
%% Record types
do_to_schema(TypeInfo, {record, RecordName}) when is_atom(RecordName) ->
    record_to_schema_internal(TypeInfo, RecordName);
do_to_schema(TypeInfo, #ed_rec{} = RecordInfo) ->
    record_to_schema_internal(TypeInfo, RecordInfo);
%% Record references
do_to_schema(TypeInfo, #ed_rec_ref{record_name = RecordName}) ->
    record_to_schema_internal(TypeInfo, RecordName);
%% Type references
do_to_schema(TypeInfo, {type, TypeName, TypeArity}) when is_atom(TypeName) ->
    case maps:get({type, TypeName, TypeArity}, TypeInfo, undefined) of
        undefined ->
            {error,
             [#ed_error{type = no_match,
                        location = [TypeName],
                        ctx = #{type => TypeName, arity => TypeArity}}]};
        Type ->
            do_to_schema(TypeInfo, Type)
    end;
%% User type references
do_to_schema(TypeInfo, #ed_user_type_ref{type_name = TypeName, variables = TypeArgs}) ->
    TypeArity = length(TypeArgs),
    case maps:get({type, TypeName, TypeArity}, TypeInfo, undefined) of
        undefined ->
            {error,
             [#ed_error{type = no_match,
                        location = [TypeName],
                        ctx = #{type => TypeName, arity => TypeArity}}]};
        Type ->
            do_to_schema(TypeInfo, Type)
    end;
%% Remote types
do_to_schema(_TypeInfo, #ed_remote_type{mfargs = {Module, TypeName, Args}}) ->
    RemoteTypeInfo = erldantic_module_types:get(Module),
    TypeArity = length(Args),
    case maps:get({type, TypeName, TypeArity}, RemoteTypeInfo, undefined) of
        undefined ->
            {error,
             [#ed_error{type = no_match,
                        location = [Module, TypeName],
                        ctx =
                            #{module => Module,
                              type => TypeName,
                              arity => TypeArity}}]};
        Type ->
            do_to_schema(RemoteTypeInfo, Type)
    end;
%% Unsupported types
do_to_schema(_TypeInfo, #ed_simple_type{type = NotSupported} = Type)
    when NotSupported =:= pid
         orelse NotSupported =:= port
         orelse NotSupported =:= reference
         orelse NotSupported =:= bitstring
         orelse NotSupported =:= nonempty_bitstring
         orelse NotSupported =:= none ->
    erlang:error({type_not_supported, Type});
do_to_schema(_TypeInfo, #ed_tuple{} = Type) ->
    erlang:error({type_not_supported, Type});
do_to_schema(_TypeInfo, #ed_function{} = Type) ->
    erlang:error({type_not_supported, Type});
%% Fallback
do_to_schema(_TypeInfo, Type) ->
    {error,
     [#ed_error{type = no_match,
                location = [],
                ctx = #{type => Type}}]}.

%% Helper functions

-spec map_fields_to_schema(erldantic:type_info(), [erldantic:map_field()]) ->
                              {ok, map()} | {error, [erldantic:error()]}.
map_fields_to_schema(TypeInfo, Fields) ->
    case process_map_fields(TypeInfo, Fields, #{}, [], false) of
        {ok, Properties, Required, HasAdditional} ->
            Schema =
                case {Properties, HasAdditional} of
                    {Props, false} when Props =/= #{} ->
                        BaseSchema = #{type => object, properties => Props},
                        case Required of
                            [] ->
                                BaseSchema;
                            _ ->
                                BaseSchema#{required => Required}
                        end;
                    {#{}, true} ->
                        #{type => object, additionalProperties => true};
                    _ ->
                        #{type => object}
                end,
            {ok, Schema};
        {error, _} = Err ->
            Err
    end.

-spec process_map_fields(erldantic:type_info(),
                         [erldantic:map_field()],
                         map(),
                         [atom()],
                         boolean()) ->
                            {ok, map(), [atom()], boolean()} | {error, [erldantic:error()]}.
process_map_fields(_TypeInfo, [], Properties, Required, HasAdditional) ->
    {ok, Properties, Required, HasAdditional};
process_map_fields(TypeInfo,
                   [{map_field_assoc, FieldName, FieldType} | Rest],
                   Properties,
                   Required,
                   HasAdditional) ->
    case do_to_schema(TypeInfo, FieldType) of
        {ok, FieldSchema} ->
            NewProperties = maps:put(FieldName, FieldSchema, Properties),
            process_map_fields(TypeInfo, Rest, NewProperties, Required, HasAdditional);
        {error, _} = Err ->
            Err
    end;
process_map_fields(TypeInfo,
                   [{map_field_exact, FieldName, FieldType} | Rest],
                   Properties,
                   Required,
                   HasAdditional) ->
    case do_to_schema(TypeInfo, FieldType) of
        {ok, FieldSchema} ->
            NewProperties = maps:put(FieldName, FieldSchema, Properties),
            NewRequired = [FieldName | Required],
            process_map_fields(TypeInfo, Rest, NewProperties, NewRequired, HasAdditional);
        {error, _} = Err ->
            Err
    end;
process_map_fields(TypeInfo,
                   [{map_field_type_assoc, _KeyType, ValueType} | Rest],
                   Properties,
                   Required,
                   _HasAdditional) ->
    case do_to_schema(TypeInfo, ValueType) of
        {ok, _ValueSchema} ->
            process_map_fields(TypeInfo, Rest, Properties, Required, true);
        {error, _} = Err ->
            Err
    end;
process_map_fields(TypeInfo, [_Field | Rest], Properties, Required, HasAdditional) ->
    process_map_fields(TypeInfo, Rest, Properties, Required, HasAdditional).

-spec record_to_schema_internal(erldantic:type_info(), atom() | #ed_rec{}) ->
                                   {ok, map()} | {error, [erldantic:error()]}.
record_to_schema_internal(TypeInfo, RecordName) when is_atom(RecordName) ->
    case maps:get({record, RecordName}, TypeInfo, undefined) of
        undefined ->
            {error,
             [#ed_error{type = no_match,
                        location = [RecordName],
                        ctx = #{record => RecordName}}]};
        #ed_rec{} = RecordInfo ->
            record_to_schema_internal(TypeInfo, RecordInfo);
        _Other ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [RecordName],
                        ctx = #{record => RecordName, error => invalid_record_type}}]}
    end;
record_to_schema_internal(TypeInfo, #ed_rec{fields = Fields}) ->
    case process_record_fields(TypeInfo, Fields, #{}, []) of
        {ok, Properties, Required} ->
            Schema = #{type => object, properties => Properties},
            FinalSchema =
                case Required of
                    [] ->
                        Schema;
                    _ ->
                        Schema#{required => Required}
                end,
            {ok, FinalSchema};
        {error, _} = Err ->
            Err
    end.

-spec process_record_fields(erldantic:type_info(),
                            [{atom(), erldantic:ed_type()}],
                            map(),
                            [atom()]) ->
                               {ok, map(), [atom()]} | {error, [erldantic:error()]}.
process_record_fields(_TypeInfo, [], Properties, Required) ->
    {ok, Properties, lists:reverse(Required)};
process_record_fields(TypeInfo, [{FieldName, FieldType} | Rest], Properties, Required) ->
    case do_to_schema(TypeInfo, FieldType) of
        {ok, FieldSchema} ->
            NewProperties = maps:put(FieldName, FieldSchema, Properties),
            NewRequired = [FieldName | Required],
            process_record_fields(TypeInfo, Rest, NewProperties, NewRequired);
        {error, _} = Err ->
            Err
    end.

%% ENDPOINT API

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

-doc("Adds a response specification to an endpoint.\nThis function adds a response with the specified status code, description, and schema reference.\nMultiple responses can be added to the same endpoint by calling this function multiple times.\n\n### Returns\nUpdated endpoint map with the new response added").
-doc(#{params =>
           #{"Description" => "Human-readable description of the response",
             "Endpoint" => "Endpoint map to add the response to",
             "SchemaRef" => "Schema reference tuple {Module, TypeName}",
             "StatusCode" => "HTTP status code (e.g., 200, 404, 500)"}}).

-spec with_response(Endpoint :: map(),
                    StatusCode :: integer(),
                    Description :: string(),
                    SchemaRef :: {module(), atom()}) ->
                       map().
with_response(Endpoint, StatusCode, Description, SchemaRef)
    when is_map(Endpoint)
         andalso is_integer(StatusCode)
         andalso is_list(Description)
         andalso is_tuple(SchemaRef) ->
    ResponseSpec = #{description => Description, schema => SchemaRef},
    Responses = maps:get(responses, Endpoint, #{}),
    UpdatedResponses = maps:put(StatusCode, ResponseSpec, Responses),
    maps:put(responses, UpdatedResponses, Endpoint).

-doc("Adds a request body specification to an endpoint.\nThis function sets the request body schema for the endpoint.\nTypically used with POST, PUT, and PATCH endpoints.\n\n### Returns\nUpdated endpoint map with request body set").
-doc(#{params =>
           #{"Endpoint" => "Endpoint map to add the request body to",
             "SchemaRef" => "Schema reference tuple {Module, TypeName}"}}).

-spec with_request_body(Endpoint :: map(), SchemaRef :: {module(), atom()}) -> map().
with_request_body(Endpoint, SchemaRef)
    when is_map(Endpoint) andalso is_tuple(SchemaRef) ->
    maps:put(request_body, SchemaRef, Endpoint).

-doc("Adds a parameter specification to an endpoint.\nThis function adds a parameter (path, query, header, or cookie) to the endpoint.\nMultiple parameters can be added by calling this function multiple times.\n\n### Parameter Specification\nThe parameter spec should be a map with these keys:\n- name: Parameter name (string)\n- in: Parameter location (path | query | header | cookie)\n- required: Whether the parameter is required (boolean)\n- schema: Schema reference tuple {Module, TypeName}\n\n### Returns\nUpdated endpoint map with the new parameter added").
-doc(#{params =>
           #{"Endpoint" => "Endpoint map to add the parameter to",
             "ParameterSpec" => "Parameter specification map"}}).

-spec with_parameter(Endpoint :: map(), ParameterSpec :: map()) -> map().
with_parameter(Endpoint, ParameterSpec)
    when is_map(Endpoint) andalso is_map(ParameterSpec) ->
    Parameters = maps:get(parameters, Endpoint, []),
    UpdatedParameters = [ParameterSpec | Parameters],
    maps:put(parameters, UpdatedParameters, Endpoint).

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
            maps:map(fun(_Path, PathEndpoints) -> generate_path_operations(PathEndpoints) end,
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
            #{openapi => "3.0.0",
              info => #{title => "API Documentation", version => "1.0.0"},
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
    lists:foldl(fun(Endpoint, Acc) ->
                   Method = maps:get(method, Endpoint),
                   Operation = generate_operation(Endpoint),
                   maps:put(Method, Operation, Acc)
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
                    maps:map(fun(StatusCode, ResponseSpec) ->
                                generate_response(StatusCode, ResponseSpec)
                             end,
                             Responses),
                %% Convert integer keys to binary for OpenAPI spec
                OpenAPIResponsesBinary =
                    maps:fold(fun(K, V, NewAcc) ->
                                 BinaryKey = integer_to_binary(K),
                                 maps:put(BinaryKey, V, NewAcc)
                              end,
                              #{},
                              OpenAPIResponses),
                maps:put(responses, OpenAPIResponsesBinary, Operation);
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
                maps:put(requestBody, RequestBody, OperationWithResponses)
        end,

    %% Add parameters if present
    Parameters = maps:get(parameters, Endpoint, []),
    case Parameters of
        [] ->
            OperationWithBody;
        _ ->
            OpenAPIParameters = lists:map(fun generate_parameter/1, Parameters),
            maps:put(parameters, OpenAPIParameters, OperationWithBody)
    end.

-spec generate_response(integer(), map()) -> map().
generate_response(_StatusCode, ResponseSpec) ->
    Description = maps:get(description, ResponseSpec),
    SchemaRef = maps:get(schema, ResponseSpec),
    SchemaName = schema_ref_to_name(SchemaRef),

    #{description => Description,
      content =>
          #{<<"application/json">> =>
                #{schema => #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>}}}}.

-spec generate_request_body({module(), atom()}) -> map().
generate_request_body(SchemaRef) ->
    SchemaName = schema_ref_to_name(SchemaRef),

    #{required => true,
      content =>
          #{<<"application/json">> =>
                #{schema => #{'$ref' => <<"#/components/schemas/", SchemaName/binary>>}}}}.

-spec generate_parameter(map()) -> map().
generate_parameter(ParameterSpec) ->
    Name = maps:get(name, ParameterSpec),
    In = maps:get(in, ParameterSpec),
    Required = maps:get(required, ParameterSpec, false),
    SchemaRef = maps:get(schema, ParameterSpec),

    %% For simple types, generate inline schema instead of reference
    Schema =
        case SchemaRef of
            {Module, TypeName} ->
                case type_to_schema(Module, TypeName) of
                    {error, _} ->
                        #{type => string};  % Fallback
                    {ok, ValidSchema} ->
                        ValidSchema
                end
        end,

    #{name => Name,
      in => In,
      required => Required,
      schema => Schema}.

-spec collect_schema_refs([map()]) -> [{module(), atom()}].
collect_schema_refs(Endpoints) ->
    lists:foldl(fun(Endpoint, Acc) ->
                   EndpointRefs = collect_endpoint_schema_refs(Endpoint),
                   lists:usort(EndpointRefs ++ Acc)
                end,
                [],
                Endpoints).

-spec collect_endpoint_schema_refs(map()) -> [{module(), atom()}].
collect_endpoint_schema_refs(Endpoint) ->
    ResponseRefs = collect_response_refs(maps:get(responses, Endpoint, #{})),
    RequestBodyRefs =
        case maps:get(request_body, Endpoint, undefined) of
            undefined ->
                [];
            Ref ->
                [Ref]
        end,
    ParameterRefs = collect_parameter_refs(maps:get(parameters, Endpoint, [])),

    ResponseRefs ++ RequestBodyRefs ++ ParameterRefs.

-spec collect_response_refs(map()) -> [{module(), atom()}].
collect_response_refs(Responses) ->
    maps:fold(fun(_StatusCode, ResponseSpec, Acc) ->
                 SchemaRef = maps:get(schema, ResponseSpec),
                 [SchemaRef | Acc]
              end,
              [],
              Responses).

-spec collect_parameter_refs([map()]) -> [{module(), atom()}].
collect_parameter_refs(Parameters) ->
    lists:filtermap(fun(ParameterSpec) ->
                       SchemaRef = maps:get(schema, ParameterSpec),
                       case SchemaRef of
                           {erlang, _} ->
                               false;  % Skip built-in types
                           {Module, TypeName} ->
                               {true, {Module, TypeName}}
                       end
                    end,
                    Parameters).

-spec generate_components([{module(), atom()}]) ->
                             {ok, map()} | {error, [erldantic:error()]}.
generate_components(SchemaRefs) ->
    case lists:foldl(fun ({Module, TypeName}, {ok, Acc}) ->
                             case type_to_schema(Module, TypeName) of
                                 {error, Errors} ->
                                     {error, Errors};  % Propagate errors
                                 {ok, Schema} when is_map(Schema) ->
                                     SchemaName = schema_ref_to_name({Module, TypeName}),
                                     {ok, maps:put(SchemaName, Schema, Acc)}
                             end;
                         (_, {error, _} = Error) ->
                             Error  % Propagate existing error
                     end,
                     {ok, #{}},
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

-spec schema_ref_to_name({module(), atom()}) -> binary().
schema_ref_to_name({_Module, TypeName}) ->
    %% Convert to PascalCase for OpenAPI convention
    %% e.g., create_user_request -> CreateUserRequest
    TypeStr = atom_to_list(TypeName),
    Words = string:split(TypeStr, "_", all),
    PascalCase = lists:map(fun capitalize_word/1, Words),
    list_to_binary(lists:flatten(PascalCase)).

%% Helper function to capitalize the first letter of a word
capitalize_word([]) ->
    [];
capitalize_word([First | Rest]) ->
    [string:to_upper(First) | Rest].
