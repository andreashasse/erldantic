# OpenAPI Schema Generator Plan for Erldantic

## Overview
This document outlines the plan for implementing an OpenAPI 3.0 schema generator for the erldantic library, based on analysis of the existing codebase.

## Current Architecture Analysis

### Existing Components
- **Type Extraction**: `erldantic_abstract_code.erl` extracts Erlang types into internal `erldantic:ed_type()` format
- **JSON Support**: `erldantic_json.erl` provides complete JSON serialization/deserialization using the `ed_type()` format
- **Type System**: Rich internal type representation with 29 different type variants (see `erldantic_internal.hrl`)
- **Module Pattern**: Each format (JSON, etc.) follows the same architectural pattern

### Key Findings
1. **No Existing Schema Generation**: Currently only JSON serialization exists, no schema generation
2. **OpenAPI Already Planned**: Listed in `todo.md` under "TODO PoC FastApi" section
3. **Robust Type System**: The `ed_type()` internal format supports all necessary OpenAPI mappings
4. **Proven Architecture**: JSON module demonstrates the pattern for implementing new formats

## Proposed Architecture

### Core Module: `erldantic_openapi.erl`

#### Main API Functions
```erlang
%% Generate OpenAPI schema for a specific type (arity 0)
-spec type_to_schema(Module :: module(), TypeName :: atom()) -> 
    {ok, Schema :: map()} | {error, [erldantic:error()]}.

%% Generate OpenAPI schema for a record
-spec record_to_schema(Module :: module(), RecordName :: atom()) -> 
    {ok, Schema :: map()} | {error, [erldantic:error()]}.
```

#### Internal Architecture
- Follow same pattern as `erldantic_json.erl`
- Use `erldantic_module_types:get(Module)` for type information
- Recursive type processing with `do_to_schema/3` function
- Error handling with `erldantic:error()` records

### Type Mappings (`ed_type()` → OpenAPI Schema)

#### Simple Types
```erlang
#ed_simple_type{type = integer}     -> #{type => integer}
#ed_simple_type{type = string}      -> #{type => string}
#ed_simple_type{type = boolean}     -> #{type => boolean}
#ed_simple_type{type = number}      -> #{type => number}
#ed_simple_type{type = binary}      -> #{type => string, format => binary}
#ed_simple_type{type = atom}        -> #{type => string} % with enum if known values
#ed_simple_type{type = term}        -> #{} % any type
```

#### Complex Types
```erlang
#ed_union{types = Types}            -> #{oneOf => [schema(T) || T <- Types]}
#ed_list{type = T}                  -> #{type => array, items => schema(T)}
#ed_nonempty_list{type = T}         -> #{type => array, items => schema(T), minItems => 1}
#ed_range{lower_bound = Min, upper_bound = Max} -> #{type => integer, minimum => Min, maximum => Max}
#ed_literal{value = V}              -> #{enum => [V]}
#ed_map{fields = Fields}            -> #{type => object, properties => ...}
#ed_rec{name = Name, fields = Fields} -> #{type => object, properties => field_schemas(Fields)}
```

#### Advanced Features
```erlang
#ed_rec_ref{record_name = Name}     -> #{'$ref' => "#/components/schemas/" ++ Name}
#ed_remote_type{mfargs = {M,T,A}}   -> resolve and generate schema
#ed_user_type_ref{type_name = Name} -> #{'$ref' => "#/components/schemas/" ++ Name}
#ed_type_with_variables{}           -> resolve variables and generate schema
```

### Implementation Strategy

#### Phase 1: Core Schema Generation
- Implement basic type mappings
- Handle simple and complex types
- Generate valid OpenAPI 3.0 schemas
- Basic error handling

#### Phase 2: Advanced Features
- Schema references and components
- Union type optimization (oneOf vs anyOf)
- Validation constraints from range types
- Nested record handling

#### Phase 3: OpenAPI Features
- Example value generation
- Description support (from type comments)
- Schema composition (allOf, anyOf, oneOf)
- Custom format mappings

### Testing Strategy

#### Unit Tests (`test/openapi_schema_test.erl`)
```erlang
% Test each ed_type mapping
simple_type_test() -> test integer, string, boolean mappings
union_type_test() -> test oneOf generation
list_type_test() -> test array schema generation
record_type_test() -> test object schema generation
range_type_test() -> test min/max constraints
literal_type_test() -> test enum generation
```

#### Integration Tests
- Test with existing record definitions from test files
- Validate generated schemas against OpenAPI 3.0 spec
- Round-trip validation tests
- Error handling tests

#### Test Module Examples
Reuse existing test modules like:
- `record_test.erl` - for record schema generation
- `union_test.erl` - for oneOf schema generation  
- `list_*_test.erl` - for array schema generation
- `map_test.erl` - for object schema generation

### File Structure
```
src/
├── erldantic_openapi.erl          % Main OpenAPI module (schemas + endpoints)
test/
├── openapi_schema_test.erl        % Unit tests for schema generation
├── openapi_endpoint_test.erl      % Unit tests for endpoint specification
├── openapi_integration_test.erl   % Integration tests for full OpenAPI docs
```

### Dependencies
- **None**: Leverages existing erldantic infrastructure
- **Optional**: JSON schema validator for testing (external tool)

## OpenAPI Endpoint Specification

### Core Concept
An OpenAPI spec defines API endpoints with their request/response schemas. We need to support:

```erlang
% Endpoint specification
-type http_method() :: get | post | put | delete | patch | head | options.
-type path() :: string().  % e.g., "/users/{id}"
-type status_code() :: integer().  % e.g., 200, 404, 500

-type endpoint_spec() :: #{
    method := http_method(),
    path := path(),
    summary => string(),
    description => string(),
    request_body => schema_ref(),
    parameters => [parameter_spec()],
    responses := #{status_code() => response_spec()}
}.

-type response_spec() :: #{
    description := string(),
    schema => schema_ref(),
    headers => #{string() => schema_ref()}
}.

-type parameter_spec() :: #{
    name := string(),
    in := query | path | header | cookie,
    required => boolean(),
    schema := schema_ref()
}.

-type schema_ref() :: {module(), atom()} | {record, module(), atom()}.
```

### Extended API Functions

#### Endpoint Definition
```erlang
%% Generate OpenAPI spec for a list of endpoints
-spec endpoints_to_openapi(Endpoints :: [endpoint_spec()]) ->
    {ok, OpenAPISpec :: map()} | {error, [erldantic:error()]}.

%% Generate OpenAPI spec with automatic schema resolution
-spec endpoints_to_openapi(Endpoints :: [endpoint_spec()], Modules :: [module()]) ->
    {ok, OpenAPISpec :: map()} | {error, [erldantic:error()]}.

%% Helper for building endpoint specs
-spec endpoint(Method :: http_method(), Path :: path()) -> endpoint_spec().
-spec with_request_body(endpoint_spec(), schema_ref()) -> endpoint_spec().
-spec with_response(endpoint_spec(), status_code(), string(), schema_ref()) -> endpoint_spec().
-spec with_parameter(endpoint_spec(), parameter_spec()) -> endpoint_spec().
```

#### Usage Example
```erlang
% Define endpoints
UserEndpoints = [
    erldantic_openapi:endpoint(get, "/users")
        |> erldantic_openapi:with_response(200, "List of users", {user_module, user_list}),
    
    erldantic_openapi:endpoint(post, "/users")
        |> erldantic_openapi:with_request_body({user_module, create_user_request})
        |> erldantic_openapi:with_response(201, "User created", {user_module, user})
        |> erldantic_openapi:with_response(400, "Invalid input", {error_module, validation_error}),
    
    erldantic_openapi:endpoint(get, "/users/{id}")
        |> erldantic_openapi:with_parameter(#{
            name => "id",
            in => path,
            required => true,
            schema => {user_module, user_id}
        })
        |> erldantic_openapi:with_response(200, "User details", {user_module, user})
        |> erldantic_openapi:with_response(404, "User not found", {error_module, not_found_error})
].

% Generate OpenAPI spec
{ok, OpenAPISpec} = erldantic_openapi:endpoints_to_openapi(UserEndpoints, [user_module, error_module]).
```

### OpenAPI Document Structure

#### Complete OpenAPI 3.0 Document
```erlang
OpenAPISpec = #{
    openapi => "3.0.0",
    info => #{
        title => "API Title",
        version => "1.0.0",
        description => "API Description"
    },
    paths => #{
        "/users" => #{
            get => #{
                summary => "List users",
                responses => #{
                    200 => #{
                        description => "List of users",
                        content => #{
                            "application/json" => #{
                                schema => #{'$ref' => "#/components/schemas/UserList"}
                            }
                        }
                    }
                }
            },
            post => #{
                summary => "Create user",
                requestBody => #{
                    required => true,
                    content => #{
                        "application/json" => #{
                            schema => #{'$ref' => "#/components/schemas/CreateUserRequest"}
                        }
                    }
                },
                responses => #{
                    201 => #{
                        description => "User created",
                        content => #{
                            "application/json" => #{
                                schema => #{'$ref' => "#/components/schemas/User"}
                            }
                        }
                    }
                }
            }
        }
    },
    components => #{
        schemas => #{
            "User" => #{type => object, properties => #{...}},
            "UserList" => #{type => array, items => #{'$ref' => "#/components/schemas/User"}},
            "CreateUserRequest" => #{type => object, properties => #{...}}
        }
    }
}.
```

### Implementation Strategy

#### Phase 1: Schema Generation (Already Planned)
- Basic type to OpenAPI schema mapping
- Component schema generation

#### Phase 2: Endpoint Specification
- Endpoint specification types and API
- Path parameter extraction
- Request/Response schema linking

#### Phase 3: Complete OpenAPI Document
- Full OpenAPI 3.0 document generation
- Schema reference resolution
- Validation and error handling

#### Phase 4: Advanced Features
- Parameter validation from path templates
- Content-Type negotiation support  
- Security scheme integration
- Tag and operation grouping

### Future Extensions

## Benefits

1. **Consistent Architecture**: Follows proven pattern from JSON module
2. **Type Safety**: Maintains Erlang's type safety guarantees
3. **Reusable Components**: Generates proper OpenAPI component references
4. **Standard Compliant**: Produces valid OpenAPI 3.0 schemas
5. **Extensible**: Easy to add more OpenAPI features
6. **Performance**: Leverages existing optimized type extraction

## Implementation Timeline

- **Week 1**: Core module structure and basic type mappings
- **Week 2**: Complex types (unions, records, lists) and testing
- **Week 3**: Schema references, components, and validation
- **Week 4**: Documentation, examples, and integration testing

This plan provides a solid foundation for implementing OpenAPI schema generation that integrates seamlessly with the existing erldantic architecture.