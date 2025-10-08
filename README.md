# Erldantic

A data validation library for Erlang inspired by Pydantic. Erldantic provides type-safe JSON serialization and deserialization for Erlang records and all Erlang types that can be converted to JSON.

- **Type-safe JSON conversion**: Convert typed erlang values to/from JSON, making sure the data conforms to the type.
- **Detailed errors**: Get error messages with location information when validation fails
- **Support for complex scenarios**: Handles unions, records, atoms, nested structures, ...
- **Works in tandem with json.erl**: Relies on json.erl to do the encoding / decoding.

Hopefully easy to add support for Elixir and Gleam in the future.

## Installation

Add erldantic to your rebar.config dependencies:

```erlang
{deps, [
    {erldantic, ".*", {git, "https://github.com/andreashasse/erldantic.git", {branch, "main"}}}
]}.
```

## Quick Start

### Basic Usage

Here's how to use erldantic for JSON serialization and deserialization:
(The same code is available in `test/example_test.erl`)

#### 1. Define your types:

```erlang
-record(email_contact, {address, verified, domain}).
-record(phone_contact, {number, verified, sms_capable}).

-type verified() ::
    #{source := one_time_code | gut_feeling, string() => string()} | undefined.
-type email_contact() ::
    #email_contact{address :: string(),
                   verified :: verified(),
                   domain :: string()}.
-type phone_contact() ::
    #phone_contact{number :: string(),
                   verified :: verified(),
                   sms_capable :: boolean()}.
-type contacts() :: [email_contact() | phone_contact()].
```

#### 2. Optionally, create helper functions:

```erlang
-spec json_to_contacts(binary()) -> {ok, contacts()} | {error, [erldantic:error()]}.
json_to_contacts(Json) ->
    Decoded = json:decode(Json),
    erldantic_json:from_json(?MODULE, {type, contacts, 0}, Decoded).

-spec contacts_to_json(contacts()) -> binary() | {error, [erldantic:error()]}.
contacts_to_json(Contacts) ->
    maybe
        {ok, Encodeable} ?= erldantic_json:to_json(?MODULE, {type, contacts, 0}, Contacts),
        iolist_to_binary(json:encode(Encodeable))
    end.
```

#### 3. Use the functions:

```erlang
%% Create some data
Contacts = [
    #email_contact{
        address = "john.doe@example.com",
        verified = #{source => one_time_code, "code" => "123456"},
        domain = "example.com"
    },
    #phone_contact{
        number = "+1-555-123-4567",
        verified = #{source => gut_feeling, "confidence" => "high"},
        sms_capable = true
    },
    #email_contact{
        address = "alice@company.org",
        domain = "company.org"
    }
],

%% Convert to JSON
Json = contacts_to_json(Contacts),
%% Results in:
%% <<"[{\"domain\":\"example.com\",\"address\":\"john.doe@example.com\",\"verified\":{\"source\":\"one_time_code\",\"code\":\"123456\"}},{\"number\":\"+1-555-123-4567\",\"verified\":{\"source\":\"gut_feeling\",\"confidence\":\"high\"},\"sms_capable\":true},{\"domain\":\"company.org\",\"address\":\"alice@company.org\"}]">>

%% Convert back from JSON
{ok, Contacts} = json_to_contacts(Json).
```

### Error Handling

Erldantic provides detailed error messages when data doesn't match your type specifications:

```erlang
BadSourceJson = <<"[{\"number\":\"+1-555-123-4567\",\"verified\":{\"source\":\"a_bad_source\",\"confidence\":\"high\"},\"sms_capable\":true}]">>.

{error, [#ed_error{...}]} = json_to_contacts(BadSourceJson).
```

## JSON Serialization API

These are the main functions for JSON serialization and deserialization:

```erlang
erldantic_json:to_json(Module, TypeOrReference, Value) -> {ok, json:encode_value()} | {error, [erldantic:error()]}.
erldantic_json:from_json(Module, TypeOrReference, Json) -> {ok, ... your type ...} | {error, [erldantic:error()]}.
```

Where:
- `Module` is the module where the type/record is defined
- `TypeOrReference` can be:
  - `{type, TypeName, Arity}` for user-defined types (e.g., `{type, my_type, 0}`)
  - `{record, RecordName}` for records (e.g., `{record, user}`)
  - An actual `ed_type()` structure (for advanced usage)

The type cannot have any parameters (arity must be 0 for user-defined types).

## JSON Schema Generation

Erldantic can generate [JSON Schema](https://json-schema.org/) specifications from your Erlang types. This is useful for API documentation, client code generation, validation in other languages, and integration with schema-based tools.

### JSON Schema API

```erlang
erldantic_json_schema:to_schema(Module, TypeOrReference) -> {ok, Schema :: map()} | {error, [erldantic:error()]}.
```

### Basic Example

```erlang
-module(my_api).

-export([generate_user_schema/0]).

-record(user, {id :: integer(), name :: string() | undefined, email :: string()}).

generate_user_schema() ->
    {ok, Schema} = erldantic_json_schema:to_schema(?MODULE, {record, user}),
    %% Schema will be:
    %% #{type => <<"object">>,
    %%   properties => #{
    %%     id => #{type => <<"integer">>},
    %%     name => #{type => <<"string">>},
    %%     email => #{type => <<"string">>}
    %%   },
    %%   required => [id, name, email]}
    Schema.

```

### Optional Fields

When a type is a union with `undefined`, the schema omits the `undefined` and marks the field as not required:

```erlang
-record(user_profile, {
    id :: integer(),
    name :: string(),
    bio :: string() | undefined  %% Optional field
}).

%% Generated schema will have:
%% required => [id, name]  %% bio is NOT required
```

## OpenAPI Specification Generation

Erldantic can generate complete [OpenAPI 3.0](https://spec.openapis.org/oas/v3.0.0) specifications for your REST APIs. This provides interactive documentation, client generation, and API testing tools.

### OpenAPI Builder API

Build endpoints using a fluent builder pattern:

```erlang
%% Create a base endpoint
erldantic_openapi:endpoint(Method, Path) -> endpoint_spec().

%% Add responses
erldantic_openapi:with_response(Endpoint, StatusCode, Description, Module, Schema) -> endpoint_spec().

%% Add request body
erldantic_openapi:with_request_body(Endpoint, Module, Schema) -> endpoint_spec().

%% Add parameters (path, query, header, cookie)
erldantic_openapi:with_parameter(Endpoint, Module, ParameterSpec) -> endpoint_spec().

%% Generate complete OpenAPI spec
erldantic_openapi:endpoints_to_openapi(Metadata, Endpoints) -> {ok, json:encode_value()} | {error, [erldantic:error()]}.
```

### Complete Example

```erlang
-module(my_api).

-record(user, {id :: integer(), name :: string(), email :: string()}).
-record(create_user_request, {name :: string(), email :: string()}).
-record(error_response, {message :: string(), code :: integer()}).

-type user() :: #user{}.
-type create_user_request() :: #create_user_request{}.
-type error_response() :: #error_response{}.
-type user_id() :: integer().

generate_openapi_spec() ->
    %% Define list users endpoint
    ListUsers1 = erldantic_openapi:endpoint(get, <<"/users">>),
    ListUsers = erldantic_openapi:with_response(ListUsers1,
                                                 200,
                                                 <<"List of all users">>,
                                                 ?MODULE,
                                                 #ed_list{type = {record, user}}),

    %% Define create user endpoint
    CreateUser1 = erldantic_openapi:endpoint(post, <<"/users">>),
    CreateUser2 = erldantic_openapi:with_request_body(CreateUser1,
                                                       ?MODULE,
                                                       {record, create_user_request}),
    CreateUser3 = erldantic_openapi:with_response(CreateUser2,
                                                   201,
                                                   <<"User created successfully">>,
                                                   ?MODULE,
                                                   {record, user}),
    CreateUser = erldantic_openapi:with_response(CreateUser3,
                                                  400,
                                                  <<"Invalid input">>,
                                                  ?MODULE,
                                                  {record, error_response}),

    %% Define get user endpoint
    GetUser1 = erldantic_openapi:endpoint(get, <<"/users/{id}">>),
    GetUser2 = erldantic_openapi:with_parameter(GetUser1,
                                                 ?MODULE,
                                                 #{name => <<"id">>,
                                                   in => path,
                                                   required => true,
                                                   schema => {type, user_id, 0}}),
    GetUser3 = erldantic_openapi:with_response(GetUser2,
                                                200,
                                                <<"User details">>,
                                                ?MODULE,
                                                {record, user}),
    GetUser = erldantic_openapi:with_response(GetUser3,
                                               404,
                                               <<"User not found">>,
                                               ?MODULE,
                                               {record, error_response}),

    %% Generate complete OpenAPI spec
    {ok, OpenAPISpec} = erldantic_openapi:endpoints_to_openapi(
        #{title => <<"My API">>, version => <<"1.0.0">>},
        [ListUsers, CreateUser, GetUser]
    ),

    %% OpenAPISpec can now be serialized to JSON and served
    json:encode(OpenAPISpec).
```

### Component Schemas

The `endpoints_to_openapi/2` function automatically:
- Collects all type references used in endpoints
- Generates JSON schemas for each referenced type
- Creates a `components.schemas` section with reusable schemas
- Uses `$ref` to reference schemas in the spec

### Schema References

You can use either:
- **Type references**: `{type, TypeName, Arity}` or `{record, RecordName}` - These create reusable component schemas
- **Direct types**: `#ed_simple_type{}`, `#ed_list{}`, etc. - These create inline schemas

```erlang
%% Using type reference (creates reusable component)
erldantic_openapi:with_response(Endpoint, 200, <<"Success">>, ?MODULE, {record, user})

%% Using direct type (inline schema)
erldantic_openapi:with_response(Endpoint, 200, <<"Success">>, ?MODULE, #ed_simple_type{type = string})
```

## Special Handling

### `undefined` Values

In records and mandatory map fields (with the `:=` operator), the value `undefined` will be used when the value is missing if the type includes `undefined`.

For example, `integer() | undefined` will become `undefined` in records and maps mandatory fields if the value is missing, and the value will not be present in the JSON.

### `term()` | `any()`

When using types with `term`, `erldantic_json` will not reject any data, which means it can return data that `json.erl` cannot convert to JSON.

### Unsupported Types

Some Erlang types are not supported for JSON conversion:
- `maybe_improper_list()` - Currently returns an error
- `pid()`, `port()`, `reference()` - Cannot be serialized to JSON
- `tuple()`, `bitstring()`, `nonempty_bitstring()` - Not JSON-compatible
- Function types - Cannot be serialized

## Configuration

### Application Environment Variables

You can configure erldantic behavior using application environment variables:

#### `use_module_types_cache`
- **Type**: `boolean()`
- **Default**: `false`
- **Description**: When set to `true`, enables caching of extracted type information for modules using persistent terms. This can improve performance when repeatedly processing the same modules.
- **Note**: When only changing types and not code, the module vsn (used for caching) is not updated, so the types will not be updated.
- **Recommendation**: Enable this in production systems where no hot code reloading is done.

Example configuration in `sys.config`:

```erlang
{erldantic, [
    {use_module_types_cache, true}
]}.
```

## Error Types

`#error{}` contains:

- `location` - List showing the path to where the error occurred
- `type` - Error type: `type_mismatch`, `no_match`, `missing_data`, `missing_type`, `type_not_supported`, `not_matched_fields`, `not_implemented`
- `ctx` - Context information about the error

## Development Status

This library is under active development. APIs and error messages will probably change.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.
