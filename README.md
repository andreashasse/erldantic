# Spectra

A data validation library for Erlang inspired by Pydantic. Point it to your erlang types (records and type specs) and it will validate and convert JSON data to/from your types, generate json schemas and help you generate openapi schemas.

## Installation

Add spectra to your rebar.config dependencies:

```erlang
{deps, [
    {spectra, "~> 0.1.5"}
]}.
```

## Data (de)serialization

Spectra provides type-safe data serialization and deserialization for Erlang records and all Erlang types that can be converted to that type. Currently the focus is on JSON.

- **Type-safe conversion**: Convert typed erlang values to/from external formats such as JSON, making sure the data conforms to the type.
- **Detailed errors**: Get error messages with location information when validation fails
- **Support for complex scenarios**: Handles unions, records, atoms, nested structures, ...

### Basic Usage

Here's how to use spectra for JSON serialization and deserialization:


```erlang
-module(demo).

-export([json_to_contacts/1, contacts_to_json/1, json_schema/0, binary_to_quality/1]).

-record(email_contact, {address, verified, domain}).
-record(phone_contact, {number, verified, sms_capable}).

-type quality() :: 1..5.
-type verified() ::
    #{source := one_time_code | gut_feeling,
      quality => quality(),
      binary() => binary()} | undefined.
-type email_contact() ::
    #email_contact{address :: nonempty_binary(),
                   verified :: verified(),
                   domain :: nonempty_binary()}.
-type phone_contact() ::
    #phone_contact{number :: binary(),
                   verified :: verified(),
                   sms_capable :: boolean()}.
-type contacts() :: [email_contact() | phone_contact()].

%% Some helper functions

-spec json_to_contacts(binary()) -> {ok, contacts()} | {error, [spectra:error()]}.
json_to_contacts(Json) ->
    spectra:decode(json, ?MODULE, contacts, Json).

-spec contacts_to_json(contacts()) -> {ok, binary()} | {error, [spectra:error()]}.
contacts_to_json(Contacts) ->
    case spectra:encode(json, ?MODULE, contacts, Contacts) of
        {ok, JsonIoList} -> {ok, iolist_to_binary(JsonIoList)};
        {error, _} = Error -> Error
    end.

-spec binary_to_quality(binary()) -> {ok, quality()} | {error, [spectra:error()]}.
binary_to_quality(Bin) ->
    spectra:decode(binary_string, ?MODULE, quality, Bin).

json_schema() ->
    {ok, IoSchema} = spectra:schema(json_schema, ?MODULE, contacts),
    iolist_to_binary(IoSchema).

```

### Using the demo module in the shell


```erlang
%% Compile the demo module (note: You need debug info)
c("demo.erl", [debug_info]).

%% Load the record defs into the shell.
rr(demo).

%% Create some data
Contacts = [
    #email_contact{
        address = <<"john.doe@example.com">>,
        verified = #{source => one_time_code, quality => 2, <<"code">> => <<"123456">>},
        domain = <<"example.com">>
    },
    #phone_contact{
        number = <<"+1-555-123-4567">>,
        verified = #{source => gut_feeling, <<"confidence">> => <<"high">>},
        sms_capable = true
    },
    #email_contact{
        address = <<"alice@company.org">>,
        domain = <<"company.org">>
    }
].

%% Convert to JSON
{ok, Json} = demo:contacts_to_json(Contacts).

%% Convert back from JSON
demo:json_to_contacts(Json).

%% If you get quality as a query parameter, you can do:
demo:binary_to_quality(<<"4">>).

%% Generate the json schema
demo:json_schema().

```

### Data Serialization API

These are the main functions for JSON serialization and deserialization:

```erlang
spectra:encode(Format, Module, Type, Value) ->
    {ok, iolist()} | {error, [spectra:error()]}.
spectra:decode(Format, Module, Type, JsonBinary) ->
    {ok, Value} | {error, [spectra:error()]}.

```

Where:
- `Format` is json, binary_string or string
- `Module` is the module where the type/record is defined (or a `type_info()` for advanced usage)
- `Type` is either:
  - an atom: spectra will look for a type of arity 0 or a record with that name
  - `{type, TypeName, Arity}` for user-defined types (e.g., `{type, my_type, 0}`)
  - `{record, RecordName}` for records (e.g., `{record, user}`)
  - An actual `sp_type()` structure (for advanced usage)


### Schema API

```erlang
spectra:schema(Format, Module, Type) ->
    {ok, Schema :: map()} | {error, [spectra:error()]}.
```

Where:
- `Format` is `json_schema` (for now)

And the rest of the arguments are the same as for the data serialization API.

## OpenAPI Spec

Spectra can generate complete [OpenAPI 3.0](https://spec.openapis.org/oas/v3.0.0) specifications for your REST APIs. This provides interactive documentation, client generation, and API testing tools.

### OpenAPI Builder API

The API for building endpoints is very experimental and will probably change a lot.
It is meant to be used by developers of web servers / web frameworks.
See [elli_openapi](https://github.com/andreashasse/elli_openapi) for an example of how to use it in a web server.

```erlang
%% Create a base endpoint
spectra_openapi:endpoint(Method, Path) ->
    endpoint_spec().

%% Add responses
spectra_openapi:with_response(Endpoint, StatusCode, Description, Module, Schema) ->
    endpoint_spec().

%% Add request body
spectra_openapi:with_request_body(Endpoint, Module, Schema) ->
    endpoint_spec().

%% Add parameters (path, query, header, cookie)
spectra_openapi:with_parameter(Endpoint, Module, ParameterSpec) ->
    endpoint_spec().

%% Generate complete OpenAPI spec
spectra_openapi:endpoints_to_openapi(Metadata, Endpoints) ->
    {ok, json:encode_value()} | {error, [spectra:error()]}.
```


## Requirements

* Modules must be compiled with `debug_info` for spectra to extract type information.


## Error Handling

Spectra uses two different error handling strategies depending on the type of error:

### Returned Errors (`{error, [spectra:error()]}`)

Data validation errors are returned as `{error, [#sp_error{}]}` tuples. These occur when input data doesn't match the expected type during encoding/decoding.

Example:
```erlang
BadSourceJson = <<"[{\"number\":\"+1-555-123-4567\",\"verified\":{\"source\":\"a_bad_source\",\"confidence\":\"high\"},\"sms_capable\":true}]">>.

{error, [#sp_error{...}]} = json_to_contacts(BadSourceJson).
```

`#error{}` contains:

- `location` - List showing the path to where the error occurred
- `type` - Error type: `type_mismatch`, `no_match`, `missing_data`, `missing_type`, `type_not_supported`, `not_matched_fields`, `not_implemented`
- `ctx` - Context information about the error

### Raised Exceptions

Configuration and structural errors raise exceptions. These occur when:
- Module not found, not loaded, or not compiled with `debug_info`
- Type or record not found in module (e.g., `{type_or_record_not_found, TypeName}`)
- Unsupported type used (e.g., `pid()`, `port()`, `tuple()`)

These errors indicate a problem with your application's configuration or type definitions, not with the data being processed.

## Special Handling

### `undefined` Values

In records and mandatory map fields (with the `:=` operator), the value `undefined` will be used when the value is missing if the type includes `undefined`.

For example, `integer() | undefined` will become `undefined` in records and maps mandatory fields if the value is missing, and the value will not be present in the JSON.

### `term()` | `any()`

When using types with `term`, `spectra_json` will not reject any data, which means it can return data that `json.erl` cannot convert to JSON.


### Char

Char is currently handled as integer, which is probably not what you want. Try to not use the char type for now. This is documented in `test/char_test.erl`.

### Unsupported Types

Each format supports a subset of Erlang types. For JSON serialization and schema, the following are not supported:
- `maybe_improper_list()` - Currently returns an error
- `pid()`, `port()`, `reference()` - Cannot be serialized to JSON
- `tuple()`, `bitstring()`, `nonempty_bitstring()` - Not JSON-compatible
- Function types - Cannot be serialized

It would be interesting to add support for key value lists, but as it isn't a native type in erlang, I haven't gotten around to it yet.

## Configuration

### Application Environment Variables

You can configure spectra behavior using application environment variables:

#### `use_module_types_cache`
- **Type**: `boolean()`
- **Default**: `false`
- **Description**: When set to `true`, enables caching of extracted type information for modules using persistent terms. This can improve performance when repeatedly processing the same modules.
- **Note**: When only changing types and not code, the module vsn (used for caching) is not updated, so the types will not be updated.
- **Recommendation**: Enable this in production systems where no hot code reloading is done.

#### `check_unicode`
- **Type**: `boolean()`
- **Default**: `false`
- **Description**: When set to `true`, enables additional Unicode validation for string data. This validates that list-type string data contains valid Unicode characters. When disabled, string conversion still works correctly but skips the additional validation step for better performance.
- **Note**: Required type conversions (e.g., binary to list, list to binary) always use Unicode functions regardless of this setting.
- **Recommendation**: Enable this if you need strict Unicode validation, or keep disabled for better performance when Unicode validity is guaranteed by other means.

Example configuration in `sys.config`:

```erlang
{spectra, [
    {use_module_types_cache, true},
    {check_unicode, false}
]}.
```

## Related Projects

- **[elli_openapi](https://github.com/andreashasse/elli_openapi)** - Elli middleware for automatic OpenAPI spec generation and validation using spectra
- **[exdantic](https://github.com/andreashasse/exdantic)** - Elixir port of spectra for data validation and JSON serialization

## Development Status

This library is under active development. APIs and error messages will probably change.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.
