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
    erldantic_json:type_from_json(?MODULE, contacts, Decoded).

-spec contacts_to_json(contacts()) -> binary() | {error, [erldantic:error()]}.
contacts_to_json(Contacts) ->
    maybe
        {ok, Encodeable} ?= erldantic_json:type_to_json(?MODULE, contacts, Contacts),
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

## Error Types

`#error{}` contains:

- `location` - List showing the path to where the error occurred
- `type` - Error type: `type_mismatch`, `no_match`, `missing_data`, `missing_type`, `type_not_supported`, `not_matched_fields`, `not_implemented`
- `ctx` - Context information about the error

## Development Status

This library is under active development. APIs and error messages will probably change.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.
