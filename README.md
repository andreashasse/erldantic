# erldantic

This is Pydantic, but for Erlang. Hopefully for Elixir and Gleam in the future!

Erldantic provides type-safe JSON serialization and deserialization for Records and all Erlang types that can be converted to json, converting between Erlang's type system and data expected by the json.erl module.
It provides detailed errors when data and types doesn't match.

## Usage

Given these types:

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

and some helper functions:

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


One can do this:

``` erlang

Contacts =
    [#email_contact{address = "john.doe@example.com",
                    verified = #{source => one_time_code, "code" => "123456"},
                    domain = "example.com"},
     #phone_contact{number = "+1-555-123-4567",
                    verified = #{source => gut_feeling, "confidence" => "high"},
                    sms_capable = true},
     #email_contact{address = "alice@company.org", domain = "company.org"}],
Json = contacts_to_json(Contacts).
io:format("~p~n", [Json]).

> <<"[{\"domain\":\"example.com\",\"address\":\"john.doe@example.com\",\"verified\":{\"source\":\"one_time_code\",\"code\":\"123456\"}},{\"number\":\"+1-555-123-4567\",\"verified\":{\"source\":\"gut_feeling\",\"confidence\":\"high\"},\"sms_capable\":true},{\"domain\":\"company.org\",\"address\":\"alice@company.org\"}]">>


{ok, Contacts} = json_to_contacts(Json).

```
