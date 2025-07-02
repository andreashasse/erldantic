-module(example_test).

-include_lib("eunit/include/eunit.hrl").

-ifdef(OTP_RELEASE).

-if(?OTP_RELEASE >= 27).

%% Using json.erl in this module.

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

example_json_roundtrip_test() ->
    Contacts =
        [#email_contact{address = "john.doe@example.com",
                        verified = #{source => one_time_code, "code" => "123456"},
                        domain = "example.com"},
         #phone_contact{number = "+1-555-123-4567",
                        verified = #{source => gut_feeling, "confidence" => "high"},
                        sms_capable = true},
         #email_contact{address = "alice@company.org", domain = "company.org"}],

    Json = contacts_to_json(Contacts),
    %% io:format("JSON Output: ~p~n", [Json]),
    ?assertEqual({ok, Contacts}, json_to_contacts(Json)).

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

-endif.
-endif.
