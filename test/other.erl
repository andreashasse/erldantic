-module(other).

-export([account_from_json/1, account_to_json/1]).

-type account() :: #{id => string(), balance => integer()}.

-spec account_to_json(account()) -> json:encode_value().
account_to_json(Account) ->
    erldantic_json:type_to_json(?MODULE, account, 0, Account).

-spec account_from_json(json:decode_value()) -> account().
account_from_json(Json) ->
    erldantic_json:type_from_json(?MODULE, account, 0, Json).
