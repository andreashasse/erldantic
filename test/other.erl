-module(other).

-export([account_from_json/1, account_to_json/1]).

-type account() :: #{id => string(), balance => integer()}.

-spec account_to_json(account()) -> json:encode_value().
account_to_json(Account) ->
    erldantic_json:to_json_no_pt({?MODULE, account, 0}, Account).

-spec account_from_json(json:decode_value()) -> account().
account_from_json(Json) ->
    erldantic_json:from_json_no_pt({?MODULE, account, 0}, Json).
