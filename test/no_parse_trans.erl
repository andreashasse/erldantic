-module(no_parse_trans).

-export([name_to_json/1]).
-export([name_from_json/1]).

-type name() :: #{first => string(), last := string()}.

-spec name_to_json(name()) -> json:encode_value().
name_to_json(Name) ->
    erldantic_json:type_to_json(no_parse_trans, name, Name).

-spec name_from_json(json:encode_value()) -> name().
name_from_json(Json) ->
    erldantic_json:type_from_json(no_parse_trans, name, Json).
