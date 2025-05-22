-module(record_type_introspect).

-export([parse_transform/2, from_json/3, to_json/3]).

parse_transform(Forms, _Options) ->
    erldantic_parse_transform:parse_transform(Forms, _Options).

from_json(TypeInfo, Type, Json) ->
    erldantic_json:from_json(TypeInfo, Type, Json).

to_json(TypeInfo, Type, Data) ->
    erldantic_json:to_json(TypeInfo, Type, Data).
