-module(record_type_introspect).

-include("../include/record_type_introspect.hrl").

-export([parse_transform/2, from_json/3]).

-type user_type_name() :: atom().
-type a_type() ::
    {type, string | integer | boolean} |
    {record_ref, user_type_name()} |
    {user_type_ref, user_type_name()} |
    #a_map{} |
    #a_rec{} |
    #a_tuple{} |
    {union, [a_type()]} |
    {literal, term()}.

-export_type([a_type/0]).

parse_transform(Forms, _Options) ->
    erldantic_parse_transform:parse_transform(Forms, _Options).

from_json(TypeInfo, Type, Json) ->
    erldantic_json:from_json(TypeInfo, Type, Json).
