-module(record_type_introspect).

-include("../include/record_type_introspect.hrl").

-export([parse_transform/2, from_json/3, to_json/3]).

-type user_type_name() :: atom().
-type a_type() ::
    {type, string | integer | boolean | float | non_neg_integer} |
    {record_ref, user_type_name()} |
    {user_type_ref, user_type_name()} |
    #a_map{} |
    #a_rec{} |
    #a_tuple{} |
    {union, [a_type()]} |
    {literal, term()} |
    {range, integer, integer(), integer()} |
    {list, a_type()}.
-type a_type_reference() :: {record, atom()} | {record_ref, atom()} | {type, atom()}.
%% What is the empty list in non_negative integer?
-type a_type_or_ref() :: a_type() | a_type_reference().

-export_type([a_type/0, a_type_reference/0, a_type_or_ref/0]).

parse_transform(Forms, _Options) ->
    erldantic_parse_transform:parse_transform(Forms, _Options).

from_json(TypeInfo, Type, Json) ->
    erldantic_json:from_json(TypeInfo, Type, Json).

to_json(TypeInfo, Type, Data) ->
    erldantic_json:to_json(TypeInfo, Type, Data).
