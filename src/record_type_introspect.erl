-module(record_type_introspect).

-include("../include/record_type_introspect.hrl").

-export([parse_transform/2, from_json/3, to_json/3]).

-type var_type() :: {VarName :: atom(), a_type()}.
-type user_type_name() :: atom().
-type a_type() ::
    {type,
     string |
     nonempty_string |
     integer |
     boolean |
     float |
     non_neg_integer |
     neg_integer |
     pos_integer |
     binary |
     atom |
     number |
     term} |
    {record_ref, user_type_name()} |
    {user_type_ref, user_type_name(), [a_type()]} |
    {var, atom()} |
    #a_map{} |
    #a_rec{} |
    #a_tuple{} |
    #a_type{} |
    {union, [a_type()]} |
    {literal, term()} |
    {range, integer, integer(), integer()} |
    {list | nonempty_list, a_type()} |
    {remote_type, {module(), atom(), [term()]}}.
-type a_type_reference() :: {record, atom()} | {record_ref, atom()} | {type, atom()}.
%% What is the empty list in non_negative integer?
-type a_type_or_ref() :: a_type() | a_type_reference().

-export_type([a_type/0, a_type_reference/0, a_type_or_ref/0, var_type/0]).

parse_transform(Forms, _Options) ->
    erldantic_parse_transform:parse_transform(Forms, _Options).

from_json(TypeInfo, Type, Json) ->
    erldantic_json:from_json(TypeInfo, Type, Json).

to_json(TypeInfo, Type, Data) ->
    erldantic_json:to_json(TypeInfo, Type, Data).
