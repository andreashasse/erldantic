-module(erldantic).

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-type type_info() :: #{a_type_reference() => a_type()}.
-type var_type() :: {VarName :: atom(), a_type()}.
-type user_type_name() :: atom().
-type record_field() :: {FieldName :: atom(), a_type()}.
%% FIXME: Add doc here.
%% iolist and iodata are aliases, but are so complex, so it is easier to handle them as separate types
-type a_type() ::
    {type,
     string |
     nonempty_string |
     integer |
     non_neg_integer |
     neg_integer |
     pos_integer |
     float |
     number |
     boolean |
     binary |
     nonempty_binary |
     bitstring |
     nonempty_bitstring |
     atom |
     term |
     reference |
     pid |
     port |
     iolist |
     iodata |
     none} |
    #ed_rec_ref{} |
    {user_type_ref, user_type_name(), [a_type()]} |
    {var, atom()} |
    #a_map{} |
    #a_rec{} |
    #a_tuple{} |
    #type_with_arguments{} |
    #a_function{} |
    #ed_union{} |
    #ed_literal{} |
    {range, integer, integer(), integer()} |
    {list | nonempty_list, a_type()} |
    #maybe_improper_list{} |
    #nonempty_improper_list{} |
    #remote_type{}.
-type map_field() ::
    {map_field_assoc | map_field_exact, Name :: atom(), erldantic:a_type()} |
    {map_field_type_assoc | map_field_type_exact, erldantic:a_type(), erldantic:a_type()}.
-type a_type_reference() ::
    {record, atom()} |
    {record_ref, atom()} |
    {type, Name :: atom(), Arity :: non_neg_integer()}.
-type error() :: #ed_error{}.
-type a_type_or_ref() :: a_type() | a_type_reference().

-export_type([a_type/0, a_type_reference/0, a_type_or_ref/0, var_type/0, type_info/0,
              record_field/0, error/0, map_field/0]).
