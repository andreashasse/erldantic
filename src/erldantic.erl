-module(erldantic).

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-type type_info() :: #{a_type_reference() => a_type()}.
-type var_type() :: {VarName :: atom(), a_type()}.
-type user_type_name() :: atom().
-type record_field() :: {FieldName :: atom(), a_type()}.
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
     atom |
     term |
     reference |
     pid |
     port |
     iolist |
     iodata } |
    {record_ref, user_type_name(), [record_field()]} |
    {user_type_ref, user_type_name(), [a_type()]} |
    {var, atom()} |
    #a_map{} |
    #a_rec{} |
    #a_tuple{} |
    #a_type{} |
    #a_function{} |
    {union, [a_type()]} |
    {literal, term()} |
    {range, integer, integer(), integer()} |
    {list | nonempty_list, a_type()} |
    {maybe_improper_list, a_type(), a_type()} |
    #remote_type{}.

     %% Below here need better representation

-type a_type_reference() ::
    {record, atom()} |
    {record_ref, atom()} |
    {type, Name :: atom(), Arity :: non_neg_integer()}.
-type error() :: #ed_error{}.
%% RENAME: Type is used for everything.
-type a_type_or_ref() :: a_type() | a_type_reference().

-export_type([a_type/0, a_type_reference/0, a_type_or_ref/0, var_type/0, type_info/0,
              record_field/0, error/0]).
