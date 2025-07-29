-module(erldantic).

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-type type_info() :: #{ed_type_reference() => ed_type()}.
-type var_type() :: {VarName :: atom(), ed_type()}.
-type user_type_name() :: atom().
-type record_field() :: {FieldName :: atom(), ed_type()}.
%% FIXME: Add doc here.
%% iolist and iodata are aliases, but are so complex, so it is easier to handle them as separate types
-type ed_type() ::
    #ed_simple_type{} |
    #ed_rec_ref{} |
    #ed_user_type_ref{} |
    #ed_var{} |
    #ed_map{} |
    #ed_rec{} |
    #ed_tuple{} |
    #ed_type_with_variables{} |
    #ed_function{} |
    #ed_union{} |
    #ed_literal{} |
    #ed_range{} |
    #ed_list{} |
    #ed_nonempty_list{} |
    #ed_maybe_improper_list{} |
    #ed_nonempty_improper_list{} |
    #ed_remote_type{}.
-type map_field() ::
    {map_field_assoc | map_field_exact, Name :: atom(), ed_type()} |
    {map_field_type_assoc | map_field_type_exact, ed_type(), ed_type()}.
-type ed_type_reference() ::
    {record, atom()} | {type, Name :: atom(), Arity :: non_neg_integer()}.
-type error() ::
    #ed_error{location :: [string() | atom()],
              type :: type_mismatch | no_match | missing_data | not_matched_fields,
              ctx :: term()}.
-type ed_type_or_ref() :: ed_type() | ed_type_reference().

%% Internal type definitions moved from erldantic_internal.hrl

-export_type([ed_type/0, ed_type_reference/0, ed_type_or_ref/0, var_type/0, type_info/0,
              record_field/0, error/0, map_field/0, user_type_name/0]).
