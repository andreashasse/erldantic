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
    ed_simple_type() |
    ed_record_ref() |
    ed_user_type_ref() |
    ed_var() |
    ed_map() |
    ed_rec() |
    ed_tuple() |
    ed_type_with_variables() |
    ed_function() |
    ed_union() |
    ed_literal() |
    ed_range() |
    ed_list() |
    ed_nonempty_list() |
    ed_maybe_improper_list() |
    ed_nonempty_improper_list() |
    ed_remote_type().
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
-type ed_simple_type() ::
    #ed_simple_type{type ::
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
                        none}.
-type ed_tuple() :: #ed_tuple{fields :: any | [ed_type()]}.
-type ed_map() :: #ed_map{fields :: [map_field()]}.
-type ed_rec() ::
    #ed_rec{name :: atom(),
            fields :: [{atom(), ed_type()}],
            arity :: pos_integer()}.
-type ed_type_with_variables() ::
    #ed_type_with_variables{type :: ed_type(), vars :: [atom()]}.
-type ed_function() :: #ed_function{args :: any | [ed_type()], return :: ed_type()}.
-type ed_union() :: #ed_union{types :: [ed_type()]}.
-type ed_literal() :: #ed_literal{value :: term()}.
-type ed_record_ref() ::
    #ed_rec_ref{record_name :: user_type_name(), field_types :: [record_field()]}.
-type ed_remote_type() :: #ed_remote_type{mfargs :: {module(), atom(), [ed_type()]}}.
-type ed_maybe_improper_list() ::
    #ed_maybe_improper_list{elements :: ed_type(), tail :: ed_type()}.
-type ed_nonempty_improper_list() ::
    #ed_nonempty_improper_list{elements :: ed_type(), tail :: ed_type()}.
-type ed_user_type_ref() ::
    #ed_user_type_ref{type_name :: user_type_name(), variables :: [ed_type()]}.
-type ed_var() :: #ed_var{name :: atom()}.
-type ed_range() ::
    #ed_range{type :: integer,
              lower_bound :: integer(),
              upper_bound :: integer()}.
-type ed_list() :: #ed_list{type :: ed_type()}.
-type ed_nonempty_list() :: #ed_nonempty_list{type :: ed_type()}.

-export_type([ed_type/0, ed_type_reference/0, ed_type_or_ref/0, var_type/0, type_info/0,
              record_field/0, error/0, map_field/0, ed_simple_type/0, ed_tuple/0, ed_map/0,
              ed_rec/0, ed_type_with_variables/0, ed_function/0, ed_union/0, ed_literal/0,
              ed_record_ref/0, ed_remote_type/0, ed_maybe_improper_list/0,
              ed_nonempty_improper_list/0, ed_user_type_ref/0, ed_var/0, ed_range/0, ed_list/0,
              ed_nonempty_list/0]).
