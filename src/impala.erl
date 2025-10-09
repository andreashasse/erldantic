-module(impala).

-include("../include/impala.hrl").
-include("../include/impala_internal.hrl").

-type type_info() :: impala_type_info:type_info().
-type var_type() :: {VarName :: atom(), im_type()}.
-type user_type_name() :: atom().
-type record_field() :: {FieldName :: atom(), im_type()}.
%% FIXME: Add doc here.
%% iolist and iodata are aliases, but are so complex, so it is easier to handle them as separate types
-type im_type() ::
    #im_simple_type{} |
    #im_rec_ref{} |
    #im_user_type_ref{} |
    #im_var{} |
    #im_map{} |
    #im_rec{} |
    #im_tuple{} |
    #im_type_with_variables{} |
    #im_function{} |
    #im_union{} |
    #im_literal{} |
    #im_range{} |
    #im_list{} |
    #im_nonempty_list{} |
    #im_maybe_improper_list{} |
    #im_nonempty_improper_list{} |
    #im_remote_type{}.
-type map_field() ::
    {map_field_assoc | map_field_exact, Name :: atom(), im_type()} |
    {map_field_type_assoc | map_field_type_exact, im_type(), im_type()}.
-type im_type_reference() ::
    {type, Name :: atom(), Arity :: arity()} | {record, Name :: atom()}.
-type error() :: #im_error{}.
-type im_type_or_ref() :: im_type() | im_type_reference().
-type im_function_spec() :: #im_function_spec{}.

%% Internal type definitions moved from impala_internal.hrl

-export_type([im_type/0, im_type_reference/0, im_type_or_ref/0, var_type/0, type_info/0,
              record_field/0, error/0, map_field/0, user_type_name/0, im_function_spec/0]).
