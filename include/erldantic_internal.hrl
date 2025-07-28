-record(a_tuple, {fields}).
-record(a_map, {fields}).
%% FIXME: | [] does not work
-record(a_rec, {name, fields, arity}).
-record(type_with_arguments, {type, vars}).
-record(a_function, {args, return}).
-record(ed_union, {types = []}).
-record(ed_literal, {value}).
-record(ed_rec_ref, {record_name, field_types}).
-record(remote_type, {mfargs}).
-record(ed_maybe_improper_list, {elements, tail}).
-record(ed_nonempty_improper_list, {elements, tail}).
-record(ed_user_type_ref, {type_name, variables}).
-record(ed_var, {name}).
-record(ed_range, {type, lower_bound, upper_bound}).
-record(ed_list, {type}).
-record(ed_nonempty_list, {type}).

-type a_tuple() :: #a_tuple{fields :: any | [erldantic:a_type()]}.
-type a_map() :: #a_map{fields :: [erldantic:map_field()]}.
-type a_rec() ::
    #a_rec{name :: atom(),
           fields :: [{atom(), erldantic:a_type()}],
           arity :: pos_integer()}.
-type type_with_arguments() ::
    #type_with_arguments{type :: erldantic:a_type(), vars :: [atom()]}.
-type a_function() ::
    #a_function{args :: any | [erldantic:a_type()], return :: erldantic:a_type()}.
-type ed_union() :: #ed_union{types :: [erldantic:a_type()]}.
-type ed_literal() :: #ed_literal{value :: term()}.
-type record_ref() ::
    #ed_rec_ref{record_name :: erldantic:user_type_name(),
                field_types :: [erldantic:record_field()]}.
-type remote_type() :: #remote_type{mfargs :: {module(), atom(), [erldantic:a_type()]}}.
-type ed_maybe_improper_list() ::
    #ed_maybe_improper_list{elements :: erldantic:a_type(), tail :: erldantic:a_type()}.
-type ed_nonempty_improper_list() ::
    #ed_nonempty_improper_list{elements :: erldantic:a_type(), tail :: erldantic:a_type()}.
-type ed_user_type_ref() ::
    #ed_user_type_ref{type_name :: erldantic:user_type_name(),
                      variables :: [erldantic:a_type()]}.
-type ed_var() :: #ed_var{name :: atom()}.
-type ed_range() ::
    #ed_range{type :: integer,
              lower_bound :: integer(),
              upper_bound :: integer()}.
-type ed_list() :: #ed_list{type :: erldantic:a_type()}.
-type ed_nonempty_list() :: #ed_nonempty_list{type :: erldantic:a_type()}.

-define(is_primary_type(PrimaryType),
        PrimaryType =:= string
        orelse PrimaryType =:= nonempty_string
        orelse PrimaryType =:= integer
        orelse PrimaryType =:= boolean
        orelse PrimaryType =:= atom
        orelse PrimaryType =:= float
        orelse PrimaryType =:= binary
        orelse PrimaryType =:= nonempty_binary
        orelse PrimaryType =:= number
        orelse PrimaryType =:= term).
-define(is_predefined_int_range(_Type),
        _Type =:= non_neg_integer orelse _Type =:= neg_integer orelse _Type =:= pos_integer).
