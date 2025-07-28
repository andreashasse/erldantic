-record(ed_simple_type, {type}).
-record(ed_tuple, {fields}).
-record(ed_map, {fields}).
%% FIXME: | [] does not work
-record(ed_rec, {name, fields, arity}).
-record(ed_type_with_variables, {type, vars}).
-record(ed_function, {args, return}).
-record(ed_union, {types = []}).
-record(ed_literal, {value}).
-record(ed_rec_ref, {record_name, field_types}).
-record(ed_remote_type, {mfargs}).
-record(ed_maybe_improper_list, {elements, tail}).
-record(ed_nonempty_improper_list, {elements, tail}).
-record(ed_user_type_ref, {type_name, variables}).
-record(ed_var, {name}).
-record(ed_range, {type, lower_bound, upper_bound}).
-record(ed_list, {type}).
-record(ed_nonempty_list, {type}).

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
