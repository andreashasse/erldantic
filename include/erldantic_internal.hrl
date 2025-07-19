-record(a_tuple, {fields :: any | [erldantic:a_type()]}).
-record(a_map, {fields :: [erldantic:map_field()]}).
%% FIXME: | [] does not work
-record(a_rec,
        {name :: atom(), fields :: [{atom(), erldantic:a_type()}], arity :: pos_integer()}).
-record(type_with_arguments, {type :: erldantic:a_type(), vars :: [atom()]}).
-record(a_function, {args :: any | [erldantic:a_type()], return :: erldantic:a_type()}).
-record(remote_type, {mfargs :: {module(), atom(), [erldantic:a_type()]}}).
-record(maybe_improper_list,
        {elements :: erldantic:a_type(), tail :: erldantic:a_type()}).
-record(nonempty_improper_list,
        {elements :: erldantic:a_type(), tail :: erldantic:a_type()}).

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
