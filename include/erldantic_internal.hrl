-record(a_tuple, {fields :: [erldantic:a_type()]}).
-record(a_map,
        {fields ::
             [{map_field_assoc | map_field_exact, Name :: atom(), erldantic:a_type()} |
              {map_field_type_assoc | map_field_type_exact,
               erldantic:a_type(),
               erldantic:a_type()}]}).
%% FIXME: | [] does not work
-record(a_rec, {name :: atom(), fields :: [{atom(), erldantic:a_type()}]}).
-record(a_type, {type :: erldantic:a_type(), vars :: [atom()]}).
-record(a_function, {args :: any | [erldantic:a_type()], return :: erldantic:a_type()}).
-record(remote_type, {mfargs :: {module(), atom(), [erldantic:a_type()]}}).

-define(is_primary_type(PrimaryType),
        PrimaryType =:= string
        orelse PrimaryType =:= nonempty_string
        orelse PrimaryType =:= integer
        orelse PrimaryType =:= boolean
        orelse PrimaryType =:= atom
        orelse PrimaryType =:= float
        orelse PrimaryType =:= binary
        orelse PrimaryType =:= number
        orelse PrimaryType =:= term
        orelse PrimaryType =:= module).
-define(is_predefined_int_range(_Type),
        _Type =:= non_neg_integer orelse _Type =:= neg_integer orelse _Type =:= pos_integer).
