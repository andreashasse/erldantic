-record(a_tuple, {fields :: [record_type_introspect:a_type()]}).
-record(a_map,
        {fields ::
             [{map_field_assoc | map_field_exact,
               Name :: atom(),
               record_type_introspect:a_type()}]}).
-record(a_rec, {name :: atom(), fields :: [{atom(), record_type_introspect:a_type()}]}).

-define(is_primary_type(PrimaryType),
        PrimaryType =:= string
        orelse PrimaryType =:= integer
        orelse PrimaryType =:= boolean
        orelse PrimaryType =:= atom
        orelse PrimaryType =:= float
        orelse PrimaryType =:= binary).
-define(is_predefined_int_range(_Type),
        _Type =:= non_neg_integer orelse _Type =:= neg_integer orelse _Type =:= pos_integer).

-record(ed_error,
        {location :: [string() | atom()],
         type ::
             type_mismatch |
             no_match |
             record_type_mismatch |
             missing_data |
             missing_type |
             json_type_mismatch |
             module_types_not_found,
         ctx :: term()}).
