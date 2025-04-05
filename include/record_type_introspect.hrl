-record(a_tuple, {fields :: [record_type_introspect:a_type()]}).
-record(a_map,
        {fields :: [{map_field_assoc | map_field_exact, Name :: atom(), record_type_introspect:a_type()}]}).
-record(a_rec, {name :: atom(), fields :: [{atom(), record_type_introspect:a_type()}]}).
