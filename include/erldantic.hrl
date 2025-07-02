-record(ed_error,
        {location :: [string() | atom()],
         type ::
             type_mismatch |
             no_match |
             missing_data |
             missing_type |
             type_not_supported |
             module_types_not_found |
             not_matched_fields,
         ctx :: term()}).
