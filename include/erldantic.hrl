-record(ed_error,
        {location :: [string() | atom()],
         type :: decode_error | type_mismatch | no_match | missing_data | not_matched_fields,
         ctx :: term()}).
