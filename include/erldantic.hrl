-record(ed_error, {location, type, ctx}).

-type ed_error() ::
    #ed_error{location :: [string() | atom()],
              type :: type_mismatch | no_match | missing_data | not_matched_fields,
              ctx :: term()}.
