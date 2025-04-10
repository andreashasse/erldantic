-record(address, {street :: string(), city :: string() | undefined}).
-record(person, {name :: name_t(), age :: integer(), home :: #address{}}).

-type name_t() :: #{first => string(), last := string()}.
% | {A :: string(), string()}.
-type address_t() :: #address{street :: string(), city :: string()}.
