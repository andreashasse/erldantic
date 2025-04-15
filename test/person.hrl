-record(address, {street :: string(), city :: string() | undefined}).
-record(person, {name :: name_t(), age :: integer(), home :: #address{}}).

-type score() :: #{value := 1..10, comment => #{lang := string(), text := string()}}.
-type name_t() :: #{first => string(), last := string()}.
-type address_t() :: #address{street :: string(), city :: string()}.
-type weird_union() :: #address{} | #{city => string(), score => score()}.
%% Add tests for below types.
-type accesses() :: [read | write].
-type tup_list() :: #{a => [integer()]}.
