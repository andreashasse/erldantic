-record(address, {street :: string(), city :: string() | undefined}).
-record(person, {name :: name_t(), age :: integer(), home :: #address{}}).

-type active() :: boolean().
-type temp() :: float().
-type age() :: non_neg_integer().
-type level() :: pos_integer().
-type negative() :: neg_integer().
-type score() :: #{value := 1..10, comment => #{lang := string(), text := string()}}.
-type name_t() :: #{first => string(), last := string()}.
-type address_t() :: #address{street :: string(), city :: string()}.
%% Add tests for below types.
-type tup_list() :: #{a => [integer()]}.
%% Binary type for testing binary handling
-type binary_data() :: binary().
-type binary_map() :: #{data := binary(), description => string()}.
%% Types for testing int list vs string confusion
-type string_type() :: string().
-type int_list_map() :: #{text := string(), numbers => [integer()]}.
