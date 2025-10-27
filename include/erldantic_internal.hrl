-record(ed_simple_type, {
    type ::
        string
        | nonempty_string
        | integer
        | non_neg_integer
        | neg_integer
        | pos_integer
        | float
        | number
        | boolean
        | binary
        | nonempty_binary
        | bitstring
        | nonempty_bitstring
        | atom
        | term
        | reference
        | pid
        | port
        | iolist
        | iodata
        | none
        | map
}).
-record(ed_tuple, {fields :: any | [erldantic:ed_type()]}).
-record(ed_map, {fields :: [erldantic:map_field()], struct_name :: undefined | atom()}).
-record(ed_rec, {
    name :: atom(), fields :: [{atom(), erldantic:ed_type()}], arity :: pos_integer()
}).
-record(ed_type_with_variables, {type :: erldantic:ed_type(), vars :: [atom()]}).
-record(ed_function, {args :: any | [erldantic:ed_type()], return :: erldantic:ed_type()}).
-record(ed_union, {types = [erldantic:ed_type()]}).
-record(ed_literal, {value :: term()}).
-record(ed_rec_ref, {
    record_name :: erldantic:user_type_name(), field_types :: [erldantic:record_field()]
}).
-record(ed_remote_type, {mfargs :: {module(), atom(), [erldantic:ed_type()]}}).
-record(ed_maybe_improper_list, {elements :: erldantic:ed_type(), tail :: erldantic:ed_type()}).
-record(ed_nonempty_improper_list, {elements :: erldantic:ed_type(), tail :: erldantic:ed_type()}).
-record(ed_user_type_ref, {
    type_name :: erldantic:user_type_name(), variables :: [erldantic:ed_type()]
}).
-record(ed_var, {name :: atom()}).
-record(ed_range, {type :: integer, lower_bound :: integer(), upper_bound :: integer()}).
-record(ed_list, {type :: erldantic:ed_type()}).
-record(ed_nonempty_list, {type :: erldantic:ed_type()}).
-record(ed_function_spec, {args :: [erldantic:ed_type()], return :: erldantic:ed_type()}).
%% New structured type information
-record(type_info, {
    types = #{} :: #{erldantic_type_info:type_key() => erldantic:ed_type()},
    records = #{} :: #{atom() => #ed_rec{}},
    functions = #{} :: #{erldantic_type_info:function_key() => [#ed_function_spec{}]}
}).
