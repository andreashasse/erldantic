-record(im_simple_type,
        {type ::
             string |
             nonempty_string |
             integer |
             non_neg_integer |
             neg_integer |
             pos_integer |
             float |
             number |
             boolean |
             binary |
             nonempty_binary |
             bitstring |
             nonempty_bitstring |
             atom |
             term |
             reference |
             pid |
             port |
             iolist |
             iodata |
             none |
             map}).
-record(im_tuple, {fields :: any | [impala:im_type()]}).
-record(im_map, {fields :: [impala:map_field()], struct_name :: undefined | atom()}).
-record(im_rec,
        {name :: atom(), fields :: [{atom(), impala:im_type()}], arity :: pos_integer()}).
-record(im_type_with_variables, {type :: impala:im_type(), vars :: [atom()]}).
-record(im_function, {args :: any | [impala:im_type()], return :: impala:im_type()}).
-record(im_union, {types = [impala:im_type()]}).
-record(im_literal, {value :: term()}).
-record(im_rec_ref,
        {record_name :: impala:user_type_name(), field_types :: [impala:record_field()]}).
-record(im_remote_type, {mfargs :: {module(), atom(), [impala:im_type()]}}).
-record(im_maybe_improper_list, {elements :: impala:im_type(), tail :: impala:im_type()}).
-record(im_nonempty_improper_list,
        {elements :: impala:im_type(), tail :: impala:im_type()}).
-record(im_user_type_ref,
        {type_name :: impala:user_type_name(), variables :: [impala:im_type()]}).
-record(im_var, {name :: atom()}).
-record(im_range, {type :: integer, lower_bound :: integer(), upper_bound :: integer()}).
-record(im_list, {type :: impala:im_type()}).
-record(im_nonempty_list, {type :: impala:im_type()}).
-record(im_function_spec, {args :: [impala:im_type()], return :: impala:im_type()}).
%% New structured type information
-record(type_info,
        {types = #{} :: #{impala_type_info:type_key() => impala:im_type()},
         records = #{} :: #{atom() => #im_rec{}},
         functions = #{} :: #{impala_type_info:function_key() => [#im_function_spec{}]}}).
