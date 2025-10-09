-module(impala_type_info_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/impala_internal.hrl").

new_test() ->
    TypeInfo = impala_type_info:new(),
    ?assertMatch(#type_info{}, TypeInfo).

add_and_get_type_test() ->
    TypeInfo0 = impala_type_info:new(),
    Type = #im_simple_type{type = integer},
    TypeInfo1 = impala_type_info:add_type(TypeInfo0, my_type, 0, Type),

    ?assertEqual({ok, Type}, impala_type_info:get_type(TypeInfo1, my_type, 0)),
    ?assertEqual(error, impala_type_info:get_type(TypeInfo1, non_existent, 0)).

add_and_get_record_test() ->
    TypeInfo0 = impala_type_info:new(),
    Record =
        #im_rec{name = user,
                fields = [{id, #im_simple_type{type = integer}}],
                arity = 2},
    TypeInfo1 = impala_type_info:add_record(TypeInfo0, user, Record),

    ?assertEqual({ok, Record}, impala_type_info:get_record(TypeInfo1, user)),
    ?assertEqual(error, impala_type_info:get_record(TypeInfo1, non_existent)).

add_and_get_function_test() ->
    TypeInfo0 = impala_type_info:new(),
    FuncSpec =
        #im_function_spec{args = [#im_simple_type{type = integer}],
                          return = #im_simple_type{type = boolean}},
    TypeInfo1 = impala_type_info:add_function(TypeInfo0, test_func, 1, FuncSpec),

    ?assertEqual({ok, FuncSpec}, impala_type_info:get_function(TypeInfo1, test_func, 1)),
    ?assertEqual(error, impala_type_info:get_function(TypeInfo1, non_existent, 1)).
