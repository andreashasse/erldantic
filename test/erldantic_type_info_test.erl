-module(erldantic_type_info_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic_internal.hrl").

new_test() ->
    TypeInfo = erldantic_type_info:new(),
    ?assertMatch(#type_info{}, TypeInfo).

add_and_get_type_test() ->
    TypeInfo0 = erldantic_type_info:new(),
    Type = #ed_simple_type{type = integer},
    TypeInfo1 = erldantic_type_info:add_type(TypeInfo0, my_type, 0, Type),

    ?assertEqual({ok, Type}, erldantic_type_info:get_type(TypeInfo1, my_type, 0)),
    ?assertEqual(error, erldantic_type_info:get_type(TypeInfo1, non_existent, 0)).

add_and_get_record_test() ->
    TypeInfo0 = erldantic_type_info:new(),
    Record =
        #ed_rec{
            name = user,
            fields = [{id, #ed_simple_type{type = integer}}],
            arity = 2
        },
    TypeInfo1 = erldantic_type_info:add_record(TypeInfo0, user, Record),

    ?assertEqual({ok, Record}, erldantic_type_info:get_record(TypeInfo1, user)),
    ?assertEqual(error, erldantic_type_info:get_record(TypeInfo1, non_existent)).

add_and_get_function_test() ->
    TypeInfo0 = erldantic_type_info:new(),
    FuncSpec =
        #ed_function_spec{
            args = [#ed_simple_type{type = integer}],
            return = #ed_simple_type{type = boolean}
        },
    TypeInfo1 = erldantic_type_info:add_function(TypeInfo0, test_func, 1, FuncSpec),

    ?assertEqual({ok, FuncSpec}, erldantic_type_info:get_function(TypeInfo1, test_func, 1)),
    ?assertEqual(error, erldantic_type_info:get_function(TypeInfo1, non_existent, 1)).
