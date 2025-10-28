-module(spectra_type_info_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra_internal.hrl").

new_test() ->
    TypeInfo = spectra_type_info:new(),
    ?assertMatch(#type_info{}, TypeInfo).

add_and_get_type_test() ->
    TypeInfo0 = spectra_type_info:new(),
    Type = #sp_simple_type{type = integer},
    TypeInfo1 = spectra_type_info:add_type(TypeInfo0, my_type, 0, Type),

    ?assertEqual({ok, Type}, spectra_type_info:get_type(TypeInfo1, my_type, 0)),
    ?assertEqual(error, spectra_type_info:get_type(TypeInfo1, non_existent, 0)).

add_and_get_record_test() ->
    TypeInfo0 = spectra_type_info:new(),
    Record =
        #sp_rec{
            name = user,
            fields = [{id, #sp_simple_type{type = integer}}],
            arity = 2
        },
    TypeInfo1 = spectra_type_info:add_record(TypeInfo0, user, Record),

    ?assertEqual({ok, Record}, spectra_type_info:get_record(TypeInfo1, user)),
    ?assertEqual(error, spectra_type_info:get_record(TypeInfo1, non_existent)).

add_and_get_function_test() ->
    TypeInfo0 = spectra_type_info:new(),
    FuncSpec =
        #sp_function_spec{
            args = [#sp_simple_type{type = integer}],
            return = #sp_simple_type{type = boolean}
        },
    TypeInfo1 = spectra_type_info:add_function(TypeInfo0, test_func, 1, FuncSpec),

    ?assertEqual({ok, FuncSpec}, spectra_type_info:get_function(TypeInfo1, test_func, 1)),
    ?assertEqual(error, spectra_type_info:get_function(TypeInfo1, non_existent, 1)).
