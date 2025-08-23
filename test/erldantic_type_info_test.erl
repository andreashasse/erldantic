-module(erldantic_type_info_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic_internal.hrl").

new_test() ->
    TypeInfo = erldantic_type_info:new(),
    ?assertEqual([], erldantic_type_info:list_types(TypeInfo)),
    ?assertEqual([], erldantic_type_info:list_records(TypeInfo)),
    ?assertEqual([], erldantic_type_info:list_functions(TypeInfo)).

add_and_get_type_test() ->
    TypeInfo0 = erldantic_type_info:new(),
    Type = #ed_simple_type{type = integer},
    TypeInfo1 = erldantic_type_info:add_type(TypeInfo0, my_type, 0, Type),

    ?assertEqual({ok, Type}, erldantic_type_info:get_type(TypeInfo1, my_type, 0)),
    ?assertEqual(error, erldantic_type_info:get_type(TypeInfo1, non_existent, 0)),
    ?assertEqual(true, erldantic_type_info:has_type(TypeInfo1, my_type, 0)),
    ?assertEqual(false, erldantic_type_info:has_type(TypeInfo1, non_existent, 0)),
    ?assertEqual([{my_type, 0}], erldantic_type_info:list_types(TypeInfo1)).

add_and_get_record_test() ->
    TypeInfo0 = erldantic_type_info:new(),
    Record =
        #ed_rec{name = user,
                fields = [{id, #ed_simple_type{type = integer}}],
                arity = 2},
    TypeInfo1 = erldantic_type_info:add_record(TypeInfo0, user, Record),

    ?assertEqual({ok, Record}, erldantic_type_info:get_record(TypeInfo1, user)),
    ?assertEqual(error, erldantic_type_info:get_record(TypeInfo1, non_existent)),
    ?assertEqual(true, erldantic_type_info:has_record(TypeInfo1, user)),
    ?assertEqual(false, erldantic_type_info:has_record(TypeInfo1, non_existent)),
    ?assertEqual([user], erldantic_type_info:list_records(TypeInfo1)).

add_and_get_function_test() ->
    TypeInfo0 = erldantic_type_info:new(),
    FuncSpec =
        #ed_function_spec{args = [#ed_simple_type{type = integer}],
                          return = #ed_simple_type{type = boolean}},
    TypeInfo1 = erldantic_type_info:add_function(TypeInfo0, test_func, 1, FuncSpec),

    ?assertEqual({ok, FuncSpec}, erldantic_type_info:get_function(TypeInfo1, test_func, 1)),
    ?assertEqual(error, erldantic_type_info:get_function(TypeInfo1, non_existent, 1)),
    ?assertEqual(true, erldantic_type_info:has_function(TypeInfo1, test_func, 1)),
    ?assertEqual(false, erldantic_type_info:has_function(TypeInfo1, non_existent, 1)),
    ?assertEqual([{test_func, 1}], erldantic_type_info:list_functions(TypeInfo1)).

merge_test() ->
    TypeInfo1 = erldantic_type_info:new(),
    TypeInfo1a =
        erldantic_type_info:add_type(TypeInfo1, type1, 0, #ed_simple_type{type = integer}),
    TypeInfo1b =
        erldantic_type_info:add_record(TypeInfo1a,
                                       rec1,
                                       #ed_rec{name = rec1,
                                               fields = [],
                                               arity = 1}),

    TypeInfo2 = erldantic_type_info:new(),
    TypeInfo2a =
        erldantic_type_info:add_type(TypeInfo2, type2, 0, #ed_simple_type{type = atom}),
    TypeInfo2b =
        erldantic_type_info:add_function(TypeInfo2a,
                                         func1,
                                         1,
                                         #ed_function_spec{args = [],
                                                           return = #ed_simple_type{type = term}}),

    Merged = erldantic_type_info:merge(TypeInfo1b, TypeInfo2b),

    ?assertEqual(2, length(erldantic_type_info:list_types(Merged))),
    ?assertEqual(1, length(erldantic_type_info:list_records(Merged))),
    ?assertEqual(1, length(erldantic_type_info:list_functions(Merged))),

    ?assertEqual({ok, #ed_simple_type{type = integer}},
                 erldantic_type_info:get_type(Merged, type1, 0)),
    ?assertEqual({ok, #ed_simple_type{type = atom}},
                 erldantic_type_info:get_type(Merged, type2, 0)).

legacy_conversion_test() ->
    %% Create a legacy map
    LegacyMap =
        #{{type, my_type, 0} => #ed_simple_type{type = integer},
          {record, my_record} =>
              #ed_rec{name = my_record,
                      fields = [],
                      arity = 1},
          {function, my_func, 1} =>
              #ed_function_spec{args = [], return = #ed_simple_type{type = term}}},

    %% Convert to new format
    TypeInfo = erldantic_type_info:from_legacy_map(LegacyMap),

    %% Verify conversion
    ?assertEqual({ok, #ed_simple_type{type = integer}},
                 erldantic_type_info:get_type(TypeInfo, my_type, 0)),
    ?assertEqual({ok,
                  #ed_rec{name = my_record,
                          fields = [],
                          arity = 1}},
                 erldantic_type_info:get_record(TypeInfo, my_record)),
    ?assertEqual({ok, #ed_function_spec{args = [], return = #ed_simple_type{type = term}}},
                 erldantic_type_info:get_function(TypeInfo, my_func, 1)),

    %% Convert back to legacy
    BackToLegacy = erldantic_type_info:to_legacy_map(TypeInfo),
    ?assertEqual(LegacyMap, BackToLegacy).
