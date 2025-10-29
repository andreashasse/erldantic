-module(spectra_module_types_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra_internal.hrl").

get_module_types_without_cache_test() ->
    application:set_env(spectra, use_module_types_cache, false),
    TypeInfo = spectra_module_types:get(other),
    ?assert(is_record(TypeInfo, type_info)),
    ?assertMatch({ok, _}, spectra_type_info:get_type(TypeInfo, account, 0)).

get_module_types_with_cache_test() ->
    application:set_env(spectra, use_module_types_cache, true),
    spectra_module_types:clear(other),
    TypeInfo1 = spectra_module_types:get(other),
    ?assert(is_record(TypeInfo1, type_info)),
    TypeInfo2 = spectra_module_types:get(other),
    ?assertEqual(TypeInfo1, TypeInfo2),
    application:set_env(spectra, use_module_types_cache, false),
    spectra_module_types:clear(other).

cache_clear_test() ->
    application:set_env(spectra, use_module_types_cache, true),
    _TypeInfo1 = spectra_module_types:get(other),
    ok = spectra_module_types:clear(other),
    TypeInfo2 = spectra_module_types:get(other),
    ?assert(is_record(TypeInfo2, type_info)),
    application:set_env(spectra, use_module_types_cache, false),
    spectra_module_types:clear(other).

get_nonexistent_module_test() ->
    application:set_env(spectra, use_module_types_cache, false),
    ?assertError(
        {module_types_not_found, nonexistent_module_xyz, non_existing},
        spectra_module_types:get(nonexistent_module_xyz)
    ).

get_nonexistent_module_with_cache_test() ->
    application:set_env(spectra, use_module_types_cache, true),
    ?assertError(
        {module_types_not_found, nonexistent_module_abc, non_existing},
        spectra_module_types:get(nonexistent_module_abc)
    ),
    application:set_env(spectra, use_module_types_cache, false).

cache_consistency_test() ->
    application:set_env(spectra, use_module_types_cache, true),
    spectra_module_types:clear(other),
    TypeInfo1 = spectra_module_types:get(other),
    TypeInfo2 = spectra_module_types:get(other),
    TypeInfo3 = spectra_module_types:get(other),
    ?assertEqual(TypeInfo1, TypeInfo2),
    ?assertEqual(TypeInfo2, TypeInfo3),
    {ok, _AccountType} = spectra_type_info:get_type(TypeInfo3, account, 0),
    application:set_env(spectra, use_module_types_cache, false),
    spectra_module_types:clear(other).
