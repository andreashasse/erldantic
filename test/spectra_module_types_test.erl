-module(spectra_module_types_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra_internal.hrl").

%% Test module for spectra_module_types caching behavior

%% Test that module types are retrieved correctly without cache
get_module_types_without_cache_test() ->
    % Ensure cache is disabled
    application:set_env(spectra, use_module_types_cache, false),

    % Get types for a known module (other module)
    TypeInfo = spectra_module_types:get(other),

    % Verify it's a valid type_info record
    ?assert(is_record(TypeInfo, type_info)),

    % Verify it contains the account type
    ?assertMatch(
        {ok, _},
        spectra_type_info:get_type(TypeInfo, account, 0)
    ).

%% Test that module types are retrieved correctly with cache enabled
get_module_types_with_cache_test() ->
    % Enable cache
    application:set_env(spectra, use_module_types_cache, true),

    % Clear any existing cache for the module
    spectra_module_types:clear(other),

    % First call should populate cache
    TypeInfo1 = spectra_module_types:get(other),
    ?assert(is_record(TypeInfo1, type_info)),

    % Second call should use cache (we can't directly verify this without
    % internal state inspection, but we can verify it returns same structure)
    TypeInfo2 = spectra_module_types:get(other),
    ?assertEqual(TypeInfo1, TypeInfo2),

    % Clean up - disable cache for other tests
    application:set_env(spectra, use_module_types_cache, false),
    spectra_module_types:clear(other).

%% Test that cache is invalidated when module is cleared
cache_clear_test() ->
    application:set_env(spectra, use_module_types_cache, true),

    % Get types to populate cache
    _TypeInfo1 = spectra_module_types:get(other),

    % Clear cache
    ok = spectra_module_types:clear(other),

    % Get types again - should repopulate cache
    TypeInfo2 = spectra_module_types:get(other),
    ?assert(is_record(TypeInfo2, type_info)),

    % Clean up
    application:set_env(spectra, use_module_types_cache, false),
    spectra_module_types:clear(other).

%% Test that accessing non-existent module throws error
get_nonexistent_module_test() ->
    application:set_env(spectra, use_module_types_cache, false),

    % Trying to get types for non-existent module should error
    ?assertError(
        {module_types_not_found, nonexistent_module_xyz, non_existing},
        spectra_module_types:get(nonexistent_module_xyz)
    ).

%% Test that accessing non-existent module with cache enabled throws error
get_nonexistent_module_with_cache_test() ->
    application:set_env(spectra, use_module_types_cache, true),

    % Trying to get types for non-existent module should error
    ?assertError(
        {module_types_not_found, nonexistent_module_abc, non_existing},
        spectra_module_types:get(nonexistent_module_abc)
    ),

    % Clean up
    application:set_env(spectra, use_module_types_cache, false).

%% Test that cache preserves type info correctly across multiple retrievals
cache_consistency_test() ->
    application:set_env(spectra, use_module_types_cache, true),
    spectra_module_types:clear(other),

    % Get types multiple times
    TypeInfo1 = spectra_module_types:get(other),
    TypeInfo2 = spectra_module_types:get(other),
    TypeInfo3 = spectra_module_types:get(other),

    % All should be identical
    ?assertEqual(TypeInfo1, TypeInfo2),
    ?assertEqual(TypeInfo2, TypeInfo3),

    % And should still be able to extract specific types
    {ok, _AccountType} = spectra_type_info:get_type(TypeInfo3, account, 0),

    % Clean up
    application:set_env(spectra, use_module_types_cache, false),
    spectra_module_types:clear(other).
