-module(spectra_module_types).

-export([get/1, clear/1]).

%% Meant to be used when doing manual testing.
-ignore_xref([clear/1]).

-type module_version() :: term().

-define(APPLICATION, spectra).

%% API
-spec get(Module :: module()) -> spectra:type_info().
get(Module) ->
    case application:get_env(?APPLICATION, use_module_types_cache, false) of
        true ->
            {ok, Vsn} = module_vsn(Module),
            case pers_type(Module) of
                {Vsn, TypeInfo} when is_map(TypeInfo) ->
                    TypeInfo;
                _ ->
                    TypeInfo = spectra_abstract_code:types_in_module(Module),
                    pers_types_set(Module, Vsn, TypeInfo),
                    TypeInfo
            end;
        false ->
            spectra_abstract_code:types_in_module(Module)
    end.

-spec clear(Module :: module()) -> ok.
clear(Module) ->
    persistent_term:erase({?MODULE, pers_types, Module}),
    ok.

%% INTERNAL

-spec pers_type(Module :: module()) ->
    {module_version(), spectra:type_info()} | undefined.
pers_type(Module) ->
    persistent_term:get({?MODULE, pers_types, Module}, undefined).

-spec pers_types_set(
    Module :: module(),
    Vsn :: module_version(),
    TypeInfo :: spectra:type_info()
) ->
    ok.
pers_types_set(Module, Vsn, TypeInfo) ->
    persistent_term:put({?MODULE, pers_types, Module}, {Vsn, TypeInfo}).

ensure_module(Module) ->
    erlang:module_loaded(Module) orelse code:which(Module) =/= non_existing.

-spec module_vsn(Module :: module()) ->
    {ok, Version :: module_version()} | {error, [spectra:error()]}.
module_vsn(Module) ->
    case ensure_module(Module) of
        true ->
            case erlang:get_module_info(Module, attributes) of
                Attrs when is_list(Attrs) ->
                    {vsn, Vsn} = lists:keyfind(vsn, 1, Attrs),
                    {ok, Vsn}
            end;
        false ->
            erlang:error({module_types_not_found, Module, non_existing})
    end.
