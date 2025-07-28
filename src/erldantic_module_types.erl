-module(erldantic_module_types).

-export([get/1, clear/1]).

%% Meant to be used when doing manual testing.
-ignore_xref([clear/1]).

arity :: pos_integer()
-type module_version() :: term().

-include("../include/erldantic.hrl").

%% API
-spec get(Module :: module()) ->
             {ok, erldantic:type_info()} | {error, [erldantic:error()]}.
get(Module) ->
    case module_vsn(Module) of
        {ok, Vsn} ->
            case pers_type(Module) of
                {Vsn, TypeInfo} when is_map(TypeInfo) ->
                    {ok, TypeInfo};
                _ ->
                    case erldantic_abstract_code:types_in_module(Module) of
                        {ok, TypeInfo} ->
                            pers_types_set(Module, Vsn, TypeInfo),
                            {ok, TypeInfo};
                        {error, _} = Err ->
                            Err
                    end
            end;
        {error, _} = Err ->
            Err
    end.

-spec clear(Module :: module()) -> ok.
clear(Module) ->
    persistent_term:erase({?MODULE, pers_types, Module}),
    ok.

%% INTERNAL

-spec pers_type(Module :: module()) ->
                   {module_version(), erldantic:type_info()} | undefined.
pers_type(Module) ->
    persistent_term:get({?MODULE, pers_types, Module}, undefined).

-spec pers_types_set(Module :: module(),
                     Vsn :: module_version(),
                     TypeInfo :: erldantic:type_info()) ->
                        ok.
pers_types_set(Module, Vsn, TypeInfo) ->
    persistent_term:put({?MODULE, pers_types, Module}, {Vsn, TypeInfo}).

ensure_module(Module) ->
    erlang:module_loaded(Module) orelse code:which(Module) =/= non_existing.

-spec module_vsn(Module :: module()) ->
                    {ok, Version :: module_version()} | {error, [erldantic:error()]}.
module_vsn(Module) ->
    case ensure_module(Module) of
        true ->
            case erlang:get_module_info(Module, attributes) of
                Attrs when is_list(Attrs) ->
                    {vsn, Vsn} = lists:keyfind(vsn, 1, Attrs),
                    {ok, Vsn}
            end;
        false ->
            {error,
             [#ed_error{type = module_types_not_found,
                        location = [],
                        ctx = #{error => non_existing, module => Module}}]}
    end.
