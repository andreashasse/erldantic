-module(no_debug_info_test).

-include_lib("eunit/include/eunit.hrl").

no_debug_info_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun({ModuleName, _}) ->
        [test_module_without_debug_info(ModuleName)]
    end}.

setup() ->
    ModuleName = test_no_debug_module,
    SourceFile = atom_to_list(ModuleName) ++ ".erl",
    Code =
        "-module(" ++
            atom_to_list(ModuleName) ++
            ").\n"
            "-export([hello/0]).\n"
            "-type user_id() :: pos_integer().\n"
            "hello() -> ok.\n",
    ok = file:write_file(SourceFile, Code),

    {ok, ModuleName} = compile:file(SourceFile, []),

    {ModuleName, SourceFile}.

cleanup({ModuleName, SourceFile}) ->
    BeamFile = atom_to_list(ModuleName) ++ ".beam",
    file:delete(SourceFile),
    file:delete(BeamFile),
    code:purge(ModuleName),
    code:delete(ModuleName).

test_module_without_debug_info(ModuleName) ->
    ?_assertError(
        {module_not_compiled_with_debug_info, ModuleName, _},
        erldantic_abstract_code:types_in_module(ModuleName)
    ).
