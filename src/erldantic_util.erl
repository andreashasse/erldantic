-module(erldantic_util).

-export([not_handled_modules/0, test_abs_code/1]).

-ignore_xref([not_handled_modules/0, test_abs_code/1]).

-include_lib("erldantic/include/erldantic.hrl").

-spec not_handled_modules() -> [{atom(), term()}].
not_handled_modules() ->
    Modules = erlang:loaded(),
    lists:filtermap(fun(Module) ->
                       case test_abs_code(Module) of
                           {ok, _Types} ->
                               false;
                           {error, [#ed_error{}|_]} ->
                               false;
                           {error, Reason} ->
                               {true, {Module, Reason}}
                       end
                    end,
                    Modules).

-spec test_abs_code(module()) ->
                       {ok, erldantic:type_info()} |
                       {error, {atom(), term(), erlang:stacktrace()}} |
                       {error, [erldantic:error()]}.
test_abs_code(Module) ->
    try
        erldantic_abstract_code:types_in_module(Module)
    catch
        Class:Reason:Stacktrace ->
            {error, {Class, Reason, Stacktrace}}
    end.
