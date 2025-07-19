-module(erldantic_util).

-export([not_handled_modules/0, test_abs_code/1, fold_until_error/3, map_until_error/2]).

-ignore_xref([not_handled_modules/0, test_abs_code/1]).

-include_lib("erldantic/include/erldantic.hrl").

-spec not_handled_modules() -> [{atom(), term()}].
not_handled_modules() ->
    Modules = erlang:loaded(),
    lists:filtermap(fun(Module) ->
                       case test_abs_code(Module) of
                           {ok, _Types} ->
                               false;
                           {error, [#ed_error{} | _]} ->
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

-spec fold_until_error(Fun ::
                           fun((Elem :: dynamic(), Acc :: dynamic()) ->
                                   {error, Err :: dynamic()} | {ok, Acc :: dynamic()}),
                       Acc :: dynamic(),
                       List :: [Elem :: dynamic()]) ->
                          {ok, Acc :: dynamic()} | {error, Err :: dynamic()}.
fold_until_error(Fun, Acc, [H | T]) ->
    case Fun(H, Acc) of
        {error, _} = Error ->
            Error;
        {ok, NewAcc} ->
            fold_until_error(Fun, NewAcc, T)
    end;
fold_until_error(Fun, Acc, []) when is_function(Fun, 2) ->
    {ok, Acc}.

-spec map_until_error(fun((Elem :: dynamic()) ->
                              {error, Err :: dynamic()} | {ok, ResElem :: dynamic()}),
                      [Elem :: dynamic()]) ->
                         {ok, [ResElem :: dynamic()]} | {error, Err :: dynamic()}.
map_until_error(Fun, List) when is_function(Fun, 1) ->
    map_until_error(Fun, List, []).

map_until_error(Fun, [], Acc) when is_function(Fun, 1) ->
    {ok, lists:reverse(Acc)};
map_until_error(Fun, [H | T], Acc) ->
    case Fun(H) of
        {error, _} = Error ->
            Error;
        {ok, Value} ->
            map_until_error(Fun, T, [Value | Acc])
    end.
