-module(fun_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-compile([nowarn_unused_type]).

-type fun1() :: fun().
-type fun2() :: fun((...) -> integer()).
-type fun3() :: fun(() -> integer()).
-type fun4() :: fun((integer(), atom()) -> integer()).
-type fun5() :: fun((integer(), atom()) -> fun()).
-type fun6() :: fun((integer(), atom()) -> fun((integer()) -> integer())).

-record(with_fun, {id :: integer(), handler :: fun()}).

erl_abstract_code_parses_fun_types_test() ->
    TypeInfo = spectra_abstract_code:types_in_module(?MODULE),
    {ok, Fun1Type} = spectra_type_info:get_type(TypeInfo, fun1, 0),
    ?assertEqual(#sp_function{args = any, return = #sp_simple_type{type = term}}, Fun1Type),
    {ok, Fun2Type} = spectra_type_info:get_type(TypeInfo, fun2, 0),
    ?assertEqual(
        #sp_function{args = any, return = #sp_simple_type{type = integer}},
        Fun2Type
    ),
    {ok, Fun3Type} = spectra_type_info:get_type(TypeInfo, fun3, 0),
    ?assertEqual(#sp_function{args = [], return = #sp_simple_type{type = integer}}, Fun3Type),
    {ok, Fun4Type} = spectra_type_info:get_type(TypeInfo, fun4, 0),
    ?assertEqual(
        #sp_function{
            args =
                [#sp_simple_type{type = integer}, #sp_simple_type{type = atom}],
            return = #sp_simple_type{type = integer}
        },
        Fun4Type
    ),
    {ok, Fun5Type} = spectra_type_info:get_type(TypeInfo, fun5, 0),
    ?assertEqual(
        #sp_function{
            args =
                [#sp_simple_type{type = integer}, #sp_simple_type{type = atom}],
            return =
                #sp_function{args = any, return = #sp_simple_type{type = term}}
        },
        Fun5Type
    ),
    {ok, Fun6Type} = spectra_type_info:get_type(TypeInfo, fun6, 0),
    ?assertEqual(
        #sp_function{
            args =
                [#sp_simple_type{type = integer}, #sp_simple_type{type = atom}],
            return =
                #sp_function{
                    args = [#sp_simple_type{type = integer}],
                    return = #sp_simple_type{type = integer}
                }
        },
        Fun6Type
    ),
    {ok, WithFunRecord} = spectra_type_info:get_record(TypeInfo, with_fun),
    ?assertEqual(
        #sp_rec{
            name = with_fun,
            fields =
                [
                    {id, #sp_simple_type{type = integer}},
                    {handler, #sp_function{args = any, return = #sp_simple_type{type = term}}}
                ],
            arity = 3
        },
        WithFunRecord
    ).

spectra_json_rejects_fun_data_test() ->
    Fun1 = fun() -> ok end,
    Fun2 = fun(_, _) -> 1 end,
    Fun3 = fun() -> 1 end,
    Fun4 = fun(I, A) when is_integer(I), is_atom(A) -> 1 end,
    Fun5 = fun(I, A) when is_integer(I), is_atom(A) -> fun() -> ok end end,
    Fun6 = fun(I, A) when is_integer(I), is_atom(A) -> fun(X) when is_integer(X) -> X end end,
    ?assertError(
        {type_not_supported, _},
        spectra_json:to_json(?MODULE, {type, fun1, 0}, Fun1)
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_json:to_json(?MODULE, {type, fun2, 0}, Fun2)
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_json:to_json(?MODULE, {type, fun3, 0}, Fun3)
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_json:to_json(?MODULE, {type, fun4, 0}, Fun4)
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_json:to_json(?MODULE, {type, fun5, 0}, Fun5)
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_json:to_json(?MODULE, {type, fun6, 0}, Fun6)
    ).

spectra_json_rejects_fun_data_from_json_test() ->
    Data = <<"any">>,
    ?assertError(
        {type_not_supported, _},
        spectra_json:from_json(?MODULE, {type, fun1, 0}, Data)
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_json:from_json(?MODULE, {type, fun2, 0}, Data)
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_json:from_json(?MODULE, {type, fun3, 0}, Data)
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_json:from_json(?MODULE, {type, fun4, 0}, Data)
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_json:from_json(?MODULE, {type, fun5, 0}, Data)
    ),
    ?assertError(
        {type_not_supported, _},
        spectra_json:from_json(?MODULE, {type, fun6, 0}, Data)
    ).

spectra_json_rejects_record_with_fun_field_test() ->
    Record = #with_fun{id = 1, handler = fun() -> ok end},
    ?assertError(
        {type_not_supported, _},
        spectra_json:to_json(?MODULE, {record, with_fun}, Record)
    ).

spectra_json_rejects_data_containing_fun_test() ->
    DataWithFun = {ok, fun() -> ok end},
    ?assertError(
        {type_not_supported, _},
        spectra_json:to_json(?MODULE, {type, fun1, 0}, DataWithFun)
    ).
