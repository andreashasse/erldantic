-module(fun_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-compile([nowarn_unused_type]).

-type fun1() :: fun().
-type fun2() :: fun((...) -> integer()).
-type fun3() :: fun(() -> integer()).
-type fun4() :: fun((integer(), atom()) -> integer()).
-type fun5() :: fun((integer(), atom()) -> fun()).
-type fun6() :: fun((integer(), atom()) -> fun((integer()) -> integer())).

-record(with_fun, {id :: integer(), handler :: fun()}).

erl_abstract_code_parses_fun_types_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),
    {ok, Fun1Type} = erldantic_type_info:get_type(TypeInfo, fun1, 0),
    ?assertEqual(#ed_function{args = any, return = #ed_simple_type{type = term}}, Fun1Type),
    {ok, Fun2Type} = erldantic_type_info:get_type(TypeInfo, fun2, 0),
    ?assertEqual(#ed_function{args = any, return = #ed_simple_type{type = integer}},
                 Fun2Type),
    {ok, Fun3Type} = erldantic_type_info:get_type(TypeInfo, fun3, 0),
    ?assertEqual(#ed_function{args = [], return = #ed_simple_type{type = integer}}, Fun3Type),
    {ok, Fun4Type} = erldantic_type_info:get_type(TypeInfo, fun4, 0),
    ?assertEqual(#ed_function{args =
                                  [#ed_simple_type{type = integer}, #ed_simple_type{type = atom}],
                              return = #ed_simple_type{type = integer}},
                 Fun4Type),
    {ok, Fun5Type} = erldantic_type_info:get_type(TypeInfo, fun5, 0),
    ?assertEqual(#ed_function{args =
                                  [#ed_simple_type{type = integer}, #ed_simple_type{type = atom}],
                              return =
                                  #ed_function{args = any, return = #ed_simple_type{type = term}}},
                 Fun5Type),
    {ok, Fun6Type} = erldantic_type_info:get_type(TypeInfo, fun6, 0),
    ?assertEqual(#ed_function{args =
                                  [#ed_simple_type{type = integer}, #ed_simple_type{type = atom}],
                              return =
                                  #ed_function{args = [#ed_simple_type{type = integer}],
                                               return = #ed_simple_type{type = integer}}},
                 Fun6Type),
    {ok, WithFunRecord} = erldantic_type_info:get_record(TypeInfo, with_fun),
    ?assertEqual(#ed_rec{name = with_fun,
                         fields =
                             [{id, #ed_simple_type{type = integer}},
                              {handler,
                               #ed_function{args = any, return = #ed_simple_type{type = term}}}],
                         arity = 3},
                 WithFunRecord).

erldantic_json_rejects_fun_data_test() ->
    Fun1 = fun() -> ok end,
    Fun2 = fun(_, _) -> 1 end,
    Fun3 = fun() -> 1 end,
    Fun4 = fun(I, A) when is_integer(I), is_atom(A) -> 1 end,
    Fun5 = fun(I, A) when is_integer(I), is_atom(A) -> fun() -> ok end end,
    Fun6 = fun(I, A) when is_integer(I), is_atom(A) -> fun(X) when is_integer(X) -> X end end,
    ?assertError({type_not_supported, _}, erldantic_json:type_to_json(?MODULE, fun1, Fun1)),
    ?assertError({type_not_supported, _}, erldantic_json:type_to_json(?MODULE, fun2, Fun2)),
    ?assertError({type_not_supported, _}, erldantic_json:type_to_json(?MODULE, fun3, Fun3)),
    ?assertError({type_not_supported, _}, erldantic_json:type_to_json(?MODULE, fun4, Fun4)),
    ?assertError({type_not_supported, _}, erldantic_json:type_to_json(?MODULE, fun5, Fun5)),
    ?assertError({type_not_supported, _}, erldantic_json:type_to_json(?MODULE, fun6, Fun6)).

erldantic_json_rejects_fun_data_from_json_test() ->
    Data = <<"any">>,
    ?assertError({type_not_supported, _}, erldantic_json:type_from_json(?MODULE, fun1, Data)),
    ?assertError({type_not_supported, _}, erldantic_json:type_from_json(?MODULE, fun2, Data)),
    ?assertError({type_not_supported, _}, erldantic_json:type_from_json(?MODULE, fun3, Data)),
    ?assertError({type_not_supported, _}, erldantic_json:type_from_json(?MODULE, fun4, Data)),
    ?assertError({type_not_supported, _}, erldantic_json:type_from_json(?MODULE, fun5, Data)),
    ?assertError({type_not_supported, _}, erldantic_json:type_from_json(?MODULE, fun6, Data)).

erldantic_json_rejects_record_with_fun_field_test() ->
    Record = #with_fun{id = 1, handler = fun() -> ok end},
    ?assertError({type_not_supported, _},
                 erldantic_json:record_to_json(?MODULE, with_fun, Record)).

erldantic_json_rejects_data_containing_fun_test() ->
    DataWithFun = {ok, fun() -> ok end},
    ?assertError({type_not_supported, _},
                 erldantic_json:type_to_json(?MODULE, fun1, DataWithFun)).
