-module(fun_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/impala.hrl").
-include("../include/impala_internal.hrl").

-compile([nowarn_unused_type]).

-type fun1() :: fun().
-type fun2() :: fun((...) -> integer()).
-type fun3() :: fun(() -> integer()).
-type fun4() :: fun((integer(), atom()) -> integer()).
-type fun5() :: fun((integer(), atom()) -> fun()).
-type fun6() :: fun((integer(), atom()) -> fun((integer()) -> integer())).

-record(with_fun, {id :: integer(), handler :: fun()}).

erl_abstract_code_parses_fun_types_test() ->
    TypeInfo = impala_abstract_code:types_in_module(?MODULE),
    {ok, Fun1Type} = impala_type_info:get_type(TypeInfo, fun1, 0),
    ?assertEqual(#im_function{args = any, return = #im_simple_type{type = term}}, Fun1Type),
    {ok, Fun2Type} = impala_type_info:get_type(TypeInfo, fun2, 0),
    ?assertEqual(#im_function{args = any, return = #im_simple_type{type = integer}},
                 Fun2Type),
    {ok, Fun3Type} = impala_type_info:get_type(TypeInfo, fun3, 0),
    ?assertEqual(#im_function{args = [], return = #im_simple_type{type = integer}}, Fun3Type),
    {ok, Fun4Type} = impala_type_info:get_type(TypeInfo, fun4, 0),
    ?assertEqual(#im_function{args =
                                  [#im_simple_type{type = integer}, #im_simple_type{type = atom}],
                              return = #im_simple_type{type = integer}},
                 Fun4Type),
    {ok, Fun5Type} = impala_type_info:get_type(TypeInfo, fun5, 0),
    ?assertEqual(#im_function{args =
                                  [#im_simple_type{type = integer}, #im_simple_type{type = atom}],
                              return =
                                  #im_function{args = any, return = #im_simple_type{type = term}}},
                 Fun5Type),
    {ok, Fun6Type} = impala_type_info:get_type(TypeInfo, fun6, 0),
    ?assertEqual(#im_function{args =
                                  [#im_simple_type{type = integer}, #im_simple_type{type = atom}],
                              return =
                                  #im_function{args = [#im_simple_type{type = integer}],
                                               return = #im_simple_type{type = integer}}},
                 Fun6Type),
    {ok, WithFunRecord} = impala_type_info:get_record(TypeInfo, with_fun),
    ?assertEqual(#im_rec{name = with_fun,
                         fields =
                             [{id, #im_simple_type{type = integer}},
                              {handler,
                               #im_function{args = any, return = #im_simple_type{type = term}}}],
                         arity = 3},
                 WithFunRecord).

impala_json_rejects_fun_data_test() ->
    Fun1 = fun() -> ok end,
    Fun2 = fun(_, _) -> 1 end,
    Fun3 = fun() -> 1 end,
    Fun4 = fun(I, A) when is_integer(I), is_atom(A) -> 1 end,
    Fun5 = fun(I, A) when is_integer(I), is_atom(A) -> fun() -> ok end end,
    Fun6 = fun(I, A) when is_integer(I), is_atom(A) -> fun(X) when is_integer(X) -> X end end,
    ?assertError({type_not_supported, _},
                 impala_json:to_json(?MODULE, {type, fun1, 0}, Fun1)),
    ?assertError({type_not_supported, _},
                 impala_json:to_json(?MODULE, {type, fun2, 0}, Fun2)),
    ?assertError({type_not_supported, _},
                 impala_json:to_json(?MODULE, {type, fun3, 0}, Fun3)),
    ?assertError({type_not_supported, _},
                 impala_json:to_json(?MODULE, {type, fun4, 0}, Fun4)),
    ?assertError({type_not_supported, _},
                 impala_json:to_json(?MODULE, {type, fun5, 0}, Fun5)),
    ?assertError({type_not_supported, _},
                 impala_json:to_json(?MODULE, {type, fun6, 0}, Fun6)).

impala_json_rejects_fun_data_from_json_test() ->
    Data = <<"any">>,
    ?assertError({type_not_supported, _},
                 impala_json:from_json(?MODULE, {type, fun1, 0}, Data)),
    ?assertError({type_not_supported, _},
                 impala_json:from_json(?MODULE, {type, fun2, 0}, Data)),
    ?assertError({type_not_supported, _},
                 impala_json:from_json(?MODULE, {type, fun3, 0}, Data)),
    ?assertError({type_not_supported, _},
                 impala_json:from_json(?MODULE, {type, fun4, 0}, Data)),
    ?assertError({type_not_supported, _},
                 impala_json:from_json(?MODULE, {type, fun5, 0}, Data)),
    ?assertError({type_not_supported, _},
                 impala_json:from_json(?MODULE, {type, fun6, 0}, Data)).

impala_json_rejects_record_with_fun_field_test() ->
    Record = #with_fun{id = 1, handler = fun() -> ok end},
    ?assertError({type_not_supported, _},
                 impala_json:to_json(?MODULE, {record, with_fun}, Record)).

impala_json_rejects_data_containing_fun_test() ->
    DataWithFun = {ok, fun() -> ok end},
    ?assertError({type_not_supported, _},
                 impala_json:to_json(?MODULE, {type, fun1, 0}, DataWithFun)).
