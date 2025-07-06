-module(todo_test).

-type stack_trim_fun() :: fun((module(), atom(), arity()) -> boolean()).

-type neg_integer() :: -1 .. - 1000.

-type int_binary_op() :: 11 bor 7.

-type fun1() :: fun().

-type fun2() :: fun((...) -> integer()).

-type fun3() :: fun(() -> integer()).

-type fun4() :: fun((integer(), atom()) -> integer()).

-type fun5() :: fun((integer(), atom()) -> fun()).

-type fun6() :: fun((integer(), atom()) -> fun((integer()) -> integer())).

-type tuple() :: {}.

-type tuple2() :: {integer(), atom()}.

-type tuple3() :: tuple().

-type maybe_improper() :: maybe_improper_list(integer(), atom()).

-type nonempty() :: nonempty_list(integer()).

-type nonempty_improper() :: nonempty_improper_list(integer(), atom()).


%% bit strings, dynamic, none(), any(), term(), pid(), port(), reference(),
%%
%% Default values in record fields