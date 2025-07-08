-module(todo_test).

% -type stack_trim_fun() :: fun((module(), atom(), arity()) -> boolean()).
% -type neg_integer() :: -1..-1000.
% -type fun1() :: fun().
% -type fun2() :: fun((...) -> integer()).
% -type fun3() :: fun(() -> integer()).
% -type fun4() :: fun((integer(), atom()) -> integer()).
% -type fun5() :: fun((integer(), atom()) -> fun()).
% -type fun6() :: fun((integer(), atom()) -> fun((integer()) -> integer())).
% -type tuple() :: {}.
% -type tuple2() :: {integer(), atom()}.
% -type tuple3() :: tuple().
% -type maybe_improper() :: maybe_improper_list(integer(), atom()).
% -type nonempty() :: [integer(), ...].
% -type nonempty_improper() :: nonempty_improper_list(integer(), atom()).
% -type list_no_type() :: list().
% -type my_mfa() :: mfa().
% -type ar() :: arity().
% -type my_map() :: map().
% -type my_iodata() :: iodata().
% -type my_tuple() :: tuple().
% -type my_list() :: iolist().
% -type my_timeout() :: timeout().
%
% From peer.erl
%-type connection() ::
        % Port :: 0..65535 |

                % {inet:ip_address(), 0..65535} |
                % standard_io.

%% Tuple and funs without types needs better representation

%% bit strings, dynamic, none(), any(), term(), pid(), port(), reference(),
%%
%% Default values in record fields
