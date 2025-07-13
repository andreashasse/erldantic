-module(todo_test).

% -type stack_trim_fun() :: fun((module(), atom(), arity()) -> boolean()).
% -type neg_integer() :: -1000..-1
% -type maybe_improper() :: maybe_improper_list(integer(), atom()).
% -type nonempty() :: [integer(), ...].
% -type nonempty_improper() :: nonempty_improper_list(integer(), atom()).
% -type list_no_type() :: list().
% -type my_mfa() :: mfa().
% -type ar() :: arity().
% -type my_iodata() :: iodata().
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
