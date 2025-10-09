-module(function_specs_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/impala.hrl").
-include("../include/impala_internal.hrl").

-compile(nowarn_unused_type).
-compile(nowarn_unused_function).

%% User-defined types for testing
-type my_custom_type() :: {ok, binary()} | {error, atom()}.
-type my_id() :: pos_integer().
-type my_list(T) :: [T].
-type my_pair(A, B) :: {A, B}.

%% Records for testing
-record(user, {id :: my_id(), name :: string(), active :: boolean()}).
-record(response, {status :: integer(), data :: term()}).

%% Test functions with different specs
-spec my_function(integer(), string()) -> boolean().
my_function(N, S) ->
    is_integer(N) andalso is_list(S).

-spec simple_func(atom()) -> term().
simple_func(_A) ->
    ok.

-spec complex_func([integer()], #{atom() => binary()}) -> {ok, pid()} | {error, atom()}.
complex_func(_List, _Map) ->
    {ok, self()}.

-spec no_arg_func() -> integer().
no_arg_func() ->
    42.

%% Functions using user-defined types
-spec process_user(#user{}) -> my_custom_type().
process_user(_User) ->
    {ok, <<"processed">>}.

-spec create_user(my_id(), string()) -> #user{}.
create_user(Id, Name) ->
    #user{id = Id,
          name = Name,
          active = true}.

-spec handle_response(#response{}) -> {integer(), term()}.
handle_response(#response{status = Status, data = Data}) ->
    {Status, Data}.

%% Function using remote types
-spec get_keys(map()) -> [maps:key()].
get_keys(Map) ->
    maps:keys(Map).

-spec format_datetime(calendar:datetime()) -> string().
format_datetime(_DateTime) ->
    "formatted".

%% Functions using parametrized types
-spec process_list(my_list(integer())) -> my_list(binary()).
process_list(List) ->
    [integer_to_binary(I) || I <- List].

-spec make_pair(A, B) -> my_pair(A, B).
make_pair(A, B) ->
    {A, B}.

%% Function with multiple type variables
-spec transform_pair(my_pair(A, B), fun((A) -> C), fun((B) -> D)) -> my_pair(C, D).
transform_pair({A, B}, F1, F2) ->
    {F1(A), F2(B)}.

%% Function with constraints and complex unions
-spec process_id(my_id() | binary()) -> {ok, my_id()} | {error, invalid_id}.
process_id(Id) when is_integer(Id) ->
    {ok, Id};
process_id(_) ->
    {error, invalid_id}.

%% Function with bounded_fun spec (using when constraints)
-spec with_bound_fun(Module, Args) -> integer()
    when Module :: module(),
         Args :: [integer()].
with_bound_fun(_Module, _Args) ->
    42.

%% Function with multiple clauses in spec
-spec multi_clause_func(my_id(), string()) -> #user{};
                       (binary(), atom()) -> boolean().
multi_clause_func(Id, Name) when is_integer(Id) ->
    #user{id = Id,
          name = Name,
          active = true};
multi_clause_func(_Binary, _Atom) ->
    false.

%% Function with mixed bounded_fun and regular fun clauses
-spec mixed_spec_func(Module, Args) -> integer()
                         when Module :: atom(),
                              Args :: [term()];
                     (binary(), atom()) -> string().
mixed_spec_func(Module, Args) when is_atom(Module) ->
    length(Args);
mixed_spec_func(Binary, _Atom) when is_binary(Binary) ->
    binary_to_list(Binary).

%% Function using external module records (if available)
-spec external_type_func(external_type:some_record()) -> ok.
external_type_func(_) ->
    ok.

function_spec_extraction_test() ->
    TypeInfo = impala_abstract_code:types_in_module(?MODULE),

    %% Test my_function/2 spec extraction
    {ok, [MyFunctionSpec]} = impala_type_info:get_function(TypeInfo, my_function, 2),
    ?assertMatch(#im_function_spec{args =
                                       [#im_simple_type{type = integer},
                                        #im_simple_type{type = string}],
                                   return = #im_simple_type{type = boolean}},
                 MyFunctionSpec),

    %% Test simple_func/1 spec extraction
    {ok, [SimpleFuncSpec]} = impala_type_info:get_function(TypeInfo, simple_func, 1),
    ?assertMatch(#im_function_spec{args = [#im_simple_type{type = atom}],
                                   return = #im_simple_type{type = term}},
                 SimpleFuncSpec),

    %% Test no_arg_func/0 spec extraction
    {ok, [NoArgFuncSpec]} = impala_type_info:get_function(TypeInfo, no_arg_func, 0),
    ?assertMatch(#im_function_spec{args = [], return = #im_simple_type{type = integer}},
                 NoArgFuncSpec),

    %% Test complex_func/2 spec extraction - more complex types
    {ok, [ComplexFuncSpec]} = impala_type_info:get_function(TypeInfo, complex_func, 2),
    ?assertMatch(#im_function_spec{args =
                                       [#im_list{type = #im_simple_type{type = integer}},
                                        #im_map{fields =
                                                    [{map_field_type_assoc,
                                                      #im_simple_type{type = atom},
                                                      #im_simple_type{type = binary}}]}],
                                   return =
                                       #im_union{types =
                                                     [#im_tuple{fields =
                                                                    [#im_literal{value = ok},
                                                                     #im_simple_type{type = pid}]},
                                                      #im_tuple{fields =
                                                                    [#im_literal{value = error},
                                                                     #im_simple_type{type =
                                                                                         atom}]}]}},
                 ComplexFuncSpec).

user_defined_types_test() ->
    TypeInfo = impala_abstract_code:types_in_module(?MODULE),

    %% Test function with user-defined record argument
    {ok, [ProcessUserSpec]} = impala_type_info:get_function(TypeInfo, process_user, 1),
    ?assertMatch(#im_function_spec{args = [#im_rec_ref{record_name = user}],
                                   return = #im_user_type_ref{type_name = my_custom_type}},
                 ProcessUserSpec),

    %% Test function returning user-defined record
    {ok, [CreateUserSpec]} = impala_type_info:get_function(TypeInfo, create_user, 2),
    ?assertMatch(#im_function_spec{args =
                                       [#im_user_type_ref{type_name = my_id},
                                        #im_simple_type{type = string}],
                                   return = #im_rec_ref{record_name = user}},
                 CreateUserSpec),

    %% Test function with record argument and tuple return
    {ok, [HandleResponseSpec]} = impala_type_info:get_function(TypeInfo, handle_response, 1),
    ?assertMatch(#im_function_spec{args = [#im_rec_ref{record_name = response}],
                                   return =
                                       #im_tuple{fields =
                                                     [#im_simple_type{type = integer},
                                                      #im_simple_type{type = term}]}},
                 HandleResponseSpec),

    %% Test function using remote types
    {ok, [GetKeysSpec]} = impala_type_info:get_function(TypeInfo, get_keys, 1),
    ?assertMatch(#im_function_spec{args =
                                       [#im_map{fields =
                                                    [{map_field_type_assoc,
                                                      #im_simple_type{type = term},
                                                      #im_simple_type{type = term}}]}],
                                   return =
                                       #im_list{type = #im_remote_type{mfargs = {maps, key, []}}}},
                 GetKeysSpec),

    %% Test function with remote type argument
    {ok, [FormatDatetimeSpec]} = impala_type_info:get_function(TypeInfo, format_datetime, 1),
    ?assertMatch(#im_function_spec{args =
                                       [#im_remote_type{mfargs = {calendar, datetime, []}}],
                                   return = #im_simple_type{type = string}},
                 FormatDatetimeSpec).

parametrized_types_test() ->
    TypeInfo = impala_abstract_code:types_in_module(?MODULE),

    %% Test function using parametrized user-defined types
    {ok, [ProcessListSpec]} = impala_type_info:get_function(TypeInfo, process_list, 1),
    ?assertMatch(#im_function_spec{args =
                                       [#im_user_type_ref{type_name = my_list,
                                                          variables =
                                                              [#im_simple_type{type = integer}]}],
                                   return =
                                       #im_user_type_ref{type_name = my_list,
                                                         variables =
                                                             [#im_simple_type{type = binary}]}},
                 ProcessListSpec),

    %% Test function with type variables in spec
    {ok, [MakePairSpec]} = impala_type_info:get_function(TypeInfo, make_pair, 2),
    ?assertMatch(#im_function_spec{args = [#im_var{name = 'A'}, #im_var{name = 'B'}],
                                   return =
                                       #im_user_type_ref{type_name = my_pair,
                                                         variables =
                                                             [#im_var{name = 'A'},
                                                              #im_var{name = 'B'}]}},
                 MakePairSpec),

    %% Test function with complex type variables and functions
    {ok, [TransformPairSpec]} = impala_type_info:get_function(TypeInfo, transform_pair, 3),
    ?assertMatch(#im_function_spec{args =
                                       [#im_user_type_ref{type_name = my_pair,
                                                          variables =
                                                              [#im_var{name = 'A'},
                                                               #im_var{name = 'B'}]},
                                        #im_function{args = [#im_var{name = 'A'}],
                                                     return = #im_var{name = 'C'}},
                                        #im_function{args = [#im_var{name = 'B'}],
                                                     return = #im_var{name = 'D'}}],
                                   return =
                                       #im_user_type_ref{type_name = my_pair,
                                                         variables =
                                                             [#im_var{name = 'C'},
                                                              #im_var{name = 'D'}]}},
                 TransformPairSpec).

complex_types_test() ->
    TypeInfo = impala_abstract_code:types_in_module(?MODULE),

    %% Test function with union of user-defined and built-in types
    {ok, [ProcessIdSpec]} = impala_type_info:get_function(TypeInfo, process_id, 1),
    ?assertMatch(#im_function_spec{args =
                                       [#im_union{types =
                                                      [#im_user_type_ref{type_name = my_id},
                                                       #im_simple_type{type = binary}]}],
                                   return =
                                       #im_union{types =
                                                     [#im_tuple{fields =
                                                                    [#im_literal{value = ok},
                                                                     #im_user_type_ref{type_name =
                                                                                           my_id}]},
                                                      #im_tuple{fields =
                                                                    [#im_literal{value = error},
                                                                     #im_literal{value =
                                                                                     invalid_id}]}]}},
                 ProcessIdSpec),

    %% Test function with external module record type
    {ok, [ExternalTypeFuncSpec]} =
        impala_type_info:get_function(TypeInfo, external_type_func, 1),
    ?assertMatch(#im_function_spec{args =
                                       [#im_remote_type{mfargs = {external_type, some_record, []}}],
                                   return = #im_literal{value = ok}},
                 ExternalTypeFuncSpec).

bounded_fun_spec_test() ->
    TypeInfo = impala_abstract_code:types_in_module(?MODULE),

    %% Test bounded_fun spec with when constraints
    {ok, [WithBoundFunSpec]} = impala_type_info:get_function(TypeInfo, with_bound_fun, 2),
    ?assertMatch(#im_function_spec{args =
                                       [#im_simple_type{type = atom},
                                        #im_list{type = #im_simple_type{type = integer}}],
                                   return = #im_simple_type{type = integer}},
                 WithBoundFunSpec).

multi_clause_spec_test() ->
    TypeInfo = impala_abstract_code:types_in_module(?MODULE),
    {ok, MultiClauseSpecs} = impala_type_info:get_function(TypeInfo, multi_clause_func, 2),
    ?assertEqual([#im_function_spec{args =
                                        [#im_user_type_ref{type_name = my_id, variables = []},
                                         #im_simple_type{type = string}],
                                    return = #im_rec_ref{record_name = user, field_types = []}},
                  #im_function_spec{args =
                                        [#im_simple_type{type = binary},
                                         #im_simple_type{type = atom}],
                                    return = #im_simple_type{type = boolean}}],
                 MultiClauseSpecs).

mixed_bounded_fun_spec_test() ->
    TypeInfo = impala_abstract_code:types_in_module(?MODULE),
    {ok, MixedSpecs} = impala_type_info:get_function(TypeInfo, mixed_spec_func, 2),
    ?assertEqual(2, length(MixedSpecs)),
    ?assertEqual([#im_function_spec{args =
                                        [#im_simple_type{type = atom},
                                         #im_list{type = #im_simple_type{type = term}}],
                                    return = #im_simple_type{type = integer}},
                  #im_function_spec{args =
                                        [#im_simple_type{type = binary},
                                         #im_simple_type{type = atom}],
                                    return = #im_simple_type{type = string}}],
                 MixedSpecs).
