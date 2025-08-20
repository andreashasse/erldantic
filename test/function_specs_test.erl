-module(function_specs_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

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

%% Function using external module records (if available)
-spec external_type_func(external_type:some_record()) -> ok.
external_type_func(_) ->
    ok.

function_spec_extraction_test() ->
    Types = erldantic_abstract_code:types_in_module(?MODULE),

    %% Test my_function/2 spec extraction
    ?assertMatch(#ed_function_spec{args =
                                       [#ed_simple_type{type = integer},
                                        #ed_simple_type{type = string}],
                                   return = #ed_simple_type{type = boolean}},
                 maps:get({function, my_function, 2}, Types)),

    %% Test simple_func/1 spec extraction
    ?assertMatch(#ed_function_spec{args = [#ed_simple_type{type = atom}],
                                   return = #ed_simple_type{type = term}},
                 maps:get({function, simple_func, 1}, Types)),

    %% Test no_arg_func/0 spec extraction
    ?assertMatch(#ed_function_spec{args = [], return = #ed_simple_type{type = integer}},
                 maps:get({function, no_arg_func, 0}, Types)),

    %% Test complex_func/2 spec extraction - more complex types
    ?assertMatch(#ed_function_spec{args =
                                       [#ed_list{type = #ed_simple_type{type = integer}},
                                        #ed_map{fields =
                                                    [{map_field_type_assoc,
                                                      #ed_simple_type{type = atom},
                                                      #ed_simple_type{type = binary}}]}],
                                   return =
                                       #ed_union{types =
                                                     [#ed_tuple{fields =
                                                                    [#ed_literal{value = ok},
                                                                     #ed_simple_type{type = pid}]},
                                                      #ed_tuple{fields =
                                                                    [#ed_literal{value = error},
                                                                     #ed_simple_type{type =
                                                                                         atom}]}]}},
                 maps:get({function, complex_func, 2}, Types)).

user_defined_types_test() ->
    Types = erldantic_abstract_code:types_in_module(?MODULE),

    %% Test function with user-defined record argument
    ?assertMatch(#ed_function_spec{args = [#ed_rec_ref{record_name = user}],
                                   return = #ed_user_type_ref{type_name = my_custom_type}},
                 maps:get({function, process_user, 1}, Types)),

    %% Test function returning user-defined record
    ?assertMatch(#ed_function_spec{args =
                                       [#ed_user_type_ref{type_name = my_id},
                                        #ed_simple_type{type = string}],
                                   return = #ed_rec_ref{record_name = user}},
                 maps:get({function, create_user, 2}, Types)),

    %% Test function with record argument and tuple return
    ?assertMatch(#ed_function_spec{args = [#ed_rec_ref{record_name = response}],
                                   return =
                                       #ed_tuple{fields =
                                                     [#ed_simple_type{type = integer},
                                                      #ed_simple_type{type = term}]}},
                 maps:get({function, handle_response, 1}, Types)),

    %% Test function using remote types
    ?assertMatch(#ed_function_spec{args =
                                       [#ed_map{fields =
                                                    [{map_field_type_assoc,
                                                      #ed_simple_type{type = term},
                                                      #ed_simple_type{type = term}}]}],
                                   return =
                                       #ed_list{type = #ed_remote_type{mfargs = {maps, key, []}}}},
                 maps:get({function, get_keys, 1}, Types)),

    %% Test function with remote type argument
    ?assertMatch(#ed_function_spec{args =
                                       [#ed_remote_type{mfargs = {calendar, datetime, []}}],
                                   return = #ed_simple_type{type = string}},
                 maps:get({function, format_datetime, 1}, Types)).

parametrized_types_test() ->
    Types = erldantic_abstract_code:types_in_module(?MODULE),

    %% Test function using parametrized user-defined types
    ?assertMatch(#ed_function_spec{args =
                                       [#ed_user_type_ref{type_name = my_list,
                                                          variables =
                                                              [#ed_simple_type{type = integer}]}],
                                   return =
                                       #ed_user_type_ref{type_name = my_list,
                                                         variables =
                                                             [#ed_simple_type{type = binary}]}},
                 maps:get({function, process_list, 1}, Types)),

    %% Test function with type variables in spec
    ?assertMatch(#ed_function_spec{args = [#ed_var{name = 'A'}, #ed_var{name = 'B'}],
                                   return =
                                       #ed_user_type_ref{type_name = my_pair,
                                                         variables =
                                                             [#ed_var{name = 'A'},
                                                              #ed_var{name = 'B'}]}},
                 maps:get({function, make_pair, 2}, Types)),

    %% Test function with complex type variables and functions
    ?assertMatch(#ed_function_spec{args =
                                       [#ed_user_type_ref{type_name = my_pair,
                                                          variables =
                                                              [#ed_var{name = 'A'},
                                                               #ed_var{name = 'B'}]},
                                        #ed_function{args = [#ed_var{name = 'A'}],
                                                     return = #ed_var{name = 'C'}},
                                        #ed_function{args = [#ed_var{name = 'B'}],
                                                     return = #ed_var{name = 'D'}}],
                                   return =
                                       #ed_user_type_ref{type_name = my_pair,
                                                         variables =
                                                             [#ed_var{name = 'C'},
                                                              #ed_var{name = 'D'}]}},
                 maps:get({function, transform_pair, 3}, Types)).

complex_types_test() ->
    Types = erldantic_abstract_code:types_in_module(?MODULE),

    %% Test function with union of user-defined and built-in types
    ?assertMatch(#ed_function_spec{args =
                                       [#ed_union{types =
                                                      [#ed_user_type_ref{type_name = my_id},
                                                       #ed_simple_type{type = binary}]}],
                                   return =
                                       #ed_union{types =
                                                     [#ed_tuple{fields =
                                                                    [#ed_literal{value = ok},
                                                                     #ed_user_type_ref{type_name =
                                                                                           my_id}]},
                                                      #ed_tuple{fields =
                                                                    [#ed_literal{value = error},
                                                                     #ed_literal{value =
                                                                                     invalid_id}]}]}},
                 maps:get({function, process_id, 1}, Types)),

    %% Test function with external module record type
    ?assertMatch(#ed_function_spec{args =
                                       [#ed_remote_type{mfargs = {external_type, some_record, []}}],
                                   return = #ed_literal{value = ok}},
                 maps:get({function, external_type_func, 1}, Types)).
