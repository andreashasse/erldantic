-module(erldantic_type).

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-export([can_be_undefined/2, is_type_reference/1]).

-spec can_be_undefined(TypeInfo :: erldantic:type_info(), Type :: erldantic:ed_type()) ->
    boolean().
can_be_undefined(TypeInfo, Type) ->
    case Type of
        #ed_type_with_variables{type = Type2} ->
            can_be_undefined(TypeInfo, Type2);
        #ed_union{types = Types} ->
            lists:any(fun(T) -> can_be_undefined(TypeInfo, T) end, Types);
        #ed_literal{value = undefined} ->
            true;
        #ed_user_type_ref{type_name = TypeName, variables = TypeArgs} ->
            TypeArity = length(TypeArgs),
            case erldantic_type_info:get_type(TypeInfo, TypeName, TypeArity) of
                {ok, Type2} ->
                    %% infinite recursion?
                    can_be_undefined(TypeInfo, Type2);
                error ->
                    %% error?
                    false
            end;
        _ ->
            false
    end.

-spec is_type_reference(erldantic:ed_type_or_ref()) -> boolean().
is_type_reference({type, _, _}) ->
    true;
is_type_reference({record, _}) ->
    true;
is_type_reference(_) ->
    false.
