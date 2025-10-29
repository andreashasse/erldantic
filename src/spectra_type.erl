-module(spectra_type).

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-export([can_be_undefined/2, is_type_reference/1]).

-spec can_be_undefined(TypeInfo :: spectra:type_info(), Type :: spectra:sp_type()) ->
    boolean().
can_be_undefined(TypeInfo, Type) ->
    case Type of
        #sp_type_with_variables{type = Type2} ->
            can_be_undefined(TypeInfo, Type2);
        #sp_union{types = Types} ->
            lists:any(fun(T) -> can_be_undefined(TypeInfo, T) end, Types);
        #sp_literal{value = undefined} ->
            true;
        #sp_user_type_ref{type_name = TypeName, variables = TypeArgs} ->
            TypeArity = length(TypeArgs),
            case spectra_type_info:get_type(TypeInfo, TypeName, TypeArity) of
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

-spec is_type_reference(spectra:sp_type_or_ref()) -> boolean().
is_type_reference({type, _, _}) ->
    true;
is_type_reference({record, _}) ->
    true;
is_type_reference(_) ->
    false.
