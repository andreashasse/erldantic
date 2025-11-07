-module(spectra_type).

-include("../include/spectra_internal.hrl").

-export([can_be_missing/3, is_type_reference/1]).

-spec can_be_missing(
    TypeInfo :: spectra:type_info(), Type :: spectra:sp_type(), spectra:missing_value()
) ->
    boolean().
can_be_missing(TypeInfo, Type, MissingType) ->
    %% DON'T PASS IN MISSING TYPE: Return it.
    case Type of
        #sp_type_with_variables{type = Type2} ->
            can_be_missing(TypeInfo, Type2, MissingType);
        #sp_union{types = Types} ->
            lists:any(fun(T) -> can_be_missing(TypeInfo, T, MissingType) end, Types);
        #sp_literal{value = LiteralValue} when LiteralValue =:= MissingType ->
            true;
        #sp_user_type_ref{type_name = TypeName, variables = TypeArgs} ->
            TypeArity = length(TypeArgs),
            {ok, RefType} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
            can_be_missing(TypeInfo, RefType, MissingType);
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
