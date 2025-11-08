-module(spectra_type).

-include("../include/spectra_internal.hrl").

-export([can_be_missing/2, is_type_reference/1]).

-spec can_be_missing(
    TypeInfo :: spectra:type_info(), Type :: spectra:sp_type()
) ->
    {true, spectra:missing_value()} | false.
can_be_missing(TypeInfo, Type) ->
    case Type of
        #sp_type_with_variables{type = Type2} ->
            can_be_missing(TypeInfo, Type2);
        #sp_union{types = Types} ->
            case lists:filtermap(fun(T) -> can_be_missing(TypeInfo, T) end, Types) of
                [] ->
                    false;
                MissingValues ->
                    {true, lists:last(MissingValues)}
            end;
        #sp_literal{value = LiteralValue} when
            LiteralValue =:= nil orelse LiteralValue =:= undefined
        ->
            {true, LiteralValue};
        #sp_user_type_ref{type_name = TypeName, variables = TypeArgs} ->
            TypeArity = length(TypeArgs),
            {ok, RefType} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
            can_be_missing(TypeInfo, RefType);
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
