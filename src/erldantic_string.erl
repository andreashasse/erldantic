-module(erldantic_string).

-export([from_string/3]).

-ignore_xref([{erldantic_string, from_string, 3}]).

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

%% API

-doc("Converts a string value to an Erlang value based on a type specification.\nThis function validates the given string value against the specified type definition\nand converts it to the corresponding Erlang value.\n\n### Returns\n{ok, ErlangValue} if conversion succeeds, or {error, Errors} if validation fails").
-doc(#{params =>
           #{"String" => "The string value to convert to Erlang format",
             "Type" => "The type specification (erldantic:ed_type_or_ref())",
             "TypeInfo" => "The type information containing type definitions"}}).

-spec from_string(TypeInfo :: erldantic:type_info(),
                  Type :: erldantic:ed_type_or_ref(),
                  String :: string()) ->
                     {ok, term()} | {error, [erldantic:error()]}.
from_string(TypeInfo, {type, TypeName, TypeArity}, String) when is_atom(TypeName) ->
    {ok, Type} = erldantic_type_info:get_type(TypeInfo, TypeName, TypeArity),
    from_string(TypeInfo, Type, String);
from_string(_TypeInfo, {record, RecordName}, String) when is_atom(RecordName) ->
    {error,
     [#ed_error{type = no_match,
                location = [],
                ctx = #{type => {record, RecordName}, value => String}}]};
from_string(_TypeInfo, #ed_simple_type{type = NotSupported} = T, _String)
    when NotSupported =:= pid
         orelse NotSupported =:= port
         orelse NotSupported =:= reference
         orelse NotSupported =:= bitstring
         orelse NotSupported =:= nonempty_bitstring
         orelse NotSupported =:= none ->
    erlang:error({type_not_supported, T});
from_string(_TypeInfo, #ed_simple_type{type = PrimaryType}, String) ->
    convert_string_to_type(PrimaryType, String);
from_string(_TypeInfo,
            #ed_range{type = integer,
                      lower_bound = Min,
                      upper_bound = Max} =
                Range,
            String) ->
    case convert_string_to_type(integer, String) of
        {ok, Value} when Min =< Value, Value =< Max ->
            {ok, Value};
        {ok, Value} when is_integer(Value) ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => Range, value => Value}}]};
        {error, Reason} ->
            {error, Reason}
    end;
from_string(_TypeInfo, #ed_remote_type{mfargs = {Module, TypeName, Args}}, String) ->
    TypeInfo = erldantic_module_types:get(Module),
    TypeArity = length(Args),
    {ok, Type} = erldantic_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
    from_string(TypeInfo, TypeWithoutVars, String);
from_string(_TypeInfo, #ed_literal{value = Literal}, String) ->
    try_convert_string_to_literal(Literal, String);
from_string(TypeInfo, #ed_union{} = Type, String) ->
    union(fun from_string/3, TypeInfo, Type, String);
from_string(_TypeInfo, Type, String) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => Type, value => String}}]}.

%% INTERNAL

-spec convert_string_to_type(Type :: atom(), String :: string()) ->
                                {ok, term()} | {error, [erldantic:error()]}.
convert_string_to_type(integer, String) ->
    try
        {ok, list_to_integer(String)}
    catch
        error:badarg ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => #ed_simple_type{type = integer}, value => String}}]}
    end;
convert_string_to_type(float, String) ->
    try
        {ok, list_to_float(String)}
    catch
        error:badarg ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => #ed_simple_type{type = float}, value => String}}]}
    end;
convert_string_to_type(number, String) ->
    case convert_string_to_type(integer, String) of
        {ok, _} = Result ->
            Result;
        {error, _} ->
            convert_string_to_type(float, String)
    end;
convert_string_to_type(boolean, "true") ->
    {ok, true};
convert_string_to_type(boolean, "false") ->
    {ok, false};
convert_string_to_type(boolean, String) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = boolean}, value => String}}]};
convert_string_to_type(atom, String) ->
    try
        {ok, list_to_existing_atom(String)}
    catch
        error:badarg ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => #ed_simple_type{type = atom}, value => String}}]}
    end;
convert_string_to_type(string, String) ->
    {ok, String};
convert_string_to_type(nonempty_string, String) when String =/= [] ->
    {ok, String};
convert_string_to_type(nonempty_string, []) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = nonempty_string}, value => []}}]};
convert_string_to_type(binary, String) ->
    {ok, list_to_binary(String)};
convert_string_to_type(nonempty_binary, String) when String =/= [] ->
    {ok, list_to_binary(String)};
convert_string_to_type(nonempty_binary, []) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = nonempty_binary}, value => []}}]};
convert_string_to_type(non_neg_integer, String) ->
    case convert_string_to_type(integer, String) of
        {ok, Value} when Value >= 0 ->
            {ok, Value};
        {ok, Value} ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => #ed_simple_type{type = non_neg_integer}, value => Value}}]};
        {error, Reason} ->
            {error, Reason}
    end;
convert_string_to_type(pos_integer, String) ->
    case convert_string_to_type(integer, String) of
        {ok, Value} when Value > 0 ->
            {ok, Value};
        {ok, Value} ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => #ed_simple_type{type = pos_integer}, value => Value}}]};
        {error, Reason} ->
            {error, Reason}
    end;
convert_string_to_type(neg_integer, String) ->
    case convert_string_to_type(integer, String) of
        {ok, Value} when Value < 0 ->
            {ok, Value};
        {ok, Value} ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => #ed_simple_type{type = neg_integer}, value => Value}}]};
        {error, Reason} ->
            {error, Reason}
    end;
convert_string_to_type(Type, String) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => Type, value => String}}]}.

-spec try_convert_string_to_literal(Literal :: term(), String :: string()) ->
                                       {ok, term()} | {error, [erldantic:error()]}.
try_convert_string_to_literal(Literal, String) when is_atom(Literal) ->
    case convert_string_to_type(atom, String) of
        {ok, Literal} ->
            {ok, Literal};
        {ok, _Other} ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => #ed_literal{value = Literal}, value => String}}]};
        {error, Reason} ->
            {error, Reason}
    end;
try_convert_string_to_literal(Literal, String) when is_integer(Literal) ->
    case convert_string_to_type(integer, String) of
        {ok, Literal} ->
            {ok, Literal};
        {ok, _Other} ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => #ed_literal{value = Literal}, value => String}}]};
        {error, Reason} ->
            {error, Reason}
    end;
try_convert_string_to_literal(Literal, String) when is_boolean(Literal) ->
    case convert_string_to_type(boolean, String) of
        {ok, Literal} ->
            {ok, Literal};
        {ok, _Other} ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => #ed_literal{value = Literal}, value => String}}]};
        {error, Reason} ->
            {error, Reason}
    end;
try_convert_string_to_literal(Literal, String) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_literal{value = Literal}, value => String}}]}.

union(Fun, TypeInfo, #ed_union{types = Types} = T, String) ->
    case do_first(Fun, TypeInfo, Types, String) of
        {error, no_match} ->
            {error,
             [#ed_error{type = no_match,
                        location = [],
                        ctx = #{type => T, value => String}}]};
        Result ->
            Result
    end.

do_first(_Fun, _TypeInfo, [], _String) ->
    {error, no_match};
do_first(Fun, TypeInfo, [Type | Rest], String) ->
    case Fun(TypeInfo, Type, String) of
        {ok, Result} ->
            {ok, Result};
        {error, _} ->
            do_first(Fun, TypeInfo, Rest, String)
    end.

apply_args(TypeInfo, Type, TypeArgs) when is_list(TypeArgs) ->
    ArgNames = arg_names(Type),
    NamedTypes =
        maps:from_list(
            lists:zip(ArgNames, TypeArgs)),
    type_replace_vars(TypeInfo, Type, NamedTypes).

arg_names(#ed_type_with_variables{vars = Args}) ->
    Args;
arg_names(_) ->
    [].

-spec type_replace_vars(TypeInfo :: erldantic:type_info(),
                        Type :: erldantic:ed_type(),
                        NamedTypes :: #{atom() => erldantic:ed_type()}) ->
                           erldantic:ed_type().
type_replace_vars(_TypeInfo, #ed_var{name = Name}, NamedTypes) ->
    maps:get(Name, NamedTypes, #ed_simple_type{type = term});
type_replace_vars(TypeInfo, #ed_type_with_variables{type = Type}, NamedTypes) ->
    case Type of
        #ed_union{types = UnionTypes} ->
            #ed_union{types =
                          lists:map(fun(UnionType) ->
                                       type_replace_vars(TypeInfo, UnionType, NamedTypes)
                                    end,
                                    UnionTypes)};
        #ed_remote_type{mfargs = {Module, TypeName, Args}} ->
            case erldantic_module_types:get(Module) of
                {ok, TypeInfo} ->
                    TypeArity = length(Args),
                    case erldantic_type_info:get_type(TypeInfo, TypeName, TypeArity) of
                        {ok, Type} ->
                            type_replace_vars(TypeInfo, Type, NamedTypes);
                        error ->
                            erlang:error({missing_type, TypeName})
                    end;
                {error, _} = Err ->
                    erlang:error(Err)
            end
    end;
type_replace_vars(_TypeInfo, Type, _NamedTypes) ->
    Type.
