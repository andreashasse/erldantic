-module(erldantic_string).

-export([from_string/3, to_string/3]).

-ignore_xref([{erldantic_string, from_string, 3}, {erldantic_string, to_string, 3}]).

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
                  String :: list()) ->
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

-doc("Converts an Erlang value to a string based on a type specification.\nThis function validates the given Erlang value against the specified type definition\nand converts it to a string representation.\n\n### Returns\n{ok, String} if conversion succeeds, or {error, Errors} if validation fails").
-doc(#{params =>
           #{"Data" => "The Erlang value to convert to string format",
             "Type" => "The type specification (erldantic:ed_type_or_ref())",
             "TypeInfo" => "The type information containing type definitions"}}).

-spec to_string(TypeInfo :: erldantic:type_info(),
                Type :: erldantic:ed_type_or_ref(),
                Data :: term()) ->
                   {ok, string()} | {error, [erldantic:error()]}.
to_string(TypeInfo, {type, TypeName, TypeArity}, Data) when is_atom(TypeName) ->
    {ok, Type} = erldantic_type_info:get_type(TypeInfo, TypeName, TypeArity),
    to_string(TypeInfo, Type, Data);
to_string(_TypeInfo, {record, RecordName}, Data) when is_atom(RecordName) ->
    {error,
     [#ed_error{type = no_match,
                location = [],
                ctx = #{type => {record, RecordName}, value => Data}}]};
to_string(_TypeInfo, #ed_simple_type{type = NotSupported} = T, _Data)
    when NotSupported =:= pid
         orelse NotSupported =:= port
         orelse NotSupported =:= reference
         orelse NotSupported =:= bitstring
         orelse NotSupported =:= nonempty_bitstring
         orelse NotSupported =:= none ->
    erlang:error({type_not_supported, T});
to_string(_TypeInfo, #ed_simple_type{type = PrimaryType}, Data) ->
    convert_type_to_string(PrimaryType, Data);
to_string(_TypeInfo,
          #ed_range{type = integer,
                    lower_bound = Min,
                    upper_bound = Max} =
              Range,
          Data) ->
    case convert_type_to_string(integer, Data) of
        {ok, String} when Min =< Data, Data =< Max ->
            {ok, String};
        {ok, _String} when is_integer(Data) ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => Range, value => Data}}]};
        {error, Reason} ->
            {error, Reason}
    end;
to_string(_TypeInfo, #ed_remote_type{mfargs = {Module, TypeName, Args}}, Data) ->
    TypeInfo = erldantic_module_types:get(Module),
    TypeArity = length(Args),
    {ok, Type} = erldantic_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
    to_string(TypeInfo, TypeWithoutVars, Data);
to_string(_TypeInfo, #ed_literal{value = Literal}, Data) ->
    try_convert_literal_to_string(Literal, Data);
to_string(TypeInfo, #ed_union{} = Type, Data) ->
    union_to_string(TypeInfo, Type, Data);
to_string(_TypeInfo, Type, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => Type, value => Data}}]}.

%% INTERNAL
convert_string_to_type(Type, String) when is_atom(Type), is_list(String) ->
    do_convert_string_to_type(Type, String);
convert_string_to_type(Type, NotString) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => Type, value => NotString}}]}.

-spec do_convert_string_to_type(Type :: atom(), String :: string()) ->
                                   {ok, term()} | {error, [erldantic:error()]}.
do_convert_string_to_type(integer, String) ->
    try
        {ok, list_to_integer(String)}
    catch
        error:badarg ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => #ed_simple_type{type = integer}, value => String}}]}
    end;
do_convert_string_to_type(float, String) ->
    try
        {ok, list_to_float(String)}
    catch
        error:badarg ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => #ed_simple_type{type = float}, value => String}}]}
    end;
do_convert_string_to_type(number, String) ->
    case do_convert_string_to_type(integer, String) of
        {ok, _} = Result ->
            Result;
        {error, _} ->
            do_convert_string_to_type(float, String)
    end;
do_convert_string_to_type(boolean, "true") ->
    {ok, true};
do_convert_string_to_type(boolean, "false") ->
    {ok, false};
do_convert_string_to_type(boolean, String) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = boolean}, value => String}}]};
do_convert_string_to_type(atom, String) ->
    try
        {ok, list_to_existing_atom(String)}
    catch
        error:badarg ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [],
                        ctx = #{type => #ed_simple_type{type = atom}, value => String}}]}
    end;
do_convert_string_to_type(string, String) ->
    {ok, String};
do_convert_string_to_type(nonempty_string, String) when String =/= [] ->
    {ok, String};
do_convert_string_to_type(nonempty_string, []) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = nonempty_string}, value => []}}]};
do_convert_string_to_type(binary, String) ->
    {ok, list_to_binary(String)};
do_convert_string_to_type(nonempty_binary, String) when String =/= [] ->
    {ok, list_to_binary(String)};
do_convert_string_to_type(nonempty_binary, []) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = nonempty_binary}, value => []}}]};
do_convert_string_to_type(non_neg_integer, String) ->
    case do_convert_string_to_type(integer, String) of
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
do_convert_string_to_type(pos_integer, String) ->
    case do_convert_string_to_type(integer, String) of
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
do_convert_string_to_type(neg_integer, String) ->
    case do_convert_string_to_type(integer, String) of
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
do_convert_string_to_type(Type, String) ->
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

convert_type_to_string(integer, Data) when is_integer(Data) ->
    {ok, integer_to_list(Data)};
convert_type_to_string(integer, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = integer}, value => Data}}]};
convert_type_to_string(float, Data) when is_float(Data) ->
    {ok, float_to_list(Data)};
convert_type_to_string(float, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = float}, value => Data}}]};
convert_type_to_string(number, Data) when is_number(Data) ->
    if is_integer(Data) ->
           {ok, integer_to_list(Data)};
       is_float(Data) ->
           {ok, float_to_list(Data)}
    end;
convert_type_to_string(number, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = number}, value => Data}}]};
convert_type_to_string(boolean, true) ->
    {ok, "true"};
convert_type_to_string(boolean, false) ->
    {ok, "false"};
convert_type_to_string(boolean, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = boolean}, value => Data}}]};
convert_type_to_string(atom, Data) when is_atom(Data) ->
    {ok, atom_to_list(Data)};
convert_type_to_string(atom, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = atom}, value => Data}}]};
convert_type_to_string(string, Data) when is_list(Data) ->
    lits_to_charlist(Data);
convert_type_to_string(string, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = string}, value => Data}}]};
convert_type_to_string(nonempty_string, Data) when is_list(Data), Data =/= [] ->
    lits_to_charlist(Data);
convert_type_to_string(nonempty_string, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = nonempty_string}, value => Data}}]};
convert_type_to_string(binary, Data) when is_binary(Data) ->
    {ok, binary_to_list(Data)};
convert_type_to_string(binary, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = binary}, value => Data}}]};
convert_type_to_string(nonempty_binary, Data) when is_binary(Data), Data =/= <<>> ->
    {ok, binary_to_list(Data)};
convert_type_to_string(nonempty_binary, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = nonempty_binary}, value => Data}}]};
convert_type_to_string(non_neg_integer, Data) when is_integer(Data), Data >= 0 ->
    {ok, integer_to_list(Data)};
convert_type_to_string(non_neg_integer, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = non_neg_integer}, value => Data}}]};
convert_type_to_string(pos_integer, Data) when is_integer(Data), Data > 0 ->
    {ok, integer_to_list(Data)};
convert_type_to_string(pos_integer, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = pos_integer}, value => Data}}]};
convert_type_to_string(neg_integer, Data) when is_integer(Data), Data < 0 ->
    {ok, integer_to_list(Data)};
convert_type_to_string(neg_integer, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_simple_type{type = neg_integer}, value => Data}}]};
convert_type_to_string(Type, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => Type, value => Data}}]}.

lits_to_charlist(Data) ->
    case application:get_env(erldantic, check_unicode, false) of
        true ->
            case unicode:characters_to_list(Data) of
                DataList when is_list(DataList) ->
                    {ok, DataList};
                _Other ->
                    {error,
                     [#ed_error{type = type_mismatch,
                                location = [],
                                ctx = #{type => #ed_simple_type{type = string}, value => Data}}]}
            end;
        false ->
            {ok, Data}
    end.

-spec try_convert_literal_to_string(Literal :: term(), Data :: term()) ->
                                       {ok, string()} | {error, [erldantic:error()]}.
try_convert_literal_to_string(Literal, Literal) when is_atom(Literal) ->
    {ok, atom_to_list(Literal)};
try_convert_literal_to_string(Literal, Literal) when is_integer(Literal) ->
    {ok, integer_to_list(Literal)};
try_convert_literal_to_string(Literal, Literal) when is_boolean(Literal) ->
    if Literal ->
           {ok, "true"};
       true ->
           {ok, "false"}
    end;
try_convert_literal_to_string(Literal, Data) ->
    {error,
     [#ed_error{type = type_mismatch,
                location = [],
                ctx = #{type => #ed_literal{value = Literal}, value => Data}}]}.

union_to_string(TypeInfo, #ed_union{types = Types} = T, Data) ->
    case do_first_to_string(TypeInfo, Types, Data) of
        {error, no_match} ->
            {error,
             [#ed_error{type = no_match,
                        location = [],
                        ctx = #{type => T, value => Data}}]};
        Result ->
            Result
    end.

do_first_to_string(_TypeInfo, [], _Data) ->
    {error, no_match};
do_first_to_string(TypeInfo, [Type | Rest], Data) ->
    case to_string(TypeInfo, Type, Data) of
        {ok, Result} ->
            {ok, Result};
        {error, _} ->
            do_first_to_string(TypeInfo, Rest, Data)
    end.
