-module(spectra_binary_string).

-export([from_binary_string/3, to_binary_string/3]).

-ignore_xref([
    {spectra_binary_string, from_binary_string, 3},
    {spectra_binary_string, to_binary_string, 3}
]).

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

%% API

-doc """
Converts a binary string value to an Erlang value based on a type specification.

This function validates the given binary string value against the specified type definition
and converts it to the corresponding Erlang value.

### Returns
{ok, ErlangValue} if conversion succeeds, or {error, Errors} if validation fails
""".
-doc #{
    params =>
        #{
            "BinaryString" => "The binary string value to convert to Erlang format",
            "Type" => "The type specification (spectra:sp_type_or_ref())",
            "TypeInfo" => "The type information containing type definitions"
        }
}.

-spec from_binary_string(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type_or_ref(),
    BinaryString :: binary()
) ->
    {ok, term()} | {error, [spectra:error()]}.
from_binary_string(TypeInfo, {type, TypeName, TypeArity}, BinaryString) when
    is_atom(TypeName)
->
    {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    from_binary_string(TypeInfo, Type, BinaryString);
from_binary_string(_TypeInfo, {record, RecordName}, BinaryString) when
    is_atom(RecordName)
->
    {error, [
        #sp_error{
            type = no_match,
            location = [],
            ctx = #{type => {record, RecordName}, value => BinaryString}
        }
    ]};
from_binary_string(_TypeInfo, #sp_simple_type{type = NotSupported} = T, _BinaryString) when
    NotSupported =:= pid orelse
        NotSupported =:= port orelse
        NotSupported =:= reference orelse
        NotSupported =:= bitstring orelse
        NotSupported =:= nonempty_bitstring orelse
        NotSupported =:= none
->
    erlang:error({type_not_supported, T});
from_binary_string(_TypeInfo, #sp_simple_type{type = PrimaryType}, BinaryString) ->
    convert_binary_string_to_type(PrimaryType, BinaryString);
from_binary_string(
    _TypeInfo,
    #sp_range{
        type = integer,
        lower_bound = Min,
        upper_bound = Max
    } =
        Range,
    BinaryString
) ->
    case convert_binary_string_to_type(integer, BinaryString) of
        {ok, Value} when Min =< Value, Value =< Max ->
            {ok, Value};
        {ok, Value} when is_integer(Value) ->
            {error, [
                #sp_error{
                    type = type_mismatch,
                    location = [],
                    ctx = #{type => Range, value => Value}
                }
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
from_binary_string(
    _TypeInfo,
    #sp_remote_type{mfargs = {Module, TypeName, Args}},
    BinaryString
) ->
    TypeInfo = spectra_module_types:get(Module),
    TypeArity = length(Args),
    {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
    from_binary_string(TypeInfo, TypeWithoutVars, BinaryString);
from_binary_string(_TypeInfo, #sp_literal{value = Literal}, BinaryString) ->
    try_convert_binary_string_to_literal(Literal, BinaryString);
from_binary_string(TypeInfo, #sp_union{} = Type, BinaryString) ->
    union(fun from_binary_string/3, TypeInfo, Type, BinaryString);
from_binary_string(_TypeInfo, Type, BinaryString) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => Type, value => BinaryString}
        }
    ]}.

-doc """
Converts an Erlang value to a binary string based on a type specification.

This function validates the given Erlang value against the specified type definition
and converts it to a binary string representation.

### Returns
{ok, BinaryString} if conversion succeeds, or {error, Errors} if validation fails
""".
-doc #{
    params =>
        #{
            "Data" => "The Erlang value to convert to binary string format",
            "Type" => "The type specification (spectra:sp_type_or_ref())",
            "TypeInfo" => "The type information containing type definitions"
        }
}.

-spec to_binary_string(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type_or_ref(),
    Data :: term()
) ->
    {ok, binary()} | {error, [spectra:error()]}.
to_binary_string(TypeInfo, {type, TypeName, TypeArity}, Data) when is_atom(TypeName) ->
    {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    to_binary_string(TypeInfo, Type, Data);
to_binary_string(_TypeInfo, {record, RecordName}, Data) when is_atom(RecordName) ->
    {error, [
        #sp_error{
            type = no_match,
            location = [],
            ctx = #{type => {record, RecordName}, value => Data}
        }
    ]};
to_binary_string(_TypeInfo, #sp_simple_type{type = NotSupported} = T, _Data) when
    NotSupported =:= pid orelse
        NotSupported =:= port orelse
        NotSupported =:= reference orelse
        NotSupported =:= bitstring orelse
        NotSupported =:= nonempty_bitstring orelse
        NotSupported =:= none
->
    erlang:error({type_not_supported, T});
to_binary_string(_TypeInfo, #sp_simple_type{type = PrimaryType}, Data) ->
    convert_type_to_binary_string(PrimaryType, Data);
to_binary_string(
    _TypeInfo,
    #sp_range{
        type = integer,
        lower_bound = Min,
        upper_bound = Max
    } =
        Range,
    Data
) ->
    case convert_type_to_binary_string(integer, Data) of
        {ok, BinaryString} when Min =< Data, Data =< Max ->
            {ok, BinaryString};
        {ok, _BinaryString} when is_integer(Data) ->
            {error, [
                #sp_error{
                    type = type_mismatch,
                    location = [],
                    ctx = #{type => Range, value => Data}
                }
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
to_binary_string(_TypeInfo, #sp_remote_type{mfargs = {Module, TypeName, Args}}, Data) ->
    TypeInfo = spectra_module_types:get(Module),
    TypeArity = length(Args),
    {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
    to_binary_string(TypeInfo, TypeWithoutVars, Data);
to_binary_string(_TypeInfo, #sp_literal{value = Literal}, Data) ->
    try_convert_literal_to_binary_string(Literal, Data);
to_binary_string(TypeInfo, #sp_union{} = Type, Data) ->
    union_to_binary_string(TypeInfo, Type, Data);
to_binary_string(_TypeInfo, Type, Data) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => Type, value => Data}
        }
    ]}.

%% INTERNAL

convert_binary_string_to_type(Type, BinaryString) when is_binary(BinaryString) ->
    do_convert_binary_string_to_type(Type, BinaryString);
convert_binary_string_to_type(Type, NonBinary) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => Type, value => NonBinary}
        }
    ]}.

-spec do_convert_binary_string_to_type(Type :: atom(), BinaryString :: binary()) ->
    {ok, term()} | {error, [spectra:error()]}.
do_convert_binary_string_to_type(integer, BinaryString) ->
    try
        {ok, binary_to_integer(BinaryString)}
    catch
        error:badarg ->
            {error, [
                #sp_error{
                    type = type_mismatch,
                    location = [],
                    ctx = #{type => #sp_simple_type{type = integer}, value => BinaryString}
                }
            ]}
    end;
do_convert_binary_string_to_type(float, BinaryString) ->
    try
        {ok, binary_to_float(BinaryString)}
    catch
        error:badarg ->
            {error, [
                #sp_error{
                    type = type_mismatch,
                    location = [],
                    ctx = #{type => #sp_simple_type{type = float}, value => BinaryString}
                }
            ]}
    end;
do_convert_binary_string_to_type(number, BinaryString) ->
    case do_convert_binary_string_to_type(integer, BinaryString) of
        {ok, _} = Result ->
            Result;
        {error, _} ->
            do_convert_binary_string_to_type(float, BinaryString)
    end;
do_convert_binary_string_to_type(boolean, <<"true">>) ->
    {ok, true};
do_convert_binary_string_to_type(boolean, <<"false">>) ->
    {ok, false};
do_convert_binary_string_to_type(boolean, BinaryString) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_simple_type{type = boolean}, value => BinaryString}
        }
    ]};
do_convert_binary_string_to_type(atom, BinaryString) ->
    try
        {ok, binary_to_existing_atom(BinaryString, utf8)}
    catch
        error:badarg ->
            {error, [
                #sp_error{
                    type = type_mismatch,
                    location = [],
                    ctx = #{type => #sp_simple_type{type = atom}, value => BinaryString}
                }
            ]}
    end;
do_convert_binary_string_to_type(string, BinaryString) ->
    {ok, binary_to_list(BinaryString)};
do_convert_binary_string_to_type(nonempty_string, BinaryString) when
    BinaryString =/= <<>>
->
    {ok, binary_to_list(BinaryString)};
do_convert_binary_string_to_type(nonempty_string, <<>>) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_simple_type{type = nonempty_string}, value => <<>>}
        }
    ]};
do_convert_binary_string_to_type(binary, BinaryString) ->
    {ok, BinaryString};
do_convert_binary_string_to_type(nonempty_binary, BinaryString) when
    BinaryString =/= <<>>
->
    {ok, BinaryString};
do_convert_binary_string_to_type(nonempty_binary, <<>>) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_simple_type{type = nonempty_binary}, value => <<>>}
        }
    ]};
do_convert_binary_string_to_type(non_neg_integer, BinaryString) ->
    case do_convert_binary_string_to_type(integer, BinaryString) of
        {ok, Value} when Value >= 0 ->
            {ok, Value};
        {ok, Value} ->
            {error, [
                #sp_error{
                    type = type_mismatch,
                    location = [],
                    ctx = #{type => #sp_simple_type{type = non_neg_integer}, value => Value}
                }
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
do_convert_binary_string_to_type(pos_integer, BinaryString) ->
    case do_convert_binary_string_to_type(integer, BinaryString) of
        {ok, Value} when Value > 0 ->
            {ok, Value};
        {ok, Value} ->
            {error, [
                #sp_error{
                    type = type_mismatch,
                    location = [],
                    ctx = #{type => #sp_simple_type{type = pos_integer}, value => Value}
                }
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
do_convert_binary_string_to_type(neg_integer, BinaryString) ->
    case do_convert_binary_string_to_type(integer, BinaryString) of
        {ok, Value} when Value < 0 ->
            {ok, Value};
        {ok, Value} ->
            {error, [
                #sp_error{
                    type = type_mismatch,
                    location = [],
                    ctx = #{type => #sp_simple_type{type = neg_integer}, value => Value}
                }
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
do_convert_binary_string_to_type(Type, BinaryString) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => Type, value => BinaryString}
        }
    ]}.

-spec try_convert_binary_string_to_literal(Literal :: term(), BinaryString :: binary()) ->
    {ok, term()} | {error, [spectra:error()]}.
try_convert_binary_string_to_literal(Literal, BinaryString) when is_atom(Literal) ->
    case convert_binary_string_to_type(atom, BinaryString) of
        {ok, Literal} ->
            {ok, Literal};
        {ok, _Other} ->
            {error, [
                #sp_error{
                    type = type_mismatch,
                    location = [],
                    ctx = #{type => #sp_literal{value = Literal}, value => BinaryString}
                }
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
try_convert_binary_string_to_literal(Literal, BinaryString) when is_integer(Literal) ->
    case convert_binary_string_to_type(integer, BinaryString) of
        {ok, Literal} ->
            {ok, Literal};
        {ok, _Other} ->
            {error, [
                #sp_error{
                    type = type_mismatch,
                    location = [],
                    ctx = #{type => #sp_literal{value = Literal}, value => BinaryString}
                }
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
try_convert_binary_string_to_literal(Literal, BinaryString) when is_boolean(Literal) ->
    case convert_binary_string_to_type(boolean, BinaryString) of
        {ok, Literal} ->
            {ok, Literal};
        {ok, _Other} ->
            {error, [
                #sp_error{
                    type = type_mismatch,
                    location = [],
                    ctx = #{type => #sp_literal{value = Literal}, value => BinaryString}
                }
            ]};
        {error, Reason} ->
            {error, Reason}
    end;
try_convert_binary_string_to_literal(Literal, BinaryString) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_literal{value = Literal}, value => BinaryString}
        }
    ]}.

union(Fun, TypeInfo, #sp_union{types = Types} = T, BinaryString) ->
    case do_first(Fun, TypeInfo, Types, BinaryString) of
        {error, no_match} ->
            {error, [
                #sp_error{
                    type = no_match,
                    location = [],
                    ctx = #{type => T, value => BinaryString}
                }
            ]};
        Result ->
            Result
    end.

do_first(_Fun, _TypeInfo, [], _BinaryString) ->
    {error, no_match};
do_first(Fun, TypeInfo, [Type | Rest], BinaryString) ->
    case Fun(TypeInfo, Type, BinaryString) of
        {ok, Result} ->
            {ok, Result};
        {error, _} ->
            do_first(Fun, TypeInfo, Rest, BinaryString)
    end.

apply_args(TypeInfo, Type, TypeArgs) when is_list(TypeArgs) ->
    ArgNames = arg_names(Type),
    NamedTypes =
        maps:from_list(
            lists:zip(ArgNames, TypeArgs)
        ),
    type_replace_vars(TypeInfo, Type, NamedTypes).

arg_names(#sp_type_with_variables{vars = Args}) ->
    Args;
arg_names(_) ->
    [].

-spec type_replace_vars(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type(),
    NamedTypes :: #{atom() => spectra:sp_type()}
) ->
    spectra:sp_type().
type_replace_vars(_TypeInfo, #sp_var{name = Name}, NamedTypes) ->
    maps:get(Name, NamedTypes, #sp_simple_type{type = term});
type_replace_vars(TypeInfo, #sp_type_with_variables{type = Type}, NamedTypes) ->
    case Type of
        #sp_union{types = UnionTypes} ->
            #sp_union{
                types =
                    lists:map(
                        fun(UnionType) ->
                            type_replace_vars(TypeInfo, UnionType, NamedTypes)
                        end,
                        UnionTypes
                    )
            };
        #sp_remote_type{mfargs = {Module, TypeName, Args}} ->
            TypeInfo = spectra_module_types:get(Module),
            TypeArity = length(Args),
            {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
            type_replace_vars(TypeInfo, Type, NamedTypes)
    end;
type_replace_vars(_TypeInfo, Type, _NamedTypes) ->
    Type.

convert_type_to_binary_string(integer, Data) when is_integer(Data) ->
    {ok, integer_to_binary(Data)};
convert_type_to_binary_string(integer, Data) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_simple_type{type = integer}, value => Data}
        }
    ]};
convert_type_to_binary_string(float, Data) when is_float(Data) ->
    {ok, float_to_binary(Data)};
convert_type_to_binary_string(float, Data) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_simple_type{type = float}, value => Data}
        }
    ]};
convert_type_to_binary_string(number, Data) when is_number(Data) ->
    if
        is_integer(Data) ->
            {ok, integer_to_binary(Data)};
        is_float(Data) ->
            {ok, float_to_binary(Data)}
    end;
convert_type_to_binary_string(number, Data) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_simple_type{type = number}, value => Data}
        }
    ]};
convert_type_to_binary_string(boolean, true) ->
    {ok, <<"true">>};
convert_type_to_binary_string(boolean, false) ->
    {ok, <<"false">>};
convert_type_to_binary_string(boolean, Data) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_simple_type{type = boolean}, value => Data}
        }
    ]};
convert_type_to_binary_string(atom, Data) when is_atom(Data) ->
    {ok, atom_to_binary(Data, utf8)};
convert_type_to_binary_string(atom, Data) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_simple_type{type = atom}, value => Data}
        }
    ]};
convert_type_to_binary_string(string, Data) when is_list(Data) ->
    case unicode:characters_to_binary(Data) of
        DataBinary when is_binary(DataBinary) ->
            {ok, DataBinary};
        _Other ->
            {error, [
                #sp_error{
                    type = type_mismatch,
                    location = [],
                    ctx = #{type => #sp_simple_type{type = string}, value => Data}
                }
            ]}
    end;
convert_type_to_binary_string(string, Data) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_simple_type{type = string}, value => Data}
        }
    ]};
convert_type_to_binary_string(nonempty_string, Data) when is_list(Data), Data =/= [] ->
    case unicode:characters_to_binary(Data) of
        DataBinary when is_binary(DataBinary) ->
            {ok, DataBinary};
        _Other ->
            {error, [
                #sp_error{
                    type = type_mismatch,
                    location = [],
                    ctx = #{type => #sp_simple_type{type = nonempty_string}, value => Data}
                }
            ]}
    end;
convert_type_to_binary_string(nonempty_string, Data) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_simple_type{type = nonempty_string}, value => Data}
        }
    ]};
convert_type_to_binary_string(binary, Data) when is_binary(Data) ->
    {ok, Data};
convert_type_to_binary_string(binary, Data) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_simple_type{type = binary}, value => Data}
        }
    ]};
convert_type_to_binary_string(nonempty_binary, Data) when
    is_binary(Data), Data =/= <<>>
->
    {ok, Data};
convert_type_to_binary_string(nonempty_binary, Data) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_simple_type{type = nonempty_binary}, value => Data}
        }
    ]};
convert_type_to_binary_string(non_neg_integer, Data) when is_integer(Data), Data >= 0 ->
    {ok, integer_to_binary(Data)};
convert_type_to_binary_string(non_neg_integer, Data) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_simple_type{type = non_neg_integer}, value => Data}
        }
    ]};
convert_type_to_binary_string(pos_integer, Data) when is_integer(Data), Data > 0 ->
    {ok, integer_to_binary(Data)};
convert_type_to_binary_string(pos_integer, Data) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_simple_type{type = pos_integer}, value => Data}
        }
    ]};
convert_type_to_binary_string(neg_integer, Data) when is_integer(Data), Data < 0 ->
    {ok, integer_to_binary(Data)};
convert_type_to_binary_string(neg_integer, Data) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_simple_type{type = neg_integer}, value => Data}
        }
    ]};
convert_type_to_binary_string(Type, Data) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => Type, value => Data}
        }
    ]}.

-spec try_convert_literal_to_binary_string(Literal :: term(), Data :: term()) ->
    {ok, binary()} | {error, [spectra:error()]}.
try_convert_literal_to_binary_string(Literal, Literal) when is_atom(Literal) ->
    {ok, atom_to_binary(Literal, utf8)};
try_convert_literal_to_binary_string(Literal, Literal) when is_integer(Literal) ->
    {ok, integer_to_binary(Literal)};
try_convert_literal_to_binary_string(Literal, Literal) when is_boolean(Literal) ->
    if
        Literal ->
            {ok, <<"true">>};
        true ->
            {ok, <<"false">>}
    end;
try_convert_literal_to_binary_string(Literal, Data) ->
    {error, [
        #sp_error{
            type = type_mismatch,
            location = [],
            ctx = #{type => #sp_literal{value = Literal}, value => Data}
        }
    ]}.

union_to_binary_string(TypeInfo, #sp_union{types = Types} = T, Data) ->
    case do_first_to_binary_string(TypeInfo, Types, Data) of
        {error, no_match} ->
            {error, [
                #sp_error{
                    type = no_match,
                    location = [],
                    ctx = #{type => T, value => Data}
                }
            ]};
        Result ->
            Result
    end.

do_first_to_binary_string(_TypeInfo, [], _Data) ->
    {error, no_match};
do_first_to_binary_string(TypeInfo, [Type | Rest], Data) ->
    case to_binary_string(TypeInfo, Type, Data) of
        {ok, Result} ->
            {ok, Result};
        {error, _} ->
            do_first_to_binary_string(TypeInfo, Rest, Data)
    end.
