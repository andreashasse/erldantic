-module(erldantic).

-export([decode/4, encode/4]).

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-type type_info() :: erldantic_type_info:type_info().
-type var_type() :: {VarName :: atom(), ed_type()}.
-type user_type_name() :: atom().
-type record_field() :: {FieldName :: atom(), ed_type()}.
%% FIXME: Add doc here.
%% iolist and iodata are aliases, but are so complex, so it is easier to handle them as separate types
-type ed_type() ::
    #ed_simple_type{} |
    #ed_rec_ref{} |
    #ed_user_type_ref{} |
    #ed_var{} |
    #ed_map{} |
    #ed_rec{} |
    #ed_tuple{} |
    #ed_type_with_variables{} |
    #ed_function{} |
    #ed_union{} |
    #ed_literal{} |
    #ed_range{} |
    #ed_list{} |
    #ed_nonempty_list{} |
    #ed_maybe_improper_list{} |
    #ed_nonempty_improper_list{} |
    #ed_remote_type{}.
-type map_field() ::
    {map_field_assoc | map_field_exact, Name :: atom(), ed_type()} |
    {map_field_type_assoc | map_field_type_exact, ed_type(), ed_type()}.
-type ed_type_reference() ::
    {type, Name :: atom(), Arity :: arity()} | {record, Name :: atom()}.
-type error() :: #ed_error{}.
-type ed_type_or_ref() :: ed_type() | ed_type_reference().
-type ed_function_spec() :: #ed_function_spec{}.

%% Internal type definitions moved from erldantic_internal.hrl

-export_type([ed_type/0, ed_type_reference/0, ed_type_or_ref/0, var_type/0, type_info/0,
              record_field/0, error/0, map_field/0, user_type_name/0, ed_function_spec/0]).

-spec decode(Format :: atom(),
             ModuleOrTypeinfo :: module() | type_info(),
             TypeOrRef :: atom() | ed_type_or_ref(),
             Binary :: any()) ->
                {ok, term()} | {error, [error()]}.
decode(Format, Module, TypeOrRef, Binary) when is_atom(Module) ->
    TypeInfo = erldantic_module_types:get(Module),
    decode(Format, TypeInfo, TypeOrRef, Binary);
decode(Format, TypeInfo, RefAtom, Binary) when is_atom(RefAtom) ->
    case find_type_from_atom(TypeInfo, RefAtom) of
        {ok, Type} ->
            decode(Format, TypeInfo, Type, Binary);
        {error, _} = Err ->
            Err
    end;
decode(json, Typeinfo, TypeOrRef, Binary) when is_binary(Binary) ->
    erldantic_json:from_json(Typeinfo, TypeOrRef, json:decode(Binary));
decode(binary_string, Typeinfo, TypeOrRef, Binary) when is_binary(Binary) ->
    erldantic_binary_string:from_binary_string(Typeinfo, TypeOrRef, Binary);
decode(string, Typeinfo, TypeOrRef, String) when is_list(String) ->
    erldantic_string:from_string(Typeinfo, TypeOrRef, String).

-spec encode(Format :: atom(),
             ModuleOrTypeinfo :: module() | type_info(),
             TypeOrRef :: atom() | ed_type_or_ref(),
             Binary :: any()) ->
                {ok, term()} | {error, [error()]}.
encode(Format, Module, TypeOrRef, Data) when is_atom(Module) ->
    TypeInfo = erldantic_module_types:get(Module),
    encode(Format, TypeInfo, TypeOrRef, Data);
encode(Format, Module, TypeAtom, Data) when is_atom(TypeAtom) ->
    case find_type_from_atom(Module, TypeAtom) of
        {ok, Type} ->
            encode(Format, Module, Type, Data);
        {error, _} = Err ->
            Err
    end;
encode(json, Typeinfo, TypeOrRef, Data) ->
    case erldantic_json:to_json(Typeinfo, TypeOrRef, Data) of
        {ok, Json} ->
            {ok, json:encode(Json)};
        {error, _} = Err ->
            Err
    end;
encode(binary_string, Typeinfo, TypeOrRef, Data) ->
    erldantic_binary_string:to_binary_string(Typeinfo, TypeOrRef, Data);
encode(string, Typeinfo, TypeOrRef, Data) ->
    erldantic_string:to_string(Typeinfo, TypeOrRef, Data).

find_type_from_atom(TypeInfo, RefAtom) ->
    case erldantic_type_info:get_type(TypeInfo, RefAtom, 0) of
        {ok, Type} ->
            {ok, Type};
        error ->
            case erldantic_type_info:get_record(TypeInfo, RefAtom) of
                {ok, Rec} ->
                    {ok, Rec};
                error ->
                    %% FIXME: error message
                    {error,
                     [#ed_error{location = [],
                                type = no_match,
                                ctx = RefAtom}]}
            end
    end.
