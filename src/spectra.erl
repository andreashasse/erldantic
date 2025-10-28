-module(spectra).

-export([decode/4, encode/4, schema/3]).

-ignore_xref([decode/4, encode/4, schema/3]).

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

-type type_info() :: spectra_type_info:type_info().
-type var_type() :: {VarName :: atom(), sp_type()}.
-type user_type_name() :: atom().
-type record_field() :: {FieldName :: atom(), sp_type()}.
%% FIXME: Add doc here.
%% iolist and iodata are aliases, but are so complex, so it is easier to handle them as separate types
-type sp_type() ::
    #sp_simple_type{}
    | #sp_rec_ref{}
    | #sp_user_type_ref{}
    | #sp_var{}
    | #sp_map{}
    | #sp_rec{}
    | #sp_tuple{}
    | #sp_type_with_variables{}
    | #sp_function{}
    | #sp_union{}
    | #sp_literal{}
    | #sp_range{}
    | #sp_list{}
    | #sp_nonempty_list{}
    | #sp_maybe_improper_list{}
    | #sp_nonempty_improper_list{}
    | #sp_remote_type{}.
-type map_field() ::
    {map_field_assoc | map_field_exact, Name :: atom(), sp_type()}
    | {map_field_type_assoc | map_field_type_exact, sp_type(), sp_type()}.
-type sp_type_reference() ::
    {type, Name :: atom(), Arity :: arity()} | {record, Name :: atom()}.
-type error() :: #sp_error{}.
-type sp_type_or_ref() :: sp_type() | sp_type_reference().
-type sp_function_spec() :: #sp_function_spec{}.

%% Internal type definitions moved from spectra_internal.hrl

-export_type([
    sp_type/0,
    sp_type_reference/0,
    sp_type_or_ref/0,
    var_type/0,
    type_info/0,
    record_field/0,
    error/0,
    map_field/0,
    user_type_name/0,
    sp_function_spec/0
]).

-doc """
Decodes data from the specified format into an Erlang term based on type information.

The function validates the decoded data against the type specification and returns
an error if the data doesn't match the expected type.

### Example:

```
-module(my_module).
-type user_id() :: pos_integer().
-type status() :: active | inactive | pending.
-record(user, {id :: user_id(), name :: binary(), age :: integer(), status :: status()}).

1> spectra:decode(json, my_module, user_id, <<"123">>).
{ok, 123}

2> spectra:decode(json, my_module, user, <<"{\"id\":42,\"name\":\"Bob\",\"age\":25, \"status\":\"active\"}">>).
{ok, #user{id = 42, name = <<"Bob">>, age = 25, status = active}}

3> spectra:decode(binary_string, my_module, status, <<"active">>).
{ok, active}

4> spectra:decode(json, my_module, user_id, <<"\"not_a_number\"">>).
{error, [#sp_error{type = type_mismatch, ...}]}
```
""".
-spec decode(
    Format :: atom(),
    ModuleOrTypeinfo :: module() | type_info(),
    TypeOrRef :: atom() | sp_type_or_ref(),
    Binary :: any()
) ->
    {ok, term()} | {error, [error()]}.
decode(Format, Module, TypeOrRef, Binary) when is_atom(Module) ->
    TypeInfo = spectra_module_types:get(Module),
    decode(Format, TypeInfo, TypeOrRef, Binary);
decode(Format, TypeInfo, RefAtom, Binary) when is_atom(RefAtom) ->
    Type = get_type_from_atom(TypeInfo, RefAtom),
    decode(Format, TypeInfo, Type, Binary);
decode(json, Typeinfo, TypeOrRef, Binary) when is_binary(Binary) ->
    case json_decode(Binary) of
        {ok, DecodedJson} ->
            spectra_json:from_json(Typeinfo, TypeOrRef, DecodedJson);
        {error, _} = Err ->
            Err
    end;
decode(binary_string, Typeinfo, TypeOrRef, Binary) when is_binary(Binary) ->
    spectra_binary_string:from_binary_string(Typeinfo, TypeOrRef, Binary);
decode(string, Typeinfo, TypeOrRef, String) when is_list(String) ->
    spectra_string:from_string(Typeinfo, TypeOrRef, String).

-doc """
Encodes an Erlang term to the specified format based on type information.

The function validates the Erlang term against the type specification before encoding
and returns an error if the data doesn't match the expected type.

### Example:

```
-module(my_module).
-type user_id() :: pos_integer().
-type status() :: active | inactive | pending.
-record(user, {id :: user_id(), name :: binary(), age :: integer(), status :: status()}).

1> spectra:encode(json, my_module, user_id, 123).
{ok, <<"123">>}

2> User = #user{id = 42, name = <<"Bob">>, age = 25, status = active}.
3> spectra:encode(json, my_module, user, User).
{ok, <<"{\"id\":42,\"name\":\"Bob\",\"age\":25, \"status\":\"active\"}">>}

4> spectra:encode(json, my_module, user_id, -5).
{error, [#sp_error{type = type_mismatch, ...}]}
```
""".
-spec encode(
    Format :: atom(),
    ModuleOrTypeinfo :: module() | type_info(),
    TypeOrRef :: atom() | sp_type_or_ref(),
    Binary :: any()
) ->
    {ok, term()} | {error, [error()]}.
encode(Format, Module, TypeOrRef, Data) when is_atom(Module) ->
    TypeInfo = spectra_module_types:get(Module),
    encode(Format, TypeInfo, TypeOrRef, Data);
encode(Format, Module, TypeAtom, Data) when is_atom(TypeAtom) ->
    Type = get_type_from_atom(Module, TypeAtom),
    encode(Format, Module, Type, Data);
encode(json, Typeinfo, TypeOrRef, Data) ->
    case spectra_json:to_json(Typeinfo, TypeOrRef, Data) of
        {ok, Json} ->
            {ok, json:encode(Json)};
        {error, _} = Err ->
            Err
    end;
encode(binary_string, Typeinfo, TypeOrRef, Data) ->
    spectra_binary_string:to_binary_string(Typeinfo, TypeOrRef, Data);
encode(string, Typeinfo, TypeOrRef, Data) ->
    spectra_string:to_string(Typeinfo, TypeOrRef, Data).

-doc """
Generates a schema for the specified type in the given format.

### Example:

```
-module(my_module).
-type user_id() :: pos_integer().
-type status() :: active | inactive | pending.
-record(user, {id :: user_id(), name :: binary(), age :: integer(), status :: status()}).

1> spectra:schema(json_schema, my_module, user).
{ok, <<"{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"integer\"},...}}">>}

2> spectra:schema(json_schema, my_module, status).
{ok, <<"{\"oneOf\":[{\"enum\":[\"active\"]},{\"enum\":[\"inactive\"]},{\"enum\":[\"pending\"]}]}">>}

3> spectra:schema(json_schema, my_module, {type, user_id, 0}).
{ok, <<"{\"type\":\"integer\",\"minimum\":1}">>}

4> spectra:schema(invalid_format, my_module, user).
{error, [#sp_error{...}]}
```
""".
-spec schema(
    Format :: atom(),
    ModuleOrTypeinfo :: module() | type_info(),
    TypeOrRef :: atom() | sp_type_or_ref()
) ->
    {ok, iodata()} | {error, [error()]}.
schema(Format, Module, TypeOrRef) when is_atom(Module) ->
    TypeInfo = spectra_module_types:get(Module),
    schema(Format, TypeInfo, TypeOrRef);
schema(Format, TypeInfo, TypeAtom) when is_atom(TypeAtom) ->
    Type = get_type_from_atom(TypeInfo, TypeAtom),
    schema(Format, TypeInfo, Type);
schema(json_schema, Module, TypeOrRef) ->
    case spectra_json_schema:to_schema(Module, TypeOrRef) of
        {ok, SchemaMap} ->
            {ok, json:encode(SchemaMap)};
        {error, _} = Err ->
            Err
    end.

get_type_from_atom(TypeInfo, RefAtom) ->
    case spectra_type_info:get_type(TypeInfo, RefAtom, 0) of
        {ok, Type} ->
            Type;
        error ->
            case spectra_type_info:get_record(TypeInfo, RefAtom) of
                {ok, Rec} ->
                    Rec;
                error ->
                    erlang:error({type_or_record_not_found, RefAtom})
            end
    end.

json_decode(Binary) ->
    try
        {ok, json:decode(Binary)}
    catch
        ErrType:Reason ->
            {error, [
                #sp_error{
                    location = [],
                    type = decode_error,
                    ctx = #{type => ErrType, reason => Reason}
                }
            ]}
    end.
