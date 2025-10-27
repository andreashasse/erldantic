-module(erldantic).

-export([decode/4, encode/4, schema/3]).

-ignore_xref([decode/4, encode/4, schema/3]).

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-type type_info() :: erldantic_type_info:type_info().
-type var_type() :: {VarName :: atom(), ed_type()}.
-type user_type_name() :: atom().
-type record_field() :: {FieldName :: atom(), ed_type()}.
%% FIXME: Add doc here.
%% iolist and iodata are aliases, but are so complex, so it is easier to handle them as separate types
-type ed_type() ::
    #ed_simple_type{}
    | #ed_rec_ref{}
    | #ed_user_type_ref{}
    | #ed_var{}
    | #ed_map{}
    | #ed_rec{}
    | #ed_tuple{}
    | #ed_type_with_variables{}
    | #ed_function{}
    | #ed_union{}
    | #ed_literal{}
    | #ed_range{}
    | #ed_list{}
    | #ed_nonempty_list{}
    | #ed_maybe_improper_list{}
    | #ed_nonempty_improper_list{}
    | #ed_remote_type{}.
-type map_field() ::
    {map_field_assoc | map_field_exact, Name :: atom(), ed_type()}
    | {map_field_type_assoc | map_field_type_exact, ed_type(), ed_type()}.
-type ed_type_reference() ::
    {type, Name :: atom(), Arity :: arity()} | {record, Name :: atom()}.
-type error() :: #ed_error{}.
-type ed_type_or_ref() :: ed_type() | ed_type_reference().
-type ed_function_spec() :: #ed_function_spec{}.

%% Internal type definitions moved from erldantic_internal.hrl

-export_type([
    ed_type/0,
    ed_type_reference/0,
    ed_type_or_ref/0,
    var_type/0,
    type_info/0,
    record_field/0,
    error/0,
    map_field/0,
    user_type_name/0,
    ed_function_spec/0
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

1> erldantic:decode(json, my_module, user_id, <<"123">>).
{ok, 123}

2> erldantic:decode(json, my_module, user, <<"{\"id\":42,\"name\":\"Bob\",\"age\":25, \"status\":\"active\"}">>).
{ok, #user{id = 42, name = <<"Bob">>, age = 25, status = active}}

3> erldantic:decode(binary_string, my_module, status, <<"active">>).
{ok, active}

4> erldantic:decode(json, my_module, user_id, <<"\"not_a_number\"">>).
{error, [#ed_error{type = type_mismatch, ...}]}
```
""".
-spec decode(
    Format :: atom(),
    ModuleOrTypeinfo :: module() | type_info(),
    TypeOrRef :: atom() | ed_type_or_ref(),
    Binary :: any()
) ->
    {ok, term()} | {error, [error()]}.
decode(Format, Module, TypeOrRef, Binary) when is_atom(Module) ->
    TypeInfo = erldantic_module_types:get(Module),
    decode(Format, TypeInfo, TypeOrRef, Binary);
decode(Format, TypeInfo, RefAtom, Binary) when is_atom(RefAtom) ->
    Type = get_type_from_atom(TypeInfo, RefAtom),
    decode(Format, TypeInfo, Type, Binary);
decode(json, Typeinfo, TypeOrRef, Binary) when is_binary(Binary) ->
    case json_decode(Binary) of
        {ok, DecodedJson} ->
            erldantic_json:from_json(Typeinfo, TypeOrRef, DecodedJson);
        {error, _} = Err ->
            Err
    end;
decode(binary_string, Typeinfo, TypeOrRef, Binary) when is_binary(Binary) ->
    erldantic_binary_string:from_binary_string(Typeinfo, TypeOrRef, Binary);
decode(string, Typeinfo, TypeOrRef, String) when is_list(String) ->
    erldantic_string:from_string(Typeinfo, TypeOrRef, String).

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

1> erldantic:encode(json, my_module, user_id, 123).
{ok, <<"123">>}

2> User = #user{id = 42, name = <<"Bob">>, age = 25, status = active}.
3> erldantic:encode(json, my_module, user, User).
{ok, <<"{\"id\":42,\"name\":\"Bob\",\"age\":25, \"status\":\"active\"}">>}

4> erldantic:encode(json, my_module, user_id, -5).
{error, [#ed_error{type = type_mismatch, ...}]}
```
""".
-spec encode(
    Format :: atom(),
    ModuleOrTypeinfo :: module() | type_info(),
    TypeOrRef :: atom() | ed_type_or_ref(),
    Binary :: any()
) ->
    {ok, term()} | {error, [error()]}.
encode(Format, Module, TypeOrRef, Data) when is_atom(Module) ->
    TypeInfo = erldantic_module_types:get(Module),
    encode(Format, TypeInfo, TypeOrRef, Data);
encode(Format, Module, TypeAtom, Data) when is_atom(TypeAtom) ->
    Type = get_type_from_atom(Module, TypeAtom),
    encode(Format, Module, Type, Data);
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

-doc """
Generates a schema for the specified type in the given format.

### Example:

```
-module(my_module).
-type user_id() :: pos_integer().
-type status() :: active | inactive | pending.
-record(user, {id :: user_id(), name :: binary(), age :: integer(), status :: status()}).

1> erldantic:schema(json_schema, my_module, user).
{ok, <<"{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"integer\"},...}}">>}

2> erldantic:schema(json_schema, my_module, status).
{ok, <<"{\"oneOf\":[{\"enum\":[\"active\"]},{\"enum\":[\"inactive\"]},{\"enum\":[\"pending\"]}]}">>}

3> erldantic:schema(json_schema, my_module, {type, user_id, 0}).
{ok, <<"{\"type\":\"integer\",\"minimum\":1}">>}

4> erldantic:schema(invalid_format, my_module, user).
{error, [#ed_error{...}]}
```
""".
-spec schema(
    Format :: atom(),
    ModuleOrTypeinfo :: module() | type_info(),
    TypeOrRef :: atom() | ed_type_or_ref()
) ->
    {ok, iodata()} | {error, [error()]}.
schema(Format, Module, TypeOrRef) when is_atom(Module) ->
    TypeInfo = erldantic_module_types:get(Module),
    schema(Format, TypeInfo, TypeOrRef);
schema(Format, TypeInfo, TypeAtom) when is_atom(TypeAtom) ->
    Type = get_type_from_atom(TypeInfo, TypeAtom),
    schema(Format, TypeInfo, Type);
schema(json_schema, Module, TypeOrRef) ->
    case erldantic_json_schema:to_schema(Module, TypeOrRef) of
        {ok, SchemaMap} ->
            {ok, json:encode(SchemaMap)};
        {error, _} = Err ->
            Err
    end.

get_type_from_atom(TypeInfo, RefAtom) ->
    case erldantic_type_info:get_type(TypeInfo, RefAtom, 0) of
        {ok, Type} ->
            Type;
        error ->
            case erldantic_type_info:get_record(TypeInfo, RefAtom) of
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
                #ed_error{
                    location = [],
                    type = decode_error,
                    ctx = #{type => ErrType, reason => Reason}
                }
            ]}
    end.
