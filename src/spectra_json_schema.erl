-module(spectra_json_schema).

-export([to_schema/2]).

-ignore_xref([to_schema/2]).

-include("../include/spectra.hrl").
-include("../include/spectra_internal.hrl").

%% API

-spec to_schema(module() | spectra:type_info(), spectra:sp_type_or_ref()) ->
    {ok, Schema :: map()} | {error, [spectra:error()]}.
to_schema(Module, Type) when is_atom(Module) ->
    TypeInfo = spectra_module_types:get(Module),
    to_schema(TypeInfo, Type);
%% Type references
to_schema(TypeInfo, {type, TypeName, TypeArity}) when is_atom(TypeName) ->
    {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, []),
    do_to_schema(TypeInfo, TypeWithoutVars);
to_schema(TypeInfo, Type) ->
    do_to_schema(TypeInfo, Type).

-spec do_to_schema(
    TypeInfo :: spectra:type_info(),
    Type :: spectra:sp_type_or_ref()
) ->
    {ok, Schema :: map()} | {error, [spectra:error()]}.
%% Simple types
do_to_schema(_TypeInfo, #sp_simple_type{type = integer}) ->
    {ok, #{type => <<"integer">>}};
do_to_schema(_TypeInfo, #sp_simple_type{type = string}) ->
    {ok, #{type => <<"string">>}};
do_to_schema(_TypeInfo, #sp_simple_type{type = iodata}) ->
    {ok, #{type => <<"string">>}};
do_to_schema(_TypeInfo, #sp_simple_type{type = iolist}) ->
    {ok, #{type => <<"string">>}};
do_to_schema(_TypeInfo, #sp_simple_type{type = boolean}) ->
    {ok, #{type => <<"boolean">>}};
do_to_schema(_TypeInfo, #sp_simple_type{type = number}) ->
    {ok, #{type => <<"number">>}};
do_to_schema(_TypeInfo, #sp_simple_type{type = float}) ->
    {ok, #{type => <<"number">>, format => <<"float">>}};
do_to_schema(_TypeInfo, #sp_simple_type{type = atom}) ->
    {ok, #{type => <<"string">>}};
do_to_schema(_TypeInfo, #sp_simple_type{type = binary}) ->
    {ok, #{type => <<"string">>}};
do_to_schema(_TypeInfo, #sp_simple_type{type = nonempty_binary}) ->
    {ok, #{type => <<"string">>, minLength => 1}};
do_to_schema(_TypeInfo, #sp_simple_type{type = nonempty_string}) ->
    {ok, #{type => <<"string">>, minLength => 1}};
do_to_schema(_TypeInfo, #sp_simple_type{type = pos_integer}) ->
    {ok, #{type => <<"integer">>, minimum => 1}};
do_to_schema(_TypeInfo, #sp_simple_type{type = non_neg_integer}) ->
    {ok, #{type => <<"integer">>, minimum => 0}};
do_to_schema(_TypeInfo, #sp_simple_type{type = neg_integer}) ->
    {ok, #{type => <<"integer">>, maximum => -1}};
do_to_schema(_TypeInfo, #sp_simple_type{type = term}) ->
    % any type
    {ok, #{}};
%% Range types
do_to_schema(
    _TypeInfo,
    #sp_range{
        type = integer,
        lower_bound = Min,
        upper_bound = Max
    }
) ->
    {ok, #{
        type => <<"integer">>,
        minimum => Min,
        maximum => Max
    }};
%% Literal types
do_to_schema(_TypeInfo, #sp_literal{value = Value}) when
    Value =:= undefined orelse Value =:= nil
->
    {ok, #{enum => [null]}};
do_to_schema(_TypeInfo, #sp_literal{value = Value, binary_value = BinaryValue}) when
    is_atom(Value)
->
    {ok, #{enum => [BinaryValue]}};
do_to_schema(_TypeInfo, #sp_literal{value = Value}) ->
    {ok, #{enum => [Value]}};
%% List types
do_to_schema(TypeInfo, #sp_list{type = ItemType}) ->
    case do_to_schema(TypeInfo, ItemType) of
        {ok, ItemSchema} ->
            {ok, #{type => <<"array">>, items => ItemSchema}};
        {error, _} = Err ->
            Err
    end;
do_to_schema(TypeInfo, #sp_nonempty_list{type = ItemType}) ->
    case do_to_schema(TypeInfo, ItemType) of
        {ok, ItemSchema} ->
            {ok, #{
                type => <<"array">>,
                items => ItemSchema,
                minItems => 1
            }};
        {error, _} = Err ->
            Err
    end;
%% Union types
do_to_schema(TypeInfo, #sp_union{types = Types}) ->
    case
        lists:partition(
            fun
                (#sp_literal{value = Value}) when Value =:= undefined orelse Value =:= nil ->
                    true;
                (_) ->
                    false
            end,
            Types
        )
    of
        {[_MissingLiteral], [SingleType]} ->
            do_to_schema(TypeInfo, SingleType);
        {[], NonMissingTypes} ->
            case try_generate_enum_schema(NonMissingTypes) of
                {ok, _} = EnumSchema ->
                    EnumSchema;
                not_all_literals ->
                    generate_oneof_schema(TypeInfo, NonMissingTypes)
            end;
        {[_MissingLiteral], OtherTypes} when length(OtherTypes) > 1 ->
            case try_generate_enum_schema(OtherTypes) of
                {ok, _} = EnumSchema ->
                    EnumSchema;
                not_all_literals ->
                    generate_oneof_schema(TypeInfo, Types)
            end
    end;
%% Map types
do_to_schema(TypeInfo, #sp_map{fields = Fields}) ->
    map_fields_to_schema(TypeInfo, Fields);
%% Record types
do_to_schema(TypeInfo, {record, RecordName}) when is_atom(RecordName) ->
    record_to_schema_internal(TypeInfo, RecordName);
do_to_schema(TypeInfo, #sp_rec{} = RecordInfo) ->
    record_to_schema_internal(TypeInfo, RecordInfo);
%% Record references
do_to_schema(TypeInfo, #sp_rec_ref{record_name = RecordName}) ->
    record_to_schema_internal(TypeInfo, RecordName);
%% User type references
do_to_schema(TypeInfo, #sp_user_type_ref{type_name = TypeName, variables = TypeArgs}) ->
    TypeArity = length(TypeArgs),
    {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    do_to_schema(TypeInfo, Type);
%% Remote types
do_to_schema(_TypeInfo, #sp_remote_type{mfargs = {Module, TypeName, Args}}) ->
    TypeInfo = spectra_module_types:get(Module),
    TypeArity = length(Args),
    {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
    do_to_schema(TypeInfo, TypeWithoutVars);
%% Unsupported types
do_to_schema(_TypeInfo, #sp_simple_type{type = NotSupported} = Type) when
    NotSupported =:= pid orelse
        NotSupported =:= port orelse
        NotSupported =:= reference orelse
        NotSupported =:= bitstring orelse
        NotSupported =:= nonempty_bitstring orelse
        NotSupported =:= none
->
    erlang:error({type_not_supported, Type});
do_to_schema(_TypeInfo, #sp_tuple{} = Type) ->
    erlang:error({type_not_supported, Type});
do_to_schema(_TypeInfo, #sp_function{} = Type) ->
    erlang:error({type_not_supported, Type});
%% Fallback
do_to_schema(_TypeInfo, Type) ->
    {error, [
        #sp_error{
            type = no_match,
            location = [],
            ctx = #{type => Type}
        }
    ]}.

%% Helper functions

record_replace_vars(RecordInfo, TypeArgs) ->
    lists:foldl(
        fun({FieldName, Type}, Fields) ->
            lists:map(
                fun
                    (#sp_rec_field{name = Name} = Field) when Name =:= FieldName ->
                        Field#sp_rec_field{type = Type};
                    (Field) ->
                        Field
                end,
                Fields
            )
        end,
        RecordInfo,
        TypeArgs
    ).

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
        #sp_map{fields = Fields} = Map ->
            Map#sp_map{
                fields =
                    lists:map(
                        fun
                            (#literal_map_field{val_type = FieldType} = Field) ->
                                Field#literal_map_field{
                                    val_type = type_replace_vars(TypeInfo, FieldType, NamedTypes)
                                };
                            (#typed_map_field{key_type = KeyType, val_type = ValueType} = Field) ->
                                Field#typed_map_field{
                                    key_type = type_replace_vars(TypeInfo, KeyType, NamedTypes),
                                    val_type = type_replace_vars(TypeInfo, ValueType, NamedTypes)
                                }
                        end,
                        Fields
                    )
            };
        #sp_rec_ref{record_name = RecordName, field_types = RefFieldTypes} ->
            {ok, #sp_rec{fields = Fields} = Rec} =
                spectra_type_info:get_record(TypeInfo, RecordName),
            NewRec = Rec#sp_rec{fields = record_replace_vars(Fields, RefFieldTypes)},
            type_replace_vars(TypeInfo, NewRec, NamedTypes);
        #sp_remote_type{mfargs = {Module, TypeName, Args}} ->
            TypeInfo = spectra_module_types:get(Module),
            TypeArity = length(Args),
            {ok, Type} = spectra_type_info:get_type(TypeInfo, TypeName, TypeArity),
            type_replace_vars(TypeInfo, Type, NamedTypes);
        #sp_list{type = ListType} ->
            #sp_list{type = type_replace_vars(TypeInfo, ListType, NamedTypes)}
    end;
type_replace_vars(_TypeInfo, #sp_rec{fields = Fields} = Rec, NamedTypes) ->
    Rec#sp_rec{
        fields =
            lists:map(
                fun(#sp_rec_field{type = NType} = Field) ->
                    Field#sp_rec_field{
                        type = type_replace_vars(_TypeInfo, NType, NamedTypes)
                    }
                end,
                Fields
            )
    };
type_replace_vars(_TypeInfo, Type, _NamedTypes) ->
    Type.

arg_names(#sp_type_with_variables{vars = Args}) ->
    Args;
arg_names(_) ->
    [].

apply_args(TypeInfo, Type, TypeArgs) when is_list(TypeArgs) ->
    ArgNames = arg_names(Type),
    NamedTypes =
        maps:from_list(
            lists:zip(ArgNames, TypeArgs)
        ),
    type_replace_vars(TypeInfo, Type, NamedTypes).

-spec map_fields_to_schema(spectra:type_info(), [spectra:map_field()]) ->
    {ok, map()} | {error, [spectra:error()]}.
map_fields_to_schema(TypeInfo, Fields) ->
    case process_map_fields(TypeInfo, Fields, #{}, [], false) of
        {ok, Properties, Required, HasAdditional} ->
            Schema =
                lists:foldl(
                    fun({Key, Value, SkipValue}, Acc) ->
                        map_add_if_not_value(Acc, Key, Value, SkipValue)
                    end,
                    #{type => <<"object">>, additionalProperties => HasAdditional},
                    [{properties, Properties, #{}}, {required, Required, []}]
                ),
            {ok, Schema};
        {error, _} = Err ->
            Err
    end.

-spec process_map_fields(
    spectra:type_info(),
    [spectra:map_field()],
    map(),
    [binary()],
    boolean()
) ->
    {ok, map(), [binary()], boolean()} | {error, [spectra:error()]}.
process_map_fields(_TypeInfo, [], Properties, Required, HasAdditional) ->
    {ok, Properties, Required, HasAdditional};
process_map_fields(
    TypeInfo,
    [#literal_map_field{kind = assoc, binary_name = BinaryName, val_type = FieldType} | Rest],
    Properties,
    Required,
    HasAdditional
) ->
    case do_to_schema(TypeInfo, FieldType) of
        {ok, FieldSchema} ->
            NewProperties = maps:put(BinaryName, FieldSchema, Properties),
            process_map_fields(TypeInfo, Rest, NewProperties, Required, HasAdditional);
        {error, _} = Err ->
            Err
    end;
process_map_fields(
    TypeInfo,
    [#literal_map_field{kind = exact, binary_name = BinaryName, val_type = FieldType} | Rest],
    Properties,
    Required,
    HasAdditional
) ->
    case do_to_schema(TypeInfo, FieldType) of
        {ok, FieldSchema} ->
            NewProperties = maps:put(BinaryName, FieldSchema, Properties),
            NewRequired = [BinaryName | Required],
            process_map_fields(TypeInfo, Rest, NewProperties, NewRequired, HasAdditional);
        {error, _} = Err ->
            Err
    end;
process_map_fields(
    _TypeInfo,
    [#typed_map_field{kind = assoc} | Rest],
    Properties,
    Required,
    _HasAdditional
) ->
    %% Generic key-value map allows additional properties
    process_map_fields(_TypeInfo, Rest, Properties, Required, true);
process_map_fields(
    _TypeInfo,
    [#typed_map_field{kind = exact} | Rest],
    Properties,
    Required,
    _HasAdditional
) ->
    %% Generic key-value map allows additional properties
    process_map_fields(_TypeInfo, Rest, Properties, Required, true).

-spec record_to_schema_internal(spectra:type_info(), atom() | #sp_rec{}) ->
    {ok, map()} | {error, [spectra:error()]}.
record_to_schema_internal(TypeInfo, RecordName) when is_atom(RecordName) ->
    {ok, RecordInfo} = spectra_type_info:get_record(TypeInfo, RecordName),
    record_to_schema_internal(TypeInfo, RecordInfo);
record_to_schema_internal(TypeInfo, #sp_rec{fields = Fields}) ->
    case process_record_fields(TypeInfo, Fields, #{}, []) of
        {ok, Properties, Required} ->
            Schema =
                #{
                    type => <<"object">>,
                    properties => Properties,
                    required => Required
                },
            {ok, Schema};
        {error, _} = Err ->
            Err
    end.

-spec process_record_fields(
    spectra:type_info(),
    [#sp_rec_field{}],
    map(),
    [binary()]
) ->
    {ok, map(), [binary()]} | {error, [spectra:error()]}.
process_record_fields(_TypeInfo, [], Properties, Required) ->
    {ok, Properties, lists:reverse(Required)};
process_record_fields(
    TypeInfo,
    [#sp_rec_field{binary_name = BinaryName, type = FieldType} | Rest],
    Properties,
    Required
) ->
    case do_to_schema(TypeInfo, FieldType) of
        {ok, FieldSchema} ->
            NewProperties = Properties#{BinaryName => FieldSchema},
            NewRequired =
                case spectra_type:can_be_missing(TypeInfo, FieldType) of
                    {true, _} ->
                        Required;
                    false ->
                        [BinaryName | Required]
                end,
            process_record_fields(TypeInfo, Rest, NewProperties, NewRequired);
        {error, _} = Err ->
            Err
    end.

%% Helper function to generate oneOf schemas
generate_oneof_schema(TypeInfo, Types) ->
    case
        spectra_util:fold_until_error(
            fun(T, Acc) ->
                case do_to_schema(TypeInfo, T) of
                    {ok, Schema} ->
                        {ok, [Schema | Acc]};
                    {error, _} = Err ->
                        Err
                end
            end,
            [],
            Types
        )
    of
        {ok, Schemas} ->
            {ok, #{oneOf => lists:reverse(Schemas)}};
        {error, _} = Err ->
            Err
    end.

try_generate_enum_schema(Types) ->
    case
        lists:all(
            fun
                (#sp_literal{}) -> true;
                (_) -> false
            end,
            Types
        )
    of
        true ->
            EnumValues = lists:map(
                fun
                    (#sp_literal{value = Value}) when Value =:= undefined orelse Value =:= nil ->
                        null;
                    (#sp_literal{value = Value}) when Value =:= true orelse Value =:= false ->
                        Value;
                    (#sp_literal{value = Value, binary_value = BinaryValue}) when is_atom(Value) ->
                        BinaryValue;
                    (#sp_literal{value = Value}) ->
                        Value
                end,
                Types
            ),
            JsonType = infer_json_type(Types),
            case JsonType of
                undefined ->
                    {ok, #{enum => EnumValues}};
                Type ->
                    {ok, #{type => Type, enum => EnumValues}}
            end;
        false ->
            not_all_literals
    end.

infer_json_type(Types) ->
    JsonTypes = lists:map(fun literal_to_json_type/1, Types),
    case lists:usort(JsonTypes) of
        [SingleType] -> SingleType;
        _ -> undefined
    end.

literal_to_json_type(#sp_literal{value = Value}) when Value =:= undefined orelse Value =:= nil ->
    null;
literal_to_json_type(#sp_literal{value = []}) ->
    null;
literal_to_json_type(#sp_literal{value = Value}) when Value =:= true orelse Value =:= false ->
    <<"boolean">>;
literal_to_json_type(#sp_literal{value = Value}) when is_integer(Value) ->
    <<"integer">>;
literal_to_json_type(#sp_literal{value = Value}) when is_atom(Value) ->
    <<"string">>.

%% Helper function to conditionally add key-value pairs to a map
-spec map_add_if_not_value(Map, Key, Value, SkipValue) -> Map when
    Map :: map(),
    Key :: term(),
    Value :: term(),
    SkipValue :: term().
map_add_if_not_value(Map, _Key, Value, SkipValue) when Value =:= SkipValue ->
    Map;
map_add_if_not_value(Map, Key, Value, _SkipValue) ->
    Map#{Key => Value}.
