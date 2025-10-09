-module(impala_json_schema).

-export([to_schema/2]).

-ignore_xref([to_schema/2]).

-include("../include/impala.hrl").
-include("../include/impala_internal.hrl").

%% API

-spec to_schema(module() | impala:type_info(), impala:im_type_or_ref()) ->
                   {ok, Schema :: map()} | {error, [impala:error()]}.
to_schema(Module, Type) when is_atom(Module) ->
    TypeInfo = impala_module_types:get(Module),
    to_schema(TypeInfo, Type);
%% Type references
to_schema(TypeInfo, {type, TypeName, TypeArity}) when is_atom(TypeName) ->
    case impala_type_info:get_type(TypeInfo, TypeName, TypeArity) of
        {ok, Type} ->
            TypeWithoutVars = apply_args(TypeInfo, Type, []),
            do_to_schema(TypeInfo, TypeWithoutVars);
        error ->
            {error,
             [#im_error{type = no_match,
                        location = [TypeName],
                        ctx = #{type => TypeName, arity => TypeArity}}]}
    end;
to_schema(TypeInfo, Type) ->
    do_to_schema(TypeInfo, Type).

-spec do_to_schema(TypeInfo :: impala:type_info(), Type :: impala:im_type_or_ref()) ->
                      {ok, Schema :: map()} | {error, [impala:error()]}.
%% Simple types
do_to_schema(_TypeInfo, #im_simple_type{type = integer}) ->
    {ok, #{type => <<"integer">>}};
do_to_schema(_TypeInfo, #im_simple_type{type = string}) ->
    {ok, #{type => <<"string">>}};
do_to_schema(_TypeInfo, #im_simple_type{type = iodata}) ->
    {ok, #{type => <<"string">>}};
do_to_schema(_TypeInfo, #im_simple_type{type = iolist}) ->
    {ok, #{type => <<"string">>}};
do_to_schema(_TypeInfo, #im_simple_type{type = boolean}) ->
    {ok, #{type => <<"boolean">>}};
do_to_schema(_TypeInfo, #im_simple_type{type = number}) ->
    {ok, #{type => <<"number">>}};
do_to_schema(_TypeInfo, #im_simple_type{type = float}) ->
    {ok, #{type => <<"number">>, format => <<"float">>}};
do_to_schema(_TypeInfo, #im_simple_type{type = atom}) ->
    {ok, #{type => <<"string">>}};
do_to_schema(_TypeInfo, #im_simple_type{type = binary}) ->
    {ok, #{type => <<"string">>}};
do_to_schema(_TypeInfo, #im_simple_type{type = nonempty_binary}) ->
    {ok, #{type => <<"string">>, minLength => 1}};
do_to_schema(_TypeInfo, #im_simple_type{type = nonempty_string}) ->
    {ok, #{type => <<"string">>, minLength => 1}};
do_to_schema(_TypeInfo, #im_simple_type{type = pos_integer}) ->
    {ok, #{type => <<"integer">>, minimum => 1}};
do_to_schema(_TypeInfo, #im_simple_type{type = non_neg_integer}) ->
    {ok, #{type => <<"integer">>, minimum => 0}};
do_to_schema(_TypeInfo, #im_simple_type{type = neg_integer}) ->
    {ok, #{type => <<"integer">>, maximum => -1}};
do_to_schema(_TypeInfo, #im_simple_type{type = term}) ->
    {ok, #{}};  % any type
%% Range types
do_to_schema(_TypeInfo,
             #im_range{type = integer,
                       lower_bound = Min,
                       upper_bound = Max}) ->
    {ok,
     #{type => <<"integer">>,
       minimum => Min,
       maximum => Max}};
%% Literal types
do_to_schema(_TypeInfo, #im_literal{value = undefined}) ->
    {ok, #{enum => [null]}};
do_to_schema(_TypeInfo, #im_literal{value = Value}) when is_atom(Value) ->
    {ok, #{enum => [atom_to_binary(Value, utf8)]}};
do_to_schema(_TypeInfo, #im_literal{value = Value}) ->
    {ok, #{enum => [Value]}};
%% List types
do_to_schema(TypeInfo, #im_list{type = ItemType}) ->
    case do_to_schema(TypeInfo, ItemType) of
        {ok, ItemSchema} ->
            {ok, #{type => <<"array">>, items => ItemSchema}};
        {error, _} = Err ->
            Err
    end;
do_to_schema(TypeInfo, #im_nonempty_list{type = ItemType}) ->
    case do_to_schema(TypeInfo, ItemType) of
        {ok, ItemSchema} ->
            {ok,
             #{type => <<"array">>,
               items => ItemSchema,
               minItems => 1}};
        {error, _} = Err ->
            Err
    end;
%% Union types
do_to_schema(TypeInfo, #im_union{types = Types}) ->
    %% Check if this is a union with undefined - if so, extract the non-undefined type
    case lists:partition(fun (#im_literal{value = undefined}) ->
                                 true;
                             (_) ->
                                 false
                         end,
                         Types)
    of
        {[_UndefinedLiteral], [SingleType]} ->
            %% Union with undefined and one other type - return just the other type
            do_to_schema(TypeInfo, SingleType);
        {[], _} ->
            %% No undefined in union - generate normal oneOf
            generate_oneof_schema(TypeInfo, Types);
        {[_UndefinedLiteral], OtherTypes} when length(OtherTypes) > 1 ->
            %% Union with undefined and multiple other types - generate oneOf for all
            generate_oneof_schema(TypeInfo, Types)
    end;
%% Map types
do_to_schema(TypeInfo, #im_map{fields = Fields}) ->
    map_fields_to_schema(TypeInfo, Fields);
%% Record types
do_to_schema(TypeInfo, {record, RecordName}) when is_atom(RecordName) ->
    record_to_schema_internal(TypeInfo, RecordName);
do_to_schema(TypeInfo, #im_rec{} = RecordInfo) ->
    record_to_schema_internal(TypeInfo, RecordInfo);
%% Record references
do_to_schema(TypeInfo, #im_rec_ref{record_name = RecordName}) ->
    record_to_schema_internal(TypeInfo, RecordName);
%% User type references
do_to_schema(TypeInfo, #im_user_type_ref{type_name = TypeName, variables = TypeArgs}) ->
    TypeArity = length(TypeArgs),
    case impala_type_info:get_type(TypeInfo, TypeName, TypeArity) of
        error ->
            {error,
             [#im_error{type = no_match,
                        location = [TypeName],
                        ctx = #{type => TypeName, arity => TypeArity}}]};
        {ok, Type} ->
            do_to_schema(TypeInfo, Type)
    end;
%% Remote types
do_to_schema(_TypeInfo, #im_remote_type{mfargs = {Module, TypeName, Args}}) ->
    TypeInfo = impala_module_types:get(Module),
    TypeArity = length(Args),
    {ok, Type} = impala_type_info:get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
    do_to_schema(TypeInfo, TypeWithoutVars);
%% Unsupported types
do_to_schema(_TypeInfo, #im_simple_type{type = NotSupported} = Type)
    when NotSupported =:= pid
         orelse NotSupported =:= port
         orelse NotSupported =:= reference
         orelse NotSupported =:= bitstring
         orelse NotSupported =:= nonempty_bitstring
         orelse NotSupported =:= none ->
    erlang:error({type_not_supported, Type});
do_to_schema(_TypeInfo, #im_tuple{} = Type) ->
    erlang:error({type_not_supported, Type});
do_to_schema(_TypeInfo, #im_function{} = Type) ->
    erlang:error({type_not_supported, Type});
%% Fallback
do_to_schema(_TypeInfo, Type) ->
    {error,
     [#im_error{type = no_match,
                location = [],
                ctx = #{type => Type}}]}.

%% Helper functions

record_replace_vars(RecordInfo, TypeArgs) ->
    lists:foldl(fun({Field, Type}, Fields) ->
                   lists:keyreplace(Field, 1, Fields, {Field, Type})
                end,
                RecordInfo,
                TypeArgs).

-spec type_replace_vars(TypeInfo :: impala:type_info(),
                        Type :: impala:im_type(),
                        NamedTypes :: #{atom() => impala:im_type()}) ->
                           impala:im_type().
type_replace_vars(_TypeInfo, #im_var{name = Name}, NamedTypes) ->
    maps:get(Name, NamedTypes, #im_simple_type{type = term});
type_replace_vars(TypeInfo, #im_type_with_variables{type = Type}, NamedTypes) ->
    case Type of
        #im_union{types = UnionTypes} ->
            #im_union{types =
                          lists:map(fun(UnionType) ->
                                       type_replace_vars(TypeInfo, UnionType, NamedTypes)
                                    end,
                                    UnionTypes)};
        #im_map{fields = Fields} ->
            #im_map{fields =
                        lists:map(fun ({map_field_assoc, FieldName, FieldType}) ->
                                          {map_field_assoc,
                                           FieldName,
                                           type_replace_vars(TypeInfo, FieldType, NamedTypes)};
                                      ({map_field_exact, FieldName, FieldType}) ->
                                          {map_field_exact,
                                           FieldName,
                                           type_replace_vars(TypeInfo, FieldType, NamedTypes)};
                                      ({map_field_type_assoc, KeyType, ValueType}) ->
                                          {map_field_type_assoc,
                                           type_replace_vars(TypeInfo, KeyType, NamedTypes),
                                           type_replace_vars(TypeInfo, ValueType, NamedTypes)};
                                      ({map_field_type_exact, KeyType, ValueType}) ->
                                          {map_field_type_exact,
                                           type_replace_vars(TypeInfo, KeyType, NamedTypes),
                                           type_replace_vars(TypeInfo, ValueType, NamedTypes)}
                                  end,
                                  Fields)};
        #im_rec_ref{record_name = RecordName, field_types = RefFieldTypes} ->
            case TypeInfo of
                #{{record, RecordName} := #im_rec{fields = Fields} = Rec} ->
                    NewRec = Rec#im_rec{fields = record_replace_vars(Fields, RefFieldTypes)},
                    type_replace_vars(TypeInfo, NewRec, NamedTypes);
                #{} ->
                    erlang:error({missing_type, {record, RecordName}})
            end;
        #im_remote_type{mfargs = {Module, TypeName, Args}} ->
            case impala_module_types:get(Module) of
                {ok, TypeInfo} ->
                    TypeArity = length(Args),
                    case TypeInfo of
                        #{{type, TypeName, TypeArity} := Type} ->
                            type_replace_vars(TypeInfo, Type, NamedTypes);
                        #{} ->
                            erlang:error({missing_type, TypeName})
                    end;
                {error, _} = Err ->
                    erlang:error(Err)
            end;
        #im_list{type = ListType} ->
            #im_list{type = type_replace_vars(TypeInfo, ListType, NamedTypes)}
    end;
type_replace_vars(_TypeInfo, #im_rec{fields = Fields} = Rec, NamedTypes) ->
    Rec#im_rec{fields =
                   lists:map(fun({Name, NType}) ->
                                {Name, type_replace_vars(_TypeInfo, NType, NamedTypes)}
                             end,
                             Fields)};
type_replace_vars(_TypeInfo, Type, _NamedTypes) ->
    Type.

arg_names(#im_type_with_variables{vars = Args}) ->
    Args;
arg_names(_) ->
    [].

apply_args(TypeInfo, Type, TypeArgs) when is_list(TypeArgs) ->
    ArgNames = arg_names(Type),
    NamedTypes =
        maps:from_list(
            lists:zip(ArgNames, TypeArgs)),
    type_replace_vars(TypeInfo, Type, NamedTypes).

-spec map_fields_to_schema(impala:type_info(), [impala:map_field()]) ->
                              {ok, map()} | {error, [impala:error()]}.
map_fields_to_schema(TypeInfo, Fields) ->
    case process_map_fields(TypeInfo, Fields, #{}, [], false) of
        {ok, Properties, Required, HasAdditional} ->
            Schema =
                lists:foldl(fun({Key, Value, SkipValue}, Acc) ->
                               map_add_if_not_value(Acc, Key, Value, SkipValue)
                            end,
                            #{type => <<"object">>, additionalProperties => HasAdditional},
                            [{properties, Properties, #{}}, {required, Required, []}]),
            {ok, Schema};
        {error, _} = Err ->
            Err
    end.

-spec process_map_fields(impala:type_info(),
                         [impala:map_field()],
                         map(),
                         [atom()],
                         boolean()) ->
                            {ok, map(), [atom()], boolean()} | {error, [impala:error()]}.
process_map_fields(_TypeInfo, [], Properties, Required, HasAdditional) ->
    {ok, Properties, Required, HasAdditional};
process_map_fields(TypeInfo,
                   [{map_field_assoc, FieldName, FieldType} | Rest],
                   Properties,
                   Required,
                   HasAdditional) ->
    case do_to_schema(TypeInfo, FieldType) of
        {ok, FieldSchema} ->
            NewProperties = maps:put(FieldName, FieldSchema, Properties),
            process_map_fields(TypeInfo, Rest, NewProperties, Required, HasAdditional);
        {error, _} = Err ->
            Err
    end;
process_map_fields(TypeInfo,
                   [{map_field_exact, FieldName, FieldType} | Rest],
                   Properties,
                   Required,
                   HasAdditional) ->
    case do_to_schema(TypeInfo, FieldType) of
        {ok, FieldSchema} ->
            NewProperties = maps:put(FieldName, FieldSchema, Properties),
            NewRequired = [FieldName | Required],
            process_map_fields(TypeInfo, Rest, NewProperties, NewRequired, HasAdditional);
        {error, _} = Err ->
            Err
    end;
process_map_fields(_TypeInfo,
                   [{map_field_type_assoc, _KeyType, _ValueType} | Rest],
                   Properties,
                   Required,
                   _HasAdditional) ->
    %% Generic key-value map allows additional properties
    process_map_fields(_TypeInfo, Rest, Properties, Required, true);
process_map_fields(_TypeInfo,
                   [{map_field_type_exact, _KeyType, _ValueType} | Rest],
                   Properties,
                   Required,
                   _HasAdditional) ->
    %% Generic key-value map allows additional properties
    process_map_fields(_TypeInfo, Rest, Properties, Required, true).

-spec record_to_schema_internal(impala:type_info(), atom() | #im_rec{}) ->
                                   {ok, map()} | {error, [impala:error()]}.
record_to_schema_internal(TypeInfo, RecordName) when is_atom(RecordName) ->
    case impala_type_info:get_record(TypeInfo, RecordName) of
        {ok, RecordInfo} ->
            record_to_schema_internal(TypeInfo, RecordInfo);
        error ->
            {error,
             [#im_error{type = no_match,
                        location = [RecordName],
                        ctx = #{type => RecordName}}]}
    end;
record_to_schema_internal(TypeInfo, #im_rec{fields = Fields}) ->
    case process_record_fields(TypeInfo, Fields, #{}, []) of
        {ok, Properties, Required} ->
            Schema =
                #{type => <<"object">>,
                  properties => Properties,
                  required => Required},
            {ok, Schema};
        {error, _} = Err ->
            Err
    end.

-spec process_record_fields(impala:type_info(),
                            [{atom(), impala:im_type()}],
                            map(),
                            [atom()]) ->
                               {ok, map(), [atom()]} | {error, [impala:error()]}.
process_record_fields(_TypeInfo, [], Properties, Required) ->
    {ok, Properties, lists:reverse(Required)};
process_record_fields(TypeInfo, [{FieldName, FieldType} | Rest], Properties, Required) ->
    case do_to_schema(TypeInfo, FieldType) of
        {ok, FieldSchema} ->
            NewProperties = Properties#{FieldName => FieldSchema},
            NewRequired =
                case impala_type:can_be_undefined(TypeInfo, FieldType) of
                    true ->
                        Required;
                    false ->
                        [FieldName | Required]
                end,
            process_record_fields(TypeInfo, Rest, NewProperties, NewRequired);
        {error, _} = Err ->
            Err
    end.

%% Helper function to generate oneOf schemas
generate_oneof_schema(TypeInfo, Types) ->
    case impala_util:fold_until_error(fun(T, Acc) ->
                                         case do_to_schema(TypeInfo, T) of
                                             {ok, Schema} ->
                                                 {ok, [Schema | Acc]};
                                             {error, _} = Err ->
                                                 Err
                                         end
                                      end,
                                      [],
                                      Types)
    of
        {ok, Schemas} ->
            {ok, #{oneOf => lists:reverse(Schemas)}};
        {error, _} = Err ->
            Err
    end.

%% Helper function to conditionally add key-value pairs to a map
-spec map_add_if_not_value(Map, Key, Value, SkipValue) -> Map
    when Map :: map(),
         Key :: term(),
         Value :: term(),
         SkipValue :: term().
map_add_if_not_value(Map, _Key, Value, SkipValue) when Value =:= SkipValue ->
    Map;
map_add_if_not_value(Map, Key, Value, _SkipValue) ->
    Map#{Key => Value}.
