-module(erldantic_json_schema).

-export([type_to_schema/2, record_to_schema/2]).

-ignore_xref([{erldantic_json_schema, type_to_schema, 2},
              {erldantic_json_schema, record_to_schema, 2}]).

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

%% API

-doc("Converts an Erlang type definition to a JSON schema.\nThis function extracts the type definition from the specified module\nand generates a corresponding JSON schema object.\nThe type must be of arity 0.\n\n### Returns\n{ok, Schema} if conversion succeeds, or {error, Errors} if the type is not found").
-doc(#{params =>
           #{"Module" => "The module containing the type definition",
             "TypeName" => "The name of the type to convert to JSON schema"}}).

-spec type_to_schema(Module :: module(), TypeName :: atom()) ->
                        Schema :: {ok, map()} | {error, [erldantic:error()]}.
type_to_schema(Module, TypeName) when is_atom(Module) andalso is_atom(TypeName) ->
    TypeArity = 0,
    TypeRef = {type, TypeName, TypeArity},
    to_schema_no_pt(Module, TypeRef).

-doc("Converts an Erlang record definition to a JSON schema.\nThis function extracts the record definition from the specified module\nand generates a corresponding JSON object schema.\n\n### Returns\n{ok, Schema} if conversion succeeds, or {error, Errors} if the record is not found").
-doc(#{params =>
           #{"Module" => "The module containing the record definition",
             "RecordName" => "The name of the record to convert to JSON schema"}}).

-spec record_to_schema(Module :: module(), RecordName :: atom()) ->
                          {ok, Schema :: map()} | {error, [erldantic:error()]}.
record_to_schema(Module, RecordName) when is_atom(Module) andalso is_atom(RecordName) ->
    to_schema_no_pt(Module, {record, RecordName}).

%% INTERNAL

-spec to_schema_no_pt(Module :: module(), TypeRef :: erldantic:ed_type_reference()) ->
                         {ok, Schema :: map()} | {error, [erldantic:error()]}.
to_schema_no_pt(Module, TypeRef) ->
    try
        TypeInfo = erldantic_module_types:get(Module),
        to_schema(TypeInfo, TypeRef)
    catch
        error:Reason ->
            {error,
             [#ed_error{type = no_match,
                        location = [Module],
                        ctx = #{reason => Reason, error_type => module_extraction_failed}}]}
    end.

-spec to_schema(erldantic:type_info(), erldantic:ed_type_or_ref()) ->
                   {ok, Schema :: map()} | {error, [erldantic:error()]}.
to_schema(TypeInfo, Type) ->
    do_to_schema(TypeInfo, Type).

-spec do_to_schema(TypeInfo :: erldantic:type_info(),
                   Type :: erldantic:ed_type_or_ref()) ->
                      {ok, Schema :: map()} | {error, [erldantic:error()]}.
%% Simple types
do_to_schema(_TypeInfo, #ed_simple_type{type = integer}) ->
    {ok, #{type => <<"integer">>}};
do_to_schema(_TypeInfo, #ed_simple_type{type = string}) ->
    {ok, #{type => <<"string">>}};
do_to_schema(_TypeInfo, #ed_simple_type{type = boolean}) ->
    {ok, #{type => <<"boolean">>}};
do_to_schema(_TypeInfo, #ed_simple_type{type = number}) ->
    {ok, #{type => <<"number">>}};
do_to_schema(_TypeInfo, #ed_simple_type{type = float}) ->
    {ok, #{type => <<"number">>, format => <<"float">>}};
do_to_schema(_TypeInfo, #ed_simple_type{type = atom}) ->
    {ok, #{type => <<"string">>}};
do_to_schema(_TypeInfo, #ed_simple_type{type = binary}) ->
    {ok, #{type => <<"string">>, format => <<"binary">>}};
do_to_schema(_TypeInfo, #ed_simple_type{type = nonempty_binary}) ->
    {ok,
     #{type => <<"string">>,
       format => <<"binary">>,
       minLength => 1}};
do_to_schema(_TypeInfo, #ed_simple_type{type = nonempty_string}) ->
    {ok, #{type => <<"string">>, minLength => 1}};
do_to_schema(_TypeInfo, #ed_simple_type{type = pos_integer}) ->
    {ok, #{type => <<"integer">>, minimum => 1}};
do_to_schema(_TypeInfo, #ed_simple_type{type = non_neg_integer}) ->
    {ok, #{type => <<"integer">>, minimum => 0}};
do_to_schema(_TypeInfo, #ed_simple_type{type = neg_integer}) ->
    {ok, #{type => <<"integer">>, maximum => -1}};
do_to_schema(_TypeInfo, #ed_simple_type{type = term}) ->
    {ok, #{}};  % any type
%% Range types
do_to_schema(_TypeInfo,
             #ed_range{type = integer,
                       lower_bound = Min,
                       upper_bound = Max}) ->
    {ok,
     #{type => <<"integer">>,
       minimum => Min,
       maximum => Max}};
%% Literal types
do_to_schema(_TypeInfo, #ed_literal{value = undefined}) ->
    {ok, #{enum => [null]}};
do_to_schema(_TypeInfo, #ed_literal{value = Value}) ->
    {ok, #{enum => [Value]}};
%% List types
do_to_schema(TypeInfo, #ed_list{type = ItemType}) ->
    case do_to_schema(TypeInfo, ItemType) of
        {ok, ItemSchema} ->
            {ok, #{type => <<"array">>, items => ItemSchema}};
        {error, _} = Err ->
            Err
    end;
do_to_schema(TypeInfo, #ed_nonempty_list{type = ItemType}) ->
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
do_to_schema(TypeInfo, #ed_union{types = Types}) ->
    case erldantic_util:fold_until_error(fun(T, Acc) ->
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
    end;
%% Map types
do_to_schema(TypeInfo, #ed_map{fields = Fields}) ->
    map_fields_to_schema(TypeInfo, Fields);
%% Record types
do_to_schema(TypeInfo, {record, RecordName}) when is_atom(RecordName) ->
    record_to_schema_internal(TypeInfo, RecordName);
do_to_schema(TypeInfo, #ed_rec{} = RecordInfo) ->
    record_to_schema_internal(TypeInfo, RecordInfo);
%% Record references
do_to_schema(TypeInfo, #ed_rec_ref{record_name = RecordName}) ->
    record_to_schema_internal(TypeInfo, RecordName);
%% Type references
do_to_schema(TypeInfo, {type, TypeName, TypeArity}) when is_atom(TypeName) ->
    Type = type_info_get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, []),
    do_to_schema(TypeInfo, TypeWithoutVars);
%% User type references
do_to_schema(TypeInfo, #ed_user_type_ref{type_name = TypeName, variables = TypeArgs}) ->
    TypeArity = length(TypeArgs),
    case maps:get({type, TypeName, TypeArity}, TypeInfo, undefined) of
        undefined ->
            {error,
             [#ed_error{type = no_match,
                        location = [TypeName],
                        ctx = #{type => TypeName, arity => TypeArity}}]};
        Type ->
            do_to_schema(TypeInfo, Type)
    end;
%% Remote types
do_to_schema(_TypeInfo, #ed_remote_type{mfargs = {Module, TypeName, Args}}) ->
    TypeInfo = erldantic_module_types:get(Module),
    TypeArity = length(Args),
    Type = type_info_get_type(TypeInfo, TypeName, TypeArity),
    TypeWithoutVars = apply_args(TypeInfo, Type, Args),
    do_to_schema(TypeInfo, TypeWithoutVars);
%% Unsupported types
do_to_schema(_TypeInfo, #ed_simple_type{type = NotSupported} = Type)
    when NotSupported =:= pid
         orelse NotSupported =:= port
         orelse NotSupported =:= reference
         orelse NotSupported =:= bitstring
         orelse NotSupported =:= nonempty_bitstring
         orelse NotSupported =:= none ->
    erlang:error({type_not_supported, Type});
do_to_schema(_TypeInfo, #ed_tuple{} = Type) ->
    erlang:error({type_not_supported, Type});
do_to_schema(_TypeInfo, #ed_function{} = Type) ->
    erlang:error({type_not_supported, Type});
%% Fallback
do_to_schema(_TypeInfo, Type) ->
    {error,
     [#ed_error{type = no_match,
                location = [],
                ctx = #{type => Type}}]}.

%% Helper functions

record_replace_vars(RecordInfo, TypeArgs) ->
    lists:foldl(fun({Field, Type}, Fields) ->
                   lists:keyreplace(Field, 1, Fields, {Field, Type})
                end,
                RecordInfo,
                TypeArgs).

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
        #ed_map{fields = Fields} ->
            #ed_map{fields =
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
        #ed_rec_ref{record_name = RecordName, field_types = RefFieldTypes} ->
            case TypeInfo of
                #{{record, RecordName} := #ed_rec{fields = Fields} = Rec} ->
                    NewRec = Rec#ed_rec{fields = record_replace_vars(Fields, RefFieldTypes)},
                    type_replace_vars(TypeInfo, NewRec, NamedTypes);
                #{} ->
                    erlang:error({missing_type, {record, RecordName}})
            end;
        #ed_remote_type{mfargs = {Module, TypeName, Args}} ->
            case erldantic_module_types:get(Module) of
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
        #ed_list{type = ListType} ->
            #ed_list{type = type_replace_vars(TypeInfo, ListType, NamedTypes)}
    end;
type_replace_vars(_TypeInfo, #ed_rec{fields = Fields} = Rec, NamedTypes) ->
    Rec#ed_rec{fields =
                   lists:map(fun({Name, NType}) ->
                                {Name, type_replace_vars(_TypeInfo, NType, NamedTypes)}
                             end,
                             Fields)};
type_replace_vars(_TypeInfo, Type, _NamedTypes) ->
    Type.

arg_names(#ed_type_with_variables{vars = Args}) ->
    Args;
arg_names(_) ->
    [].

apply_args(TypeInfo, Type, TypeArgs) when is_list(TypeArgs) ->
    ArgNames = arg_names(Type),
    NamedTypes =
        maps:from_list(
            lists:zip(ArgNames, TypeArgs)),
    type_replace_vars(TypeInfo, Type, NamedTypes).

-spec type_info_get_type(TypeInfo :: erldantic:type_info(),
                         TypeName :: atom(),
                         TypeArity :: non_neg_integer()) ->
                            erldantic:ed_type().
type_info_get_type(TypeInfo, TypeName, TypeArity) ->
    maps:get({type, TypeName, TypeArity}, TypeInfo).

-spec map_fields_to_schema(erldantic:type_info(), [erldantic:map_field()]) ->
                              {ok, map()} | {error, [erldantic:error()]}.
map_fields_to_schema(TypeInfo, Fields) ->
    case process_map_fields(TypeInfo, Fields, #{}, [], false) of
        {ok, Properties, Required, HasAdditional} ->
            Schema =
                case {Properties, HasAdditional} of
                    {Props, false} when Props =/= #{} ->
                        BaseSchema = #{type => <<"object">>, properties => Props},
                        case Required of
                            [] ->
                                BaseSchema;
                            _ ->
                                BaseSchema#{required => Required}
                        end;
                    {#{}, true} ->
                        #{type => <<"object">>, additionalProperties => true};
                    _ ->
                        #{type => <<"object">>}
                end,
            {ok, Schema};
        {error, _} = Err ->
            Err
    end.

-spec process_map_fields(erldantic:type_info(),
                         [erldantic:map_field()],
                         map(),
                         [atom()],
                         boolean()) ->
                            {ok, map(), [atom()], boolean()} | {error, [erldantic:error()]}.
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
                   [{map_field_type_assoc, KeyType, ValueType} | _Rest],
                   _Properties,
                   _Required,
                   _HasAdditional) ->
    erlang:error({type_not_supported, {map_field_type_assoc, KeyType, ValueType}});
process_map_fields(_TypeInfo,
                   [{map_field_type_exact, KeyType, ValueType} | _Rest],
                   _Properties,
                   _Required,
                   _HasAdditional) ->
    erlang:error({type_not_supported, {map_field_type_exact, KeyType, ValueType}}).

-spec record_to_schema_internal(erldantic:type_info(), atom() | #ed_rec{}) ->
                                   {ok, map()} | {error, [erldantic:error()]}.
record_to_schema_internal(TypeInfo, RecordName) when is_atom(RecordName) ->
    RecordInfo = type_info_get_record(TypeInfo, RecordName),
    record_to_schema_internal(TypeInfo, RecordInfo);
record_to_schema_internal(TypeInfo, #ed_rec{fields = Fields}) ->
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

type_info_get_record(TypeInfo, RecordName) ->
    maps:get({record, RecordName}, TypeInfo).

-spec process_record_fields(erldantic:type_info(),
                            [{atom(), erldantic:ed_type()}],
                            map(),
                            [atom()]) ->
                               {ok, map(), [atom()]} | {error, [erldantic:error()]}.
process_record_fields(_TypeInfo, [], Properties, Required) ->
    {ok, Properties, lists:reverse(Required)};
process_record_fields(TypeInfo, [{FieldName, FieldType} | Rest], Properties, Required) ->
    case do_to_schema(TypeInfo, FieldType) of
        {ok, FieldSchema} ->
            NewProperties = Properties#{FieldName => FieldSchema},
            NewRequired = [FieldName | Required],
            process_record_fields(TypeInfo, Rest, NewProperties, NewRequired);
        {error, _} = Err ->
            Err
    end.
