-module(erldantic_openapi).

-export([type_to_schema/2, record_to_schema/2]).

-ignore_xref([{erldantic_openapi, type_to_schema, 2},
              {erldantic_openapi, record_to_schema, 2}]).

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

%% API

-doc("Converts an Erlang type definition to an OpenAPI 3.0 schema.\nThis function extracts the type definition from the specified module\nand generates a corresponding OpenAPI schema object.\nThe type must be of arity 0.\n\n### Returns\n{ok, Schema} if conversion succeeds, or {error, Errors} if the type is not found").
-doc(#{params =>
           #{"Module" => "The module containing the type definition",
             "TypeName" => "The name of the type to convert to OpenAPI schema"}}).

-spec type_to_schema(Module :: module(), TypeName :: atom()) ->
                        Schema :: map() | {error, [erldantic:error()]}.
type_to_schema(Module, TypeName) when is_atom(Module) andalso is_atom(TypeName) ->
    TypeArity = 0,
    TypeRef = {type, TypeName, TypeArity},
    case to_schema_no_pt(Module, TypeRef) of
        {ok, Schema} ->
            Schema;
        {error, Errors} ->
            {error, Errors}
    end.

-doc("Converts an Erlang record definition to an OpenAPI 3.0 schema.\nThis function extracts the record definition from the specified module\nand generates a corresponding OpenAPI object schema.\n\n### Returns\n{ok, Schema} if conversion succeeds, or {error, Errors} if the record is not found").
-doc(#{params =>
           #{"Module" => "The module containing the record definition",
             "RecordName" => "The name of the record to convert to OpenAPI schema"}}).

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
    case do_to_schema(TypeInfo, Type) of
        {ok, Schema} ->
            {ok, Schema};
        {error, Errs} ->
            {error, Errs}
    end.

-spec do_to_schema(TypeInfo :: erldantic:type_info(),
                   Type :: erldantic:ed_type_or_ref()) ->
                      {ok, Schema :: map()} | {error, [erldantic:error()]}.
%% Simple types
do_to_schema(_TypeInfo, #ed_simple_type{type = integer}) ->
    {ok, #{type => integer}};
do_to_schema(_TypeInfo, #ed_simple_type{type = string}) ->
    {ok, #{type => string}};
do_to_schema(_TypeInfo, #ed_simple_type{type = boolean}) ->
    {ok, #{type => boolean}};
do_to_schema(_TypeInfo, #ed_simple_type{type = number}) ->
    {ok, #{type => number}};
do_to_schema(_TypeInfo, #ed_simple_type{type = float}) ->
    {ok, #{type => number, format => float}};
do_to_schema(_TypeInfo, #ed_simple_type{type = atom}) ->
    {ok, #{type => string}};
do_to_schema(_TypeInfo, #ed_simple_type{type = binary}) ->
    {ok, #{type => string, format => binary}};
do_to_schema(_TypeInfo, #ed_simple_type{type = nonempty_binary}) ->
    {ok,
     #{type => string,
       format => binary,
       minLength => 1}};
do_to_schema(_TypeInfo, #ed_simple_type{type = nonempty_string}) ->
    {ok, #{type => string, minLength => 1}};
do_to_schema(_TypeInfo, #ed_simple_type{type = pos_integer}) ->
    {ok, #{type => integer, minimum => 1}};
do_to_schema(_TypeInfo, #ed_simple_type{type = non_neg_integer}) ->
    {ok, #{type => integer, minimum => 0}};
do_to_schema(_TypeInfo, #ed_simple_type{type = neg_integer}) ->
    {ok, #{type => integer, maximum => -1}};
do_to_schema(_TypeInfo, #ed_simple_type{type = term}) ->
    {ok, #{}};  % any type
%% Range types
do_to_schema(_TypeInfo,
             #ed_range{type = integer,
                       lower_bound = Min,
                       upper_bound = Max}) ->
    {ok,
     #{type => integer,
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
            {ok, #{type => array, items => ItemSchema}};
        {error, _} = Err ->
            Err
    end;
do_to_schema(TypeInfo, #ed_nonempty_list{type = ItemType}) ->
    case do_to_schema(TypeInfo, ItemType) of
        {ok, ItemSchema} ->
            {ok,
             #{type => array,
               items => ItemSchema,
               minItems => 1}};
        {error, _} = Err ->
            Err
    end;
%% Union types
do_to_schema(TypeInfo, #ed_union{types = Types}) ->
    case lists:foldl(fun (T, {ok, Acc}) ->
                             case do_to_schema(TypeInfo, T) of
                                 {ok, Schema} ->
                                     {ok, [Schema | Acc]};
                                 {error, _} = Err ->
                                     Err
                             end;
                         (_, {error, _} = Err) ->
                             Err
                     end,
                     {ok, []},
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
%% Type references
do_to_schema(TypeInfo, {type, TypeName, TypeArity}) when is_atom(TypeName) ->
    case maps:get({type, TypeName, TypeArity}, TypeInfo, undefined) of
        undefined ->
            {error,
             [#ed_error{type = no_match,
                        location = [TypeName],
                        ctx = #{type => TypeName, arity => TypeArity}}]};
        Type ->
            do_to_schema(TypeInfo, Type)
    end;
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
    RemoteTypeInfo = erldantic_module_types:get(Module),
    TypeArity = length(Args),
    case maps:get({type, TypeName, TypeArity}, RemoteTypeInfo, undefined) of
        undefined ->
            {error,
             [#ed_error{type = no_match,
                        location = [Module, TypeName],
                        ctx =
                            #{module => Module,
                              type => TypeName,
                              arity => TypeArity}}]};
        Type ->
            do_to_schema(RemoteTypeInfo, Type)
    end;
%% Unsupported types
do_to_schema(_TypeInfo, #ed_simple_type{type = NotSupported} = Type)
    when NotSupported =:= pid
         orelse NotSupported =:= port
         orelse NotSupported =:= reference
         orelse NotSupported =:= bitstring
         orelse NotSupported =:= nonempty_bitstring
         orelse NotSupported =:= none ->
    {error,
     [#ed_error{type = no_match,
                location = [],
                ctx = #{type => Type}}]};
do_to_schema(_TypeInfo, #ed_tuple{} = Type) ->
    {error,
     [#ed_error{type = no_match,
                location = [],
                ctx = #{type => Type}}]};
do_to_schema(_TypeInfo, #ed_function{} = Type) ->
    {error,
     [#ed_error{type = no_match,
                location = [],
                ctx = #{type => Type}}]};
%% Fallback
do_to_schema(_TypeInfo, Type) ->
    {error,
     [#ed_error{type = no_match,
                location = [],
                ctx = #{type => Type}}]}.

%% Helper functions

-spec map_fields_to_schema(erldantic:type_info(), [erldantic:map_field()]) ->
                              {ok, map()} | {error, [erldantic:error()]}.
map_fields_to_schema(TypeInfo, Fields) ->
    case process_map_fields(TypeInfo, Fields, #{}, [], false) of
        {ok, Properties, Required, HasAdditional} ->
            Schema =
                case {Properties, HasAdditional} of
                    {Props, false} when Props =/= #{} ->
                        BaseSchema = #{type => object, properties => Props},
                        case Required of
                            [] ->
                                BaseSchema;
                            _ ->
                                BaseSchema#{required => Required}
                        end;
                    {#{}, true} ->
                        #{type => object, additionalProperties => true};
                    _ ->
                        #{type => object}
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
process_map_fields(TypeInfo,
                   [{map_field_type_assoc, _KeyType, ValueType} | Rest],
                   Properties,
                   Required,
                   _HasAdditional) ->
    case do_to_schema(TypeInfo, ValueType) of
        {ok, _ValueSchema} ->
            process_map_fields(TypeInfo, Rest, Properties, Required, true);
        {error, _} = Err ->
            Err
    end;
process_map_fields(TypeInfo, [_Field | Rest], Properties, Required, HasAdditional) ->
    process_map_fields(TypeInfo, Rest, Properties, Required, HasAdditional).

-spec record_to_schema_internal(erldantic:type_info(), atom() | #ed_rec{}) ->
                                   {ok, map()} | {error, [erldantic:error()]}.
record_to_schema_internal(TypeInfo, RecordName) when is_atom(RecordName) ->
    case maps:get({record, RecordName}, TypeInfo, undefined) of
        undefined ->
            {error,
             [#ed_error{type = no_match,
                        location = [RecordName],
                        ctx = #{record => RecordName}}]};
        #ed_rec{} = RecordInfo ->
            record_to_schema_internal(TypeInfo, RecordInfo);
        _Other ->
            {error,
             [#ed_error{type = type_mismatch,
                        location = [RecordName],
                        ctx = #{record => RecordName, error => invalid_record_type}}]}
    end;
record_to_schema_internal(TypeInfo, #ed_rec{fields = Fields}) ->
    case process_record_fields(TypeInfo, Fields, #{}, []) of
        {ok, Properties, Required} ->
            Schema = #{type => object, properties => Properties},
            FinalSchema =
                case Required of
                    [] ->
                        Schema;
                    _ ->
                        Schema#{required => Required}
                end,
            {ok, FinalSchema};
        {error, _} = Err ->
            Err
    end.

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
            NewProperties = maps:put(FieldName, FieldSchema, Properties),
            NewRequired = [FieldName | Required],
            process_record_fields(TypeInfo, Rest, NewProperties, NewRequired);
        {error, _} = Err ->
            Err
    end.
