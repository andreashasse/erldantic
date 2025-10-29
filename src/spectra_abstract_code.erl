-module(spectra_abstract_code).

-include("../include/spectra_internal.hrl").

-include_lib("kernel/include/eep48.hrl").

-export([types_in_module/1]).

-define(is_primary_type(PrimaryType),
    PrimaryType =:= string orelse
        PrimaryType =:= nonempty_string orelse
        PrimaryType =:= integer orelse
        PrimaryType =:= boolean orelse
        PrimaryType =:= atom orelse
        PrimaryType =:= float orelse
        PrimaryType =:= binary orelse
        PrimaryType =:= nonempty_binary orelse
        PrimaryType =:= number orelse
        PrimaryType =:= term
).
-define(is_predefined_int_range(_Type),
    _Type =:= non_neg_integer orelse _Type =:= neg_integer orelse _Type =:= pos_integer
).

%% Due to erl_parse:af_wild_attribute() I can't use some of the types.
-type erl_parse__af_field_decl() :: term().

-spec types_in_module(atom()) -> spectra:type_info().
types_in_module(Module) ->
    case code:which(Module) of
        cover_compiled ->
            {_, _, FilePath} = code:get_object_code(Module),
            types_in_module_path(FilePath);
        Error when Error =:= non_existing orelse Error =:= preloaded ->
            erlang:error({module_types_not_found, Module, Error});
        FilePath ->
            types_in_module_path(FilePath)
    end.

types_in_module_path(FilePath) ->
    case beam_lib:chunks(FilePath, [abstract_code]) of
        {ok, {_Module, [{abstract_code, {_, Forms}}]}} ->
            NamedTypes = lists:filtermap(fun(F) -> type_in_form(F) end, Forms),
            build_type_info(NamedTypes);
        {ok, {Module, [{abstract_code, no_abstract_code}]}} ->
            erlang:error({module_not_compiled_with_debug_info, Module, FilePath});
        {error, beam_lib, Reason} ->
            erlang:error({beam_lib_error, FilePath, Reason})
    end.

build_type_info(NamedTypes) ->
    lists:foldl(fun build_type_info_fold/2, spectra_type_info:new(), NamedTypes).

build_type_info_fold({{type, Name, Arity}, Type}, TypeInfo) ->
    spectra_type_info:add_type(TypeInfo, Name, Arity, Type);
build_type_info_fold({{record, Name}, Record}, TypeInfo) ->
    spectra_type_info:add_record(TypeInfo, Name, Record);
build_type_info_fold({{function, Name, Arity}, FuncSpec}, TypeInfo) ->
    spectra_type_info:add_function(TypeInfo, Name, Arity, FuncSpec).

-spec type_in_form(erl_parse:abstract_form() | erl_parse:form_info()) ->
    false | {true, type_form_result()}.
-type type_form_result() ::
    {{type, atom(), arity()}, spectra:sp_type()}
    | {{record, atom()}, spectra:sp_type()}
    | {{function, atom(), arity()}, [spectra:sp_function_spec()]}.

type_in_form({attribute, _, record, {RecordName, Fields}}) when
    is_list(Fields) andalso is_atom(RecordName)
->
    FieldInfos = lists:map(fun record_field_info/1, Fields),
    {true,
        {{record, RecordName}, #sp_rec{
            name = RecordName,
            fields = FieldInfos,
            arity = length(FieldInfos) + 1
        }}};
type_in_form({attribute, _, TypeOrOpaque, {TypeName, {_, _, record, Attrs}, [] = Args}}) when
    is_atom(TypeName) andalso
        (TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque orelse TypeOrOpaque =:= nominal)
->
    %% FIXME: Sort out why this function clause differs from all others.
    true = is_list(Attrs),
    {RecordName, FieldTypes} = record_field_types(Attrs),
    TypeArity = length(Args),
    Record = #sp_rec_ref{record_name = RecordName, field_types = FieldTypes},
    {true, {{type, TypeName, TypeArity}, Record}};
type_in_form({attribute, _, TypeOrOpaque, {TypeName, Type, Args}}) when
    is_atom(TypeName) andalso
        is_list(Args) andalso
        (TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque orelse TypeOrOpaque =:= nominal)
->
    [FieldInfo] = field_info_to_type(Type),
    Vars = lists:map(fun({var, _, VarName}) when is_atom(VarName) -> VarName end, Args),
    TypeArity = length(Args),
    case Vars of
        [] ->
            {true, {{type, TypeName, TypeArity}, FieldInfo}};
        _ ->
            {true,
                {{type, TypeName, TypeArity}, #sp_type_with_variables{type = FieldInfo, vars = Vars}}}
    end;
type_in_form({attribute, _, spec, {{FunctionName, Arity}, FunctionTypes}}) when
    is_atom(FunctionName) andalso is_integer(Arity) andalso is_list(FunctionTypes)
->
    ProcessedSpecs =
        lists:map(
            fun(FuncType) ->
                case FuncType of
                    {type, _, 'fun', [{type, _, product, Args}, ReturnType]} when
                        is_list(Args)
                    ->
                        ArgTypes =
                            lists:map(
                                fun(Arg) ->
                                    [ArgType] = field_info_to_type(Arg),
                                    ArgType
                                end,
                                Args
                            ),
                        [ReturnTypeProcessed] = field_info_to_type(ReturnType),
                        #sp_function_spec{args = ArgTypes, return = ReturnTypeProcessed};
                    {type, _, bounded_fun, [
                        {type, _, 'fun', [{type, _, product, Args}, ReturnType]}, Constraints
                    ]} when
                        is_list(Args) andalso is_list(Constraints)
                    ->
                        ConstraintMap = bound_fun_constraints(Constraints),
                        ArgTypes =
                            lists:map(
                                fun(Arg) ->
                                    [ArgType] =
                                        field_info_to_type(
                                            bound_fun_substitute_vars(
                                                Arg,
                                                ConstraintMap
                                            )
                                        ),
                                    ArgType
                                end,
                                Args
                            ),
                        [ReturnTypeProcessed] =
                            field_info_to_type(
                                bound_fun_substitute_vars(
                                    ReturnType,
                                    ConstraintMap
                                )
                            ),
                        #sp_function_spec{args = ArgTypes, return = ReturnTypeProcessed}
                end
            end,
            FunctionTypes
        ),
    {true, {{function, FunctionName, Arity}, ProcessedSpecs}};
type_in_form({attribute, _, spec, Spec}) ->
    error({bug_spec_not_handled, Spec});
type_in_form({attribute, _, TypeOrOpaque, _} = T) when
    TypeOrOpaque =:= opaque orelse TypeOrOpaque =:= type orelse TypeOrOpaque =:= nominal
->
    error({not_supported, T});
type_in_form(_) ->
    false.

-spec record_field_types(list()) -> {atom(), [{atom(), spectra:sp_type()}]}.
record_field_types(Attrs) ->
    [{atom, _, RecordName} | FieldInfo] = Attrs,
    true = is_atom(RecordName),
    FieldTypes =
        lists:map(
            fun({type, _, field_type, [{atom, _, FieldName}, RowFieldInfo]}) when
                is_atom(FieldName)
            ->
                [FieldType] = field_info_to_type(RowFieldInfo),
                {FieldName, FieldType}
            end,
            FieldInfo
        ),
    {RecordName, FieldTypes}.

-spec field_info_to_type(term()) -> [spectra:sp_type()].
field_info_to_type({ann_type, _, [{var, _, _VarName}, Type]}) ->
    field_info_to_type(Type);
field_info_to_type({atom, _, Value}) when is_atom(Value) ->
    [#sp_literal{value = Value}];
field_info_to_type({char, _, Value}) when is_integer(Value) ->
    [#sp_literal{value = Value}];
field_info_to_type({integer, _, Value}) when is_integer(Value) ->
    [#sp_literal{value = Value}];
field_info_to_type(Op) when element(1, Op) =:= op ->
    Value = integer_value(Op),
    [#sp_literal{value = Value}];
field_info_to_type({var, _, VarName}) when is_atom(VarName) ->
    [#sp_var{name = VarName}];
field_info_to_type({remote_type, _, [{atom, _, Module}, {atom, _, Type}, Args]}) when
    is_atom(Module) andalso is_atom(Type) andalso is_list(Args)
->
    MyArgs =
        lists:map(
            fun(Arg) ->
                [ArgType] = field_info_to_type(Arg),
                ArgType
            end,
            Args
        ),
    [#sp_remote_type{mfargs = {Module, Type, MyArgs}}];
field_info_to_type({Type, _, map, any}) when
    Type =:= type orelse Type =:= opaque orelse Type =:= nominal
->
    MapFields =
        [{map_field_type_assoc, #sp_simple_type{type = term}, #sp_simple_type{type = term}}],
    [#sp_map{fields = MapFields, struct_name = undefined}];
field_info_to_type({Type, _, tuple, any}) when
    Type =:= type orelse Type =:= opaque orelse Type =:= nominal
->
    [#sp_tuple{fields = any}];
field_info_to_type({user_type, _, Type, TypeAttrs}) when
    is_atom(Type) andalso is_list(TypeAttrs)
->
    TAttrs = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
    [#sp_user_type_ref{type_name = Type, variables = TAttrs}];
field_info_to_type({TypeOrOpaque, _, Type, TypeAttrs}) when
    is_list(TypeAttrs) andalso
        (TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque orelse TypeOrOpaque =:= nominal)
->
    case Type of
        record ->
            {SubTypeRecordName, FieldTypes} = record_field_types(TypeAttrs),
            [#sp_rec_ref{record_name = SubTypeRecordName, field_types = FieldTypes}];
        map ->
            MapFields = lists:flatmap(fun map_field_info/1, TypeAttrs),
            {StructName, NewMapFields} = extract_struct_name(MapFields),
            [#sp_map{fields = NewMapFields, struct_name = StructName}];
        tuple ->
            TupleFields = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [#sp_tuple{fields = TupleFields}];
        union ->
            UnionFields = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [#sp_union{types = UnionFields}];
        Fun when Fun =:= 'fun' orelse Fun =:= function ->
            case TypeAttrs of
                [] ->
                    [#sp_function{args = any, return = #sp_simple_type{type = term}}];
                [{type, _, any}, ReturnType] ->
                    [AReturnType] = field_info_to_type(ReturnType),
                    [#sp_function{args = any, return = AReturnType}];
                [{type, _, product, FunArgTypes}, ReturnType] ->
                    true = is_list(FunArgTypes),
                    AFunArgTypes = lists:flatmap(fun field_info_to_type/1, FunArgTypes),
                    [AReturnType] = field_info_to_type(ReturnType),
                    [#sp_function{args = AFunArgTypes, return = AReturnType}]
            end;
        arity ->
            [
                #sp_range{
                    type = integer,
                    lower_bound = 0,
                    upper_bound = 255
                }
            ];
        byte ->
            [
                #sp_range{
                    type = integer,
                    lower_bound = 0,
                    upper_bound = 255
                }
            ];
        char ->
            [
                #sp_range{
                    type = integer,
                    lower_bound = 0,
                    upper_bound = 16#10ffff
                }
            ];
        mfa ->
            [
                #sp_tuple{
                    fields =
                        [
                            #sp_simple_type{type = atom},
                            #sp_simple_type{type = atom},
                            #sp_range{
                                type = integer,
                                lower_bound = 0,
                                upper_bound = 255
                            }
                        ]
                }
            ];
        any ->
            [#sp_simple_type{type = term}];
        timeout ->
            [
                #sp_union{
                    types =
                        [
                            #sp_simple_type{type = non_neg_integer},
                            #sp_literal{value = infinity}
                        ]
                }
            ];
        pid ->
            [#sp_simple_type{type = pid}];
        iodata ->
            [#sp_simple_type{type = iodata}];
        iolist ->
            [#sp_simple_type{type = iolist}];
        port ->
            [#sp_simple_type{type = port}];
        reference ->
            [#sp_simple_type{type = reference}];
        node ->
            [#sp_simple_type{type = atom}];
        identifier ->
            [
                #sp_union{
                    types =
                        [
                            #sp_simple_type{type = pid},
                            #sp_simple_type{type = port},
                            #sp_simple_type{type = reference}
                        ]
                }
            ];
        range ->
            [MinValue, MaxValue] = TypeAttrs,
            Min = integer_value(MinValue),
            Max = integer_value(MaxValue),
            [
                #sp_range{
                    type = integer,
                    lower_bound = Min,
                    upper_bound = Max
                }
            ];
        list ->
            case lists:flatmap(fun field_info_to_type/1, TypeAttrs) of
                [ListType] ->
                    [#sp_list{type = ListType}];
                [] ->
                    [#sp_list{type = #sp_simple_type{type = term}}]
            end;
        nonempty_list ->
            case lists:flatmap(fun field_info_to_type/1, TypeAttrs) of
                [ListType] ->
                    [#sp_nonempty_list{type = ListType}];
                [] ->
                    [#sp_nonempty_list{type = #sp_simple_type{type = term}}]
            end;
        maybe_improper_list ->
            case lists:flatmap(fun field_info_to_type/1, TypeAttrs) of
                [Elements, Tail] ->
                    [#sp_maybe_improper_list{elements = Elements, tail = Tail}];
                [] ->
                    [
                        #sp_maybe_improper_list{
                            elements = #sp_simple_type{type = term},
                            tail = #sp_simple_type{type = term}
                        }
                    ]
            end;
        nonempty_improper_list ->
            [Elements, Tail] = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [#sp_nonempty_improper_list{elements = Elements, tail = Tail}];
        module ->
            [#sp_simple_type{type = atom}];
        PrimaryType when ?is_primary_type(PrimaryType) ->
            [#sp_simple_type{type = PrimaryType}];
        PartailRangeInteger when ?is_predefined_int_range(PartailRangeInteger) ->
            [#sp_simple_type{type = PartailRangeInteger}];
        bitstring ->
            [#sp_simple_type{type = bitstring}];
        nonempty_bitstring ->
            [#sp_simple_type{type = nonempty_bitstring}];
        dynamic ->
            [#sp_simple_type{type = term}];
        nil ->
            [#sp_literal{value = []}];
        none ->
            [#sp_simple_type{type = none}];
        no_return ->
            [#sp_simple_type{type = none}]
    end.

integer_value({char, _, Value}) when is_integer(Value) ->
    Value;
integer_value({integer, _, Value}) when is_integer(Value) ->
    Value;
integer_value({op, _, Operator, Left, Right}) ->
    case Operator of
        '-' ->
            integer_value(Left) - integer_value(Right);
        '+' ->
            integer_value(Left) + integer_value(Right);
        '*' ->
            integer_value(Left) * integer_value(Right);
        'div' ->
            integer_value(Left) div integer_value(Right);
        'rem' ->
            integer_value(Left) rem integer_value(Right);
        'band' ->
            integer_value(Left) band integer_value(Right);
        'bor' ->
            integer_value(Left) bor integer_value(Right);
        'bxor' ->
            integer_value(Left) bxor integer_value(Right);
        'bsl' ->
            integer_value(Left) bsl integer_value(Right);
        'bsr' ->
            integer_value(Left) bsr integer_value(Right)
    end;
integer_value({op, _, Operator, Unary}) ->
    case Operator of
        '-' ->
            -integer_value(Unary);
        '+' ->
            integer_value(Unary);
        'bnot' ->
            bnot integer_value(Unary)
    end.

-spec map_field_info(term()) ->
    [
        {map_field_assoc | map_field_exact, atom(), spectra:sp_type()}
        | {map_field_type_assoc | map_field_type_exact, spectra:sp_type(), spectra:sp_type()}
    ].
map_field_info({TypeOfType, _, Type, TypeAttrs}) ->
    case {TypeOfType, Type} of
        {type, map_field_assoc} ->
            case TypeAttrs of
                [{atom, _, MapFieldName}, FieldInfo] when is_atom(MapFieldName) ->
                    [AType] = field_info_to_type(FieldInfo),
                    [{map_field_assoc, MapFieldName, AType}];
                [KeyFieldInfo, ValueFieldInfo] ->
                    [KeyType] = field_info_to_type(KeyFieldInfo),
                    [ValueType] = field_info_to_type(ValueFieldInfo),
                    [{map_field_type_assoc, KeyType, ValueType}]
            end;
        {type, map_field_exact} ->
            case TypeAttrs of
                [{atom, _, MapFieldName}, FieldInfo] ->
                    %%                    beam_core_to_ssa:format_error(Arg1),
                    true = is_atom(MapFieldName),
                    [AType] = field_info_to_type(FieldInfo),
                    [{map_field_exact, MapFieldName, AType}];
                [KeyFieldInfo, ValueFieldInfo] ->
                    [KeyType] = field_info_to_type(KeyFieldInfo),
                    [ValueType] = field_info_to_type(ValueFieldInfo),
                    [{map_field_type_exact, KeyType, ValueType}]
            end
    end.

-spec record_field_info(erl_parse__af_field_decl()) -> {atom(), spectra:sp_type()}.
record_field_info({record_field, _, {atom, _, FieldName}, _Type}) when
    is_atom(FieldName)
->
    %% FIXME: Handle default values in record fields. Also handle default values in typed_record_field?
    {FieldName, #sp_simple_type{type = term}};
record_field_info({record_field, _, {atom, _, FieldName}}) when is_atom(FieldName) ->
    {FieldName, #sp_simple_type{type = term}};
record_field_info({typed_record_field, {record_field, _, {atom, _, FieldName}}, Type}) when
    is_atom(FieldName)
->
    [TypeInfo] = field_info_to_type(Type),
    {FieldName, TypeInfo};
record_field_info({typed_record_field, {record_field, _, {atom, _, FieldName}, _}, Type}) when
    is_atom(FieldName)
->
    [TypeInfo] = field_info_to_type(Type),
    {FieldName, TypeInfo}.

%% Helper functions for bounded_fun handling
-spec bound_fun_constraints(list()) -> #{atom() => term()}.
bound_fun_constraints(Constraints) ->
    lists:foldl(fun bound_fun_constraint_aux/2, #{}, Constraints).

bound_fun_constraint_aux(
    {type, _, constraint, [{atom, _, is_subtype}, [{var, _, VarName}, TypeDef]]},
    Acc
) when
    is_atom(VarName)
->
    maps:put(VarName, TypeDef, Acc);
bound_fun_constraint_aux(Constraint, _Acc) ->
    error({unsupported_constraint, Constraint}).

bound_fun_substitute_vars({var, _, VarName} = Var, ConstraintMap) when is_atom(VarName) ->
    maps:get(VarName, ConstraintMap, Var);
bound_fun_substitute_vars(Term, _ConstraintMap) ->
    Term.

%% Helper function to extract struct name from map fields for Elixir structs
-spec extract_struct_name([spectra:map_field()]) ->
    {undefined | atom(), [spectra:map_field()]}.
extract_struct_name(MapFields) ->
    case
        lists:partition(
            fun
                ({map_field_exact, '__struct__', _}) ->
                    true;
                ({map_field_assoc, '__struct__', _}) ->
                    true;
                (_) ->
                    false
            end,
            MapFields
        )
    of
        {[{_, '__struct__', #sp_literal{value = SName}}], OtherMapFields} when is_atom(SName) ->
            {SName, OtherMapFields};
        {[], _} ->
            {undefined, MapFields}
    end.
