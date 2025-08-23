-module(erldantic_abstract_code).

-include("../include/erldantic_internal.hrl").

-export([types_in_module/1]).

-define(is_primary_type(PrimaryType),
        PrimaryType =:= string
        orelse PrimaryType =:= nonempty_string
        orelse PrimaryType =:= integer
        orelse PrimaryType =:= boolean
        orelse PrimaryType =:= atom
        orelse PrimaryType =:= float
        orelse PrimaryType =:= binary
        orelse PrimaryType =:= nonempty_binary
        orelse PrimaryType =:= number
        orelse PrimaryType =:= term).
-define(is_predefined_int_range(_Type),
        _Type =:= non_neg_integer orelse _Type =:= neg_integer orelse _Type =:= pos_integer).

%% Due to erl_parse:af_wild_attribute() I can't use some of the types.
-type erl_parse__af_field_decl() :: term().

-spec types_in_module(atom()) -> erldantic:type_info().
types_in_module(Module) ->
    case code:which(Module) of
        Error
            when Error =:= non_existing
                 orelse Error =:= cover_compiled
                 orelse Error =:= preloaded ->
            erlang:error({module_types_not_found, Module, Error});
        FilePath ->
            case beam_lib:chunks(FilePath, [abstract_code]) of
                {ok, {Module, [{abstract_code, {_, Forms}}]}} ->
                    NamedTypes = lists:filtermap(fun(F) -> type_in_form(F) end, Forms),
                    build_type_info(NamedTypes);
                {error, beam_lib, Reason} ->
                    erlang:error({beam_lib_error, Module, Reason})
            end
    end.

build_type_info(NamedTypes) ->
    lists:foldl(fun build_type_info_fold/2, erldantic_type_info:new(), NamedTypes).

build_type_info_fold({{type, Name, Arity}, Type}, TypeInfo) ->
    erldantic_type_info:add_type(TypeInfo, Name, Arity, Type);
build_type_info_fold({{record, Name}, Record}, TypeInfo) ->
    erldantic_type_info:add_record(TypeInfo, Name, Record);
build_type_info_fold({{function, Name, Arity}, FuncSpec}, TypeInfo) ->
    erldantic_type_info:add_function(TypeInfo, Name, Arity, FuncSpec).

-spec type_in_form(erl_parse:abstract_form() | erl_parse:form_info()) ->
                      false | {true, type_form_result()}.
-type type_form_result() ::
    {{type, atom(), arity()}, erldantic:ed_type()} |
    {{record, atom()}, erldantic:ed_type()} |
    {{function, atom(), arity()}, erldantic:ed_function_spec()}.

type_in_form({attribute, _, record, {_RecordName, []} = T}) ->
    error({not_supported, T});
type_in_form({attribute, _, record, {RecordName, Fields}})
    when is_list(Fields) andalso is_atom(RecordName) ->
    FieldInfos = lists:map(fun record_field_info/1, Fields),
    {true,
     {{record, RecordName},
      #ed_rec{name = RecordName,
              fields = FieldInfos,
              arity = length(FieldInfos) + 1}}};
type_in_form({attribute, _, TypeOrOpaque, {TypeName, {_, _, record, Attrs}, [] = Args}})
    when is_atom(TypeName) andalso (TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque) ->
    %% FIXME: Sort out why this function clause differs from all others.
    true = is_list(Attrs),
    {RecordName, FieldTypes} = record_field_types(Attrs),
    TypeArity = length(Args),
    Record = #ed_rec_ref{record_name = RecordName, field_types = FieldTypes},
    {true, {{type, TypeName, TypeArity}, Record}};
type_in_form({attribute, _, TypeOrOpaque, {TypeName, Type, Args}})
    when is_atom(TypeName)
         andalso is_list(Args)
         andalso (TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque) ->
    [FieldInfo] = field_info_to_type(Type),
    Vars = lists:map(fun({var, _, VarName}) when is_atom(VarName) -> VarName end, Args),
    TypeArity = length(Args),
    case Vars of
        [] ->
            {true, {{type, TypeName, TypeArity}, FieldInfo}};
        _ ->
            {true,
             {{type, TypeName, TypeArity}, #ed_type_with_variables{type = FieldInfo, vars = Vars}}}
    end;
type_in_form({attribute, _, spec, {{FunctionName, Arity}, [{type, _, 'fun', [{type, _, product, Args}, ReturnType]}]}})
    when is_atom(FunctionName) andalso is_integer(Arity) andalso is_list(Args) ->
    ArgTypes =
        lists:map(fun(Arg) ->
                     [ArgType] = field_info_to_type(Arg),
                     ArgType
                  end,
                  Args),
    [ReturnTypeProcessed] = field_info_to_type(ReturnType),
    {true,
     {{function, FunctionName, Arity},
      #ed_function_spec{args = ArgTypes, return = ReturnTypeProcessed}}};
type_in_form({attribute, _, TypeOrOpaque, _} = T)
    when TypeOrOpaque =:= opaque orelse TypeOrOpaque =:= type ->
    error({not_supported, T});
type_in_form(_) ->
    false.

-spec record_field_types(list()) -> {atom(), [{atom(), erldantic:ed_type()}]}.
record_field_types(Attrs) ->
    [{atom, _, RecordName} | FieldInfo] = Attrs,
    true = is_atom(RecordName),
    FieldTypes =
        lists:map(fun({type, _, field_type, [{atom, _, FieldName}, RowFieldInfo]})
                         when is_atom(FieldName) ->
                     [FieldType] = field_info_to_type(RowFieldInfo),
                     {FieldName, FieldType}
                  end,
                  FieldInfo),
    {RecordName, FieldTypes}.

-spec field_info_to_type(term()) -> [erldantic:ed_type()].
field_info_to_type({ann_type, _, [{var, _, _VarName}, Type]}) ->
    field_info_to_type(Type);
field_info_to_type({atom, _, Value}) when is_atom(Value) ->
    [#ed_literal{value = Value}];
field_info_to_type({integer, _, Value}) when is_integer(Value) ->
    [#ed_literal{value = Value}];
field_info_to_type(Op) when element(1, Op) =:= op ->
    Value = integer_value(Op),
    [#ed_literal{value = Value}];
field_info_to_type({var, _, VarName}) when is_atom(VarName) ->
    [#ed_var{name = VarName}];
field_info_to_type({remote_type, _, [{atom, _, Module}, {atom, _, Type}, Args]})
    when is_atom(Module) andalso is_atom(Type) andalso is_list(Args) ->
    MyArgs =
        lists:map(fun(Arg) ->
                     [ArgType] = field_info_to_type(Arg),
                     ArgType
                  end,
                  Args),
    [#ed_remote_type{mfargs = {Module, Type, MyArgs}}];
field_info_to_type({Type, _, map, any}) when Type =:= type orelse Type =:= opaque ->
    [#ed_map{fields =
                 [{map_field_type_assoc,
                   #ed_simple_type{type = term},
                   #ed_simple_type{type = term}}]}];
field_info_to_type({Type, _, tuple, any}) when Type =:= type orelse Type =:= opaque ->
    [#ed_tuple{fields = any}];
field_info_to_type({user_type, _, Type, TypeAttrs})
    when is_atom(Type) andalso is_list(TypeAttrs) ->
    TAttrs = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
    [#ed_user_type_ref{type_name = Type, variables = TAttrs}];
field_info_to_type({TypeOrOpaque, _, Type, TypeAttrs})
    when is_list(TypeAttrs) andalso (TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque) ->
    case Type of
        record ->
            {SubTypeRecordName, FieldTypes} = record_field_types(TypeAttrs),
            [#ed_rec_ref{record_name = SubTypeRecordName, field_types = FieldTypes}];
        map ->
            MapFields = lists:flatmap(fun map_field_info/1, TypeAttrs),
            [#ed_map{fields = MapFields}];
        tuple ->
            TupleFields = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [#ed_tuple{fields = TupleFields}];
        union ->
            UnionFields = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [#ed_union{types = UnionFields}];
        Fun when Fun =:= 'fun' orelse Fun =:= function ->
            case TypeAttrs of
                [] ->
                    [#ed_function{args = any, return = #ed_simple_type{type = term}}];
                [{type, _, any}, ReturnType] ->
                    [AReturnType] = field_info_to_type(ReturnType),
                    [#ed_function{args = any, return = AReturnType}];
                [{type, _, product, FunArgTypes}, ReturnType] ->
                    true = is_list(FunArgTypes),
                    AFunArgTypes = lists:flatmap(fun field_info_to_type/1, FunArgTypes),
                    [AReturnType] = field_info_to_type(ReturnType),
                    [#ed_function{args = AFunArgTypes, return = AReturnType}]
            end;
        arity ->
            [#ed_range{type = integer,
                       lower_bound = 0,
                       upper_bound = 255}];
        byte ->
            [#ed_range{type = integer,
                       lower_bound = 0,
                       upper_bound = 255}];
        char ->
            [#ed_range{type = integer,
                       lower_bound = 0,
                       upper_bound = 16#10ffff}];
        mfa ->
            [#ed_tuple{fields =
                           [#ed_simple_type{type = atom},
                            #ed_simple_type{type = atom},
                            #ed_range{type = integer,
                                      lower_bound = 0,
                                      upper_bound = 255}]}];
        any ->
            [#ed_simple_type{type = term}];
        timeout ->
            [#ed_union{types =
                           [#ed_simple_type{type = non_neg_integer},
                            #ed_literal{value = infinity}]}];
        pid ->
            [#ed_simple_type{type = pid}];
        iodata ->
            [#ed_simple_type{type = iodata}];
        iolist ->
            [#ed_simple_type{type = iolist}];
        port ->
            [#ed_simple_type{type = port}];
        reference ->
            [#ed_simple_type{type = reference}];
        node ->
            [#ed_simple_type{type = atom}];
        identifier ->
            [#ed_union{types =
                           [#ed_simple_type{type = pid},
                            #ed_simple_type{type = port},
                            #ed_simple_type{type = reference}]}];
        range ->
            [MinValue, MaxValue] = TypeAttrs,
            Min = integer_value(MinValue),
            Max = integer_value(MaxValue),
            [#ed_range{type = integer,
                       lower_bound = Min,
                       upper_bound = Max}];
        list ->
            case lists:flatmap(fun field_info_to_type/1, TypeAttrs) of
                [ListType] ->
                    [#ed_list{type = ListType}];
                [] ->
                    [#ed_list{type = #ed_simple_type{type = term}}]
            end;
        nonempty_list ->
            case lists:flatmap(fun field_info_to_type/1, TypeAttrs) of
                [ListType] ->
                    [#ed_nonempty_list{type = ListType}];
                [] ->
                    [#ed_nonempty_list{type = #ed_simple_type{type = term}}]
            end;
        maybe_improper_list ->
            case lists:flatmap(fun field_info_to_type/1, TypeAttrs) of
                [Elements, Tail] ->
                    [#ed_maybe_improper_list{elements = Elements, tail = Tail}];
                [] ->
                    [#ed_maybe_improper_list{elements = #ed_simple_type{type = term},
                                             tail = #ed_simple_type{type = term}}]
            end;
        nonempty_improper_list ->
            [Elements, Tail] = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [#ed_nonempty_improper_list{elements = Elements, tail = Tail}];
        module ->
            [#ed_simple_type{type = atom}];
        PrimaryType when ?is_primary_type(PrimaryType) ->
            [#ed_simple_type{type = PrimaryType}];
        PartailRangeInteger when ?is_predefined_int_range(PartailRangeInteger) ->
            [#ed_simple_type{type = PartailRangeInteger}];
        bitstring ->
            [#ed_simple_type{type = bitstring}];
        nonempty_bitstring ->
            [#ed_simple_type{type = nonempty_bitstring}];
        dynamic ->
            [#ed_simple_type{type = term}];
        nil ->
            [#ed_literal{value = []}];
        none ->
            [#ed_simple_type{type = none}];
        no_return ->
            [#ed_simple_type{type = none}]
    end.

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
                        [{map_field_assoc | map_field_exact, atom(), erldantic:ed_type()} |
                         {map_field_type_assoc | map_field_type_exact,
                          erldantic:ed_type(),
                          erldantic:ed_type()}].
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

-spec record_field_info(erl_parse__af_field_decl()) -> {atom(), erldantic:ed_type()}.
record_field_info({record_field, _, {atom, _, FieldName}, _Type})
    when is_atom(FieldName) ->
    %% FIXME: Handle default values in record fields. Also handle default values in typed_record_field?
    {FieldName, #ed_simple_type{type = term}};
record_field_info({record_field, _, {atom, _, FieldName}}) when is_atom(FieldName) ->
    {FieldName, #ed_simple_type{type = term}};
record_field_info({typed_record_field, {record_field, _, {atom, _, FieldName}}, Type})
    when is_atom(FieldName) ->
    [TypeInfo] = field_info_to_type(Type),
    {FieldName, TypeInfo};
record_field_info({typed_record_field, {record_field, _, {atom, _, FieldName}, _}, Type})
    when is_atom(FieldName) ->
    [TypeInfo] = field_info_to_type(Type),
    {FieldName, TypeInfo}.
