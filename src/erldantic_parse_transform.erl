-module(erldantic_parse_transform).

-export([parse_transform/2, type_in_form/1]).

-include("../include/record_type_introspect.hrl").

-compile({parse_transform, parse_trans_codegen}).

parse_transform(Forms, _Options) ->
    io:format("Inspecting records at compile-time...~n"),

    NamedTypes = lists:filtermap(fun type_in_form/1, Forms),

    trans_add_to_from_json(Forms, NamedTypes).

trans_add_to_from_json(Forms, NamedTypes) ->
    {BeforeFunctions, Functions} =
        lists:splitwith(fun ({function, _Pos, _F, _A, _Body}) ->
                                false;
                            ({attribute, _, spec, _}) ->
                                false;
                            (_) ->
                                true
                        end,
                        Forms),
    {BeforeExports, ExportsUntilFunctions} =
        lists:splitwith(fun ({attribute, _, export, _}) ->
                                false;
                            (_) ->
                                true
                        end,
                        BeforeFunctions),
    %io:format("Before Exports: ~p~n", [BeforeExports]),
    %io:format("Exports Until Functions: ~p~n", [ExportsUntilFunctions]),
    %io:format("Functions: ~p~n", [Functions]),
    BeforeExports
    ++ named_types_to_exports(NamedTypes)
    ++ ExportsUntilFunctions
    ++ named_types_to_functions(NamedTypes)
    ++ Functions.

named_types_to_functions(NamedTypes) ->
    % {NewNamedTypes, _} =
    %     lists:mapfoldl(fun({{TypeOfThing, TypeName}, Info}, Acc) ->
    %                       Name =
    %                           case sets:is_element(TypeName, Acc) of
    %                               true ->
    %                                   binary_to_atom(iolist_to_binary([TypeOfThing, "_", TypeName]),
    %                                                  utf8);
    %                               _ -> TypeName
    %                           end,
    %                       {{Name, Info}, sets:add_element(Name, Acc)}
    %                    end,
    %                    sets:new([{version, 2}]),
    %                    NamedTypes),
    % [].
    M = maps:from_list(NamedTypes),
    lists:flatmap(fun(A) -> named_type_to_function(A, M) end, NamedTypes).

named_types_to_exports(NamedTypes) ->
    Exports =
        lists:flatmap(fun({{_, TypeName}, _}) ->
                         [{function_name(TypeName, "_from_json"), 1},
                          {function_name(TypeName, "_to_json"), 1}]
                      end,
                      NamedTypes),
    [{attribute, {5, 2}, export, Exports}].

named_type_to_function({{TypeOfThing, TypeName}, _Info}, Infos) ->
    [codegen:gen_function(function_name(TypeName, "_from_json"),
                          fun(Json) ->
                             record_type_introspect:from_json({'$var', Infos},
                                                              {{'$var', TypeOfThing},
                                                               {'$var', TypeName}},
                                                              Json)
                          end),
     codegen:gen_function(function_name(TypeName, "_to_json"),
                          fun(Data) ->
                             record_type_introspect:to_json({'$var', Infos},
                                                            {{'$var', TypeOfThing},
                                                             {'$var', TypeName}},
                                                            Data)
                          end)].

function_name(TypeName, PostFix) ->
    binary_to_atom(iolist_to_binary([atom_to_list(TypeName), PostFix])).

-spec type_in_form(term()) ->
                      false |
                      {true,
                       {record_type_introspect:a_type_reference(),
                        record_type_introspect:a_type()}}.
type_in_form({attribute, _, record, {RecordName, Fields}})
    when is_list(Fields) andalso is_atom(RecordName) ->
    FieldInfos = lists:map(fun record_field_info/1, Fields),
    {true, {{record, RecordName}, #a_rec{name = RecordName, fields = FieldInfos}}};
type_in_form({attribute, _, type, {TypeName, {type, _, record, Attrs}, []}})
    when is_atom(TypeName) ->
    [{atom, _, RecordName} | FieldInfo] = Attrs,
    true = is_atom(RecordName),
    FieldTypes =
        lists:map(fun({type, _, field_type, [{atom, _, FieldName}, RowFieldInfo]})
                         when is_atom(FieldName) ->
                     [A] = field_info_to_type(RowFieldInfo),
                     {FieldName, A}
                  end,
                  FieldInfo),
    {true, {{type, TypeName}, #a_rec{name = RecordName, fields = FieldTypes}}};
type_in_form({attribute, _, type, {TypeName, {type, _, _, _} = Type, []}})
    when is_atom(TypeName) ->
    [FieldInfo] = field_info_to_type(Type),
    {true, {{type, TypeName}, FieldInfo}};
type_in_form(_) ->
    false.

-spec field_info_to_type(term()) -> [record_type_introspect:a_type()].
field_info_to_type({ann_type, _, _Attr}) ->
    [];
field_info_to_type({atom, _, Value}) ->
    [{literal, Value}];
field_info_to_type({integer, _, Value}) ->
    [{literal, Value}];
field_info_to_type({remote_type, _, MTA}) ->
    [{atom, _, Module}, {atom, _, Type}, Args] = MTA,
    [{remote_type, {Module, Type, Args}}];
field_info_to_type({TypeOfType, _, Type, TypeAttrs}) ->
    true = is_list(TypeAttrs),
    case {TypeOfType, Type} of
        {type, record} ->
            [{atom, _, SubTypeRecordName}] = TypeAttrs,
            true = is_atom(SubTypeRecordName),
            [{record_ref, SubTypeRecordName}];
        {user_type, Type} ->
            true = is_atom(Type),
            [{user_type_ref, Type}];
        {type, map} ->
            MapFields = lists:flatmap(fun map_field_info/1, TypeAttrs),
            [#a_map{fields = MapFields}];
        {type, tuple} ->
            TupleFields = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [#a_tuple{fields = TupleFields}];
        {type, union} ->
            UnionFields = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [{union, UnionFields}];
        {type, range} ->
            [{RangeType, _, Min}, {RangeType, _, Max}] = TypeAttrs,
            case {RangeType, Min, Max} of
                {integer, Min, Max} when is_integer(Min), is_integer(Max) ->
                    [{range, RangeType, Min, Max}]
            end;
        {type, list} ->
            [ListType] = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [{list, ListType}];
        {type, nonempty_list} ->
            [ListType] = lists:flatmap(fun field_info_to_type/1, TypeAttrs),
            [{nonempty_list, ListType}];
        {type, PrimaryType} when ?is_primary_type(PrimaryType) ->
            [{type, PrimaryType}];
        {type, PartailRangeInteger} when ?is_predefined_int_range(PartailRangeInteger) ->
            [{type, PartailRangeInteger}];
        {type, term} ->
            [{type, term}];
        {literal, Literal} ->
            [{literal, Literal}]
    end.

-spec map_field_info(term()) ->
                        [{map_field_assoc | map_field_exact,
                          atom(),
                          record_type_introspect:a_type()}].
map_field_info({TypeOfType, _, Type, TypeAttrs}) ->
    case {TypeOfType, Type} of
        {type, map_field_assoc} ->
            [{atom, _, MapFieldName}, FieldInfo] = TypeAttrs,
            true = is_atom(MapFieldName),
            [AType] = field_info_to_type(FieldInfo),
            [{map_field_assoc, MapFieldName, AType}];
        {type, map_field_exact} ->
            [{atom, _, MapFieldName}, FieldInfo] = TypeAttrs,
            true = is_atom(MapFieldName),
            [AType] = field_info_to_type(FieldInfo),
            [{map_field_exact, MapFieldName, AType}]
    end.

-spec record_field_info(term()) -> {atom(), record_type_introspect:a_type()}.
record_field_info({typed_record_field, {record_field, _, {atom, _, FieldName}}, Type})
    when is_atom(FieldName) ->
    [TypeInfo] = field_info_to_type(Type),
    {FieldName, TypeInfo}.
