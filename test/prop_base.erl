-module(prop_base).

-include_lib("proper/include/proper.hrl").

prop_hej() ->
    ?FORALL(
        {{TypeName, Type}, JsonValue},
        {test_type(), json_generator:json_value()},
        begin
            TypeInfo = #{{type, TypeName} => Type},
            case from_json(TypeInfo, Type, JsonValue) of
                {ok, Data} ->
                    case erldantic_json:to_json(TypeInfo, Type, Data) of
                        {ok, Value} ->
                            Json = iolist_to_binary(json:encode(Value)),
                            ?WHENFAIL(
                                io:format(
                                    "~nJsonValue ~p~nValue    ~p~nJson   ~p",
                                    [JsonValue, Value, Json]
                                ),
                                Json =/= JsonValue
                            );
                        {error, Reason} ->
                            io:format(
                                "~nFailed to_json~n Type ~p~n Json ~p~n Data ~p~n Reason ~p",
                                [Type, JsonValue, Data, Reason]
                            ),
                            collect(failed_to_json, false)
                    end;
                {error, _} ->
                    collect(json_dont_match_type, true)
            end
        end
    ).

from_json(TypeInfo, Type, JsonValue) ->
    try
        erldantic_json:from_json(TypeInfo, Type, JsonValue)
    catch
        error:{type_not_supported, _} ->
            {error, type_not_supported};
        error:{type_not_implemented, _} ->
            {error, type_not_implemented};
        error:{module_types_not_found, _} ->
            {error, module_types_not_found};
        error:{type_not_found, _} ->
            {error, {type_not_found}};
        error:{record_not_found, _} ->
            {error, {record_not_found}}
    end.

test_type() ->
    {my_type, ed_type_generators:ed_type()}.
