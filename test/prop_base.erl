-module(prop_base).

-include_lib("proper/include/proper.hrl").

-type other_thingy() :: this | that.

-record(my_record, {id, data :: string(), d2 :: [other_thingy()], d3 :: string()}).

-type my_t() :: #my_record{id :: integer()}.

prop_hej() ->
    ?FORALL(Map,
            my_t(),
            begin
                {ok, Value} = erldantic_json:type_to_json(?MODULE, my_t, Map),
                Json = iolist_to_binary(json:encode(Value)),
                Value2 = json:decode(Json),
                Res = erldantic_json:type_from_json(?MODULE, my_t, Value2),
                ?WHENFAIL(io:format("Map   ~p~nValue ~p~nJson   ~p~nValue2 ~p~nRes   ~p~n",
                                    [Map, Value, Json, Value2, Res]),
                          Res =:= {ok, Map})
            end).
