-module(json_generator).

-include_lib("proper/include/proper.hrl").

-export([
    json_value/0,
    json_object/0,
    json_array/0,
    json_string/0,
    json_number/0,
    json_boolean/0,
    json_null/0
]).

%% Main generator for any JSON-encodable value
-spec json_value() -> proper_types:type().
json_value() ->
    ?SIZED(Size, json_value(Size)).

json_value(0) ->
    oneof([json_null(), json_boolean(), json_number(), json_string()]);
json_value(Size) ->
    oneof([
        json_null(),
        json_boolean(),
        json_number(),
        json_string(),
        json_array(Size div 2),
        json_object(Size div 2)
    ]).

%% JSON null value
-spec json_null() -> proper_types:type().
json_null() ->
    null.

%% JSON boolean values
-spec json_boolean() -> proper_types:type().
json_boolean() ->
    boolean().

%% JSON numbers (integers and floats)
-spec json_number() -> proper_types:type().
json_number() ->
    oneof([
        integer(),
        float(),
        % Special float values that JSON can handle
        oneof([0.0, -0.0, 1.0, -1.0])
    ]).

%% JSON strings (UTF-8 encoded)
-spec json_string() -> proper_types:type().
json_string() ->
    % Empty string
    oneof([
        <<>>,
        % UTF-8 binary strings using PropEr's built-in generator
        utf8()
    ]).

%% JSON arrays
-spec json_array() -> proper_types:type().
json_array() ->
    json_array(3).

json_array(Size) ->
    ?LET(Len, choose(0, Size), vector(Len, json_value(Size))).

%% JSON objects (maps with binary string keys)
-spec json_object() -> proper_types:type().
json_object() ->
    json_object(3).

json_object(Size) ->
    ?LET(
        Len,
        choose(0, Size),
        ?LET(Pairs, vector(Len, {json_string(), json_value(Size)}), maps:from_list(Pairs))
    ).
