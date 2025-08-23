-module(erldantic_type_info).

-include("../include/erldantic_internal.hrl").

%% Construction
-export([new/0, from_legacy_map/1]).
%% Type operations
-export([add_type/4, get_type/3, has_type/3, list_types/1]).
%% Record operations
-export([add_record/3, get_record/2, has_record/2, list_records/1]).
%% Function operations
-export([add_function/4, get_function/3, has_function/3, list_functions/1]).
%% Utility functions
-export([to_legacy_map/1, merge/2]).

%% Types
-export_type([type_info/0, type_key/0, function_key/0]).

-type type_info() :: #type_info{}.
-type type_key() :: {Name :: atom(), Arity :: arity()}.
-type function_key() :: {Name :: atom(), Arity :: arity()}.

%% Construction functions

-spec new() -> type_info().
new() ->
    #type_info{}.

-spec from_legacy_map(map()) -> type_info().
from_legacy_map(LegacyMap) ->
    maps:fold(fun from_legacy_map_fold/3, new(), LegacyMap).

from_legacy_map_fold({type, Name, Arity}, Type, Acc) ->
    add_type(Acc, Name, Arity, Type);
from_legacy_map_fold({record, Name}, Record, Acc) ->
    add_record(Acc, Name, Record);
from_legacy_map_fold({function, Name, Arity}, FuncSpec, Acc) ->
    add_function(Acc, Name, Arity, FuncSpec).

%% Type operations

-spec add_type(type_info(), atom(), arity(), erldantic:ed_type()) -> type_info().
add_type(#type_info{types = Types} = TypeInfo, Name, Arity, Type) ->
    TypeInfo#type_info{types = Types#{{Name, Arity} => Type}}.

-spec get_type(type_info(), atom(), arity()) -> {ok, erldantic:ed_type()} | error.
get_type(#type_info{types = Types}, Name, Arity) ->
    case maps:find({Name, Arity}, Types) of
        {ok, Type} ->
            {ok, Type};
        error ->
            error
    end.

-spec has_type(type_info(), atom(), arity()) -> boolean().
has_type(#type_info{types = Types}, Name, Arity) ->
    maps:is_key({Name, Arity}, Types).

-spec list_types(type_info()) -> [type_key()].
list_types(#type_info{types = Types}) ->
    maps:keys(Types).

%% Record operations

-spec add_record(type_info(), atom(), erldantic:ed_type()) -> type_info().
add_record(#type_info{records = Records} = TypeInfo, Name, Record) ->
    TypeInfo#type_info{records = Records#{Name => Record}}.

-spec get_record(type_info(), atom()) -> {ok, erldantic:ed_type()} | error.
get_record(#type_info{records = Records}, Name) ->
    case maps:find(Name, Records) of
        {ok, Record} ->
            {ok, Record};
        error ->
            error
    end.

-spec has_record(type_info(), atom()) -> boolean().
has_record(#type_info{records = Records}, Name) ->
    maps:is_key(Name, Records).

-spec list_records(type_info()) -> [atom()].
list_records(#type_info{records = Records}) ->
    maps:keys(Records).

%% Function operations

-spec add_function(type_info(), atom(), arity(), erldantic:ed_function_spec()) ->
                      type_info().
add_function(#type_info{functions = Functions} = TypeInfo, Name, Arity, FuncSpec) ->
    TypeInfo#type_info{functions = Functions#{{Name, Arity} => FuncSpec}}.

-spec get_function(type_info(), atom(), arity()) ->
                      {ok, erldantic:ed_function_spec()} | error.
get_function(#type_info{functions = Functions}, Name, Arity) ->
    case maps:find({Name, Arity}, Functions) of
        {ok, FuncSpec} ->
            {ok, FuncSpec};
        error ->
            error
    end.

-spec has_function(type_info(), atom(), arity()) -> boolean().
has_function(#type_info{functions = Functions}, Name, Arity) ->
    maps:is_key({Name, Arity}, Functions).

-spec list_functions(type_info()) -> [function_key()].
list_functions(#type_info{functions = Functions}) ->
    maps:keys(Functions).

%% Utility functions

-spec to_legacy_map(type_info()) -> map().
to_legacy_map(#type_info{types = Types,
                         records = Records,
                         functions = Functions}) ->
    TypesMap =
        maps:fold(fun({Name, Arity}, Type, Acc) -> Acc#{{type, Name, Arity} => Type} end,
                  #{},
                  Types),

    RecordsMap =
        maps:fold(fun(Name, Record, Acc) -> Acc#{{record, Name} => Record} end,
                  TypesMap,
                  Records),

    maps:fold(fun({Name, Arity}, FuncSpec, Acc) -> Acc#{{function, Name, Arity} => FuncSpec}
              end,
              RecordsMap,
              Functions).

-spec merge(type_info(), type_info()) -> type_info().
merge(#type_info{types = Types1,
                 records = Records1,
                 functions = Functions1},
      #type_info{types = Types2,
                 records = Records2,
                 functions = Functions2}) ->
    #type_info{types = maps:merge(Types1, Types2),
               records = maps:merge(Records1, Records2),
               functions = maps:merge(Functions1, Functions2)}.
