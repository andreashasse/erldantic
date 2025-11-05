-module(spectra_type_info).

-include("../include/spectra_internal.hrl").

-ignore_xref([get_function/3]).

-export([new/0]).
-export([add_type/4, get_type/3, all_types/1, update_type/4]).
-export([add_record/3, get_record/2, all_records/1, update_record/3]).
-export([add_function/4, get_function/3]).

-export_type([type_info/0, type_key/0, function_key/0]).

-type type_info() :: #type_info{}.
-type type_key() :: {Name :: atom(), Arity :: arity()}.
-type function_key() :: {Name :: atom(), Arity :: arity()}.

-spec new() -> type_info().
new() ->
    #type_info{}.

-spec add_type(type_info(), atom(), arity(), spectra:sp_type()) -> type_info().
add_type(#type_info{types = Types} = TypeInfo, Name, Arity, Type) ->
    TypeInfo#type_info{types = Types#{{Name, Arity} => Type}}.

-spec get_type(type_info(), atom(), arity()) -> {ok, spectra:sp_type()} | error.
get_type(#type_info{types = Types}, Name, Arity) ->
    maps:find({Name, Arity}, Types).

-spec add_record(type_info(), atom(), #sp_rec{}) -> type_info().
add_record(#type_info{records = Records} = TypeInfo, Name, Record) ->
    TypeInfo#type_info{records = Records#{Name => Record}}.

-spec get_record(type_info(), atom()) -> {ok, #sp_rec{}} | error.
get_record(#type_info{records = Records}, Name) ->
    maps:find(Name, Records).

-spec add_function(type_info(), atom(), arity(), [spectra:sp_function_spec()]) ->
    type_info().
add_function(#type_info{functions = Functions} = TypeInfo, Name, Arity, FuncSpec) ->
    TypeInfo#type_info{functions = Functions#{{Name, Arity} => FuncSpec}}.

-spec get_function(type_info(), atom(), arity()) ->
    {ok, [spectra:sp_function_spec()]} | error.
get_function(#type_info{functions = Functions}, Name, Arity) ->
    maps:find({Name, Arity}, Functions).

-spec all_types(type_info()) -> #{type_key() => spectra:sp_type()}.
all_types(#type_info{types = Types}) ->
    Types.

-spec update_type(type_info(), atom(), arity(), spectra:sp_type()) -> type_info().
update_type(#type_info{types = Types} = TypeInfo, Name, Arity, Type) ->
    TypeInfo#type_info{types = Types#{{Name, Arity} => Type}}.

-spec all_records(type_info()) -> #{atom() => #sp_rec{}}.
all_records(#type_info{records = Records}) ->
    Records.

-spec update_record(type_info(), atom(), #sp_rec{}) -> type_info().
update_record(#type_info{records = Records} = TypeInfo, Name, Record) ->
    TypeInfo#type_info{records = Records#{Name => Record}}.
