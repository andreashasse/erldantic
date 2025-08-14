-module(ed_type_generators).

-include_lib("proper/include/proper.hrl").

-include("../include/erldantic_internal.hrl").

-export([ed_type/0, ed_simple_type/0, ed_tuple/0, ed_map/0, ed_rec/0,
         ed_type_with_variables/0, ed_function/0, ed_union/0, ed_literal/0, ed_rec_ref/0,
         ed_remote_type/0, ed_maybe_improper_list/0, ed_nonempty_improper_list/0,
         ed_user_type_ref/0, ed_var/0, ed_range/0, ed_list/0, ed_nonempty_list/0, map_field/0]).

%% Simple type generator
simple_type_atom() ->
    oneof([string,
           nonempty_string,
           integer,
           non_neg_integer,
           neg_integer,
           pos_integer,
           float,
           number,
           boolean,
           binary,
           nonempty_binary,
           bitstring,
           nonempty_bitstring,
           atom,
           term,
           reference,
           pid,
           port,
           iolist,
           iodata,
           none,
           map]).

ed_simple_type() ->
    ?LET(Type, simple_type_atom(), #ed_simple_type{type = Type}).

%% Tuple generator
ed_tuple() ->
    oneof([#ed_tuple{fields = any},
           ?LET(Fields, list(ed_type()), #ed_tuple{fields = Fields})]).

%% Map field generator
map_field() ->
    oneof([?LET({Name, Type}, {atom(), ed_type()}, {map_field_assoc, Name, Type}),
           ?LET({Name, Type}, {atom(), ed_type()}, {map_field_exact, Name, Type}),
           ?LET({KeyType, ValueType},
                {ed_type(), ed_type()},
                {map_field_type_assoc, KeyType, ValueType}),
           ?LET({KeyType, ValueType},
                {ed_type(), ed_type()},
                {map_field_type_exact, KeyType, ValueType})]).

%% Map generator
ed_map() ->
    ?LET(Fields, list(map_field()), #ed_map{fields = Fields}).

%% Record generator
ed_rec() ->
    ?LET({Name, Fields, Arity},
         {atom(), list({atom(), ed_type()}), pos_integer()},
         #ed_rec{name = Name,
                 fields = Fields,
                 arity = Arity}).

%% Type with variables generator
ed_type_with_variables() ->
    ?LET({Type, Vars},
         {ed_type(), list(atom())},
         #ed_type_with_variables{type = Type, vars = Vars}).

%% Function generator
ed_function() ->
    oneof([?LET(Return, ed_type(), #ed_function{args = any, return = Return}),
           ?LET({Args, Return},
                {list(ed_type()), ed_type()},
                #ed_function{args = Args, return = Return})]).

%% Union generator
ed_union() ->
    ?LET(Types, non_empty(list(ed_type())), #ed_union{types = Types}).

%% Literal generator
ed_literal() ->
    ?LET(Value, term(), #ed_literal{value = Value}).

%% Record reference generator
record_field() ->
    ?LET({FieldName, Type}, {atom(), ed_type()}, {FieldName, Type}).

ed_rec_ref() ->
    ?LET({RecordName, FieldTypes},
         {atom(), list(record_field())},
         #ed_rec_ref{record_name = RecordName, field_types = FieldTypes}).

%% Remote type generator
ed_remote_type() ->
    ?LET({Module, Function, Args},
         {atom(), atom(), list(ed_type())},
         #ed_remote_type{mfargs = {Module, Function, Args}}).

%% Maybe improper list generator
ed_maybe_improper_list() ->
    ?LET({Elements, Tail},
         {ed_type(), ed_type()},
         #ed_maybe_improper_list{elements = Elements, tail = Tail}).

%% Nonempty improper list generator
ed_nonempty_improper_list() ->
    ?LET({Elements, Tail},
         {ed_type(), ed_type()},
         #ed_nonempty_improper_list{elements = Elements, tail = Tail}).

%% User type reference generator
ed_user_type_ref() ->
    ?LET({TypeName, Variables},
         {atom(), list(ed_type())},
         #ed_user_type_ref{type_name = TypeName, variables = Variables}).

%% Variable generator
ed_var() ->
    ?LET(Name, atom(), #ed_var{name = Name}).

%% Range generator
ed_range() ->
    ?LET({Lower, Upper},
         {integer(), integer()},
         #ed_range{type = integer,
                   lower_bound = min(Lower, Upper),
                   upper_bound = max(Lower, Upper)}).

%% List generator
ed_list() ->
    ?LET(Type, ed_type(), #ed_list{type = Type}).

%% Nonempty list generator
ed_nonempty_list() ->
    ?LET(Type, ed_type(), #ed_nonempty_list{type = Type}).

%% Main ed_type generator - generates any possible ed_type() value
ed_type() ->
    ?SIZED(Size, ed_type(Size)).

ed_type(0) ->
    oneof([ed_simple_type(), ed_literal(), ed_var()]);
ed_type(Size) ->
    oneof([ed_simple_type(),
           %resize(Size div 2, ed_tuple()),
           %resize(Size div 2, ed_map()),
           %resize(Size div 2, ed_rec()),
           %resize(Size div 2, ed_type_with_variables()),
           %resize(Size div 2, ed_function()),
           %resize(Size div 2, ed_union()),
           %ed_literal(),
           %resize(Size div 2, ed_rec_ref()),
           %resize(Size div 2, ed_remote_type()),
           %resize(Size div 2, ed_maybe_improper_list()),
           %resize(Size div 2, ed_nonempty_improper_list()),
           %resize(Size div 2, ed_user_type_ref()),
           %ed_var(),
           ed_range(),
           resize(Size div 2, ed_list()),
           resize(Size div 2, ed_nonempty_list())
    ]).
