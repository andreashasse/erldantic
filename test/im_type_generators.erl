-module(im_type_generators).

-include_lib("proper/include/proper.hrl").

-include("../include/impala_internal.hrl").

-export([im_type/0, im_simple_type/0, im_tuple/0, im_map/0, im_rec/0,
         im_type_with_variables/0, im_function/0, im_union/0, im_literal/0, im_rec_ref/0,
         im_remote_type/0, im_maybe_improper_list/0, im_nonempty_improper_list/0,
         im_user_type_ref/0, im_var/0, im_range/0, im_list/0, im_nonempty_list/0, map_field/0]).

%% Simple type generator
simple_type_atom() ->
    frequency([{10, integer},
               {10, non_neg_integer},
               {10, neg_integer},
               {10, pos_integer},
               {10, float},
               {10, number},
               {10, boolean},
               {10, atom},
               {10, string},
               {10, nonempty_string},
               {10, binary},
               {10, nonempty_binary},
               {1, bitstring},
               {1, nonempty_bitstring},
               {1, term},
               {1, reference},
               {1, pid},
               {1, port},
               {10, iolist},
               {10, iodata},
               {1, none},
               {10, map}]).

im_simple_type() ->
    ?LET(Type, simple_type_atom(), #im_simple_type{type = Type}).

%% Tuple generator
im_tuple() ->
    ?SIZED(Size, im_tuple(Size)).

im_tuple(Size) ->
    ?LET(Len,
         choose(0, Size),
         oneof([#im_tuple{fields = any},
                ?LET(Fields, vector(Len, im_type(Size)), #im_tuple{fields = Fields})])).

%% Map field generator
map_field() ->
    ?SIZED(Size, map_field(Size)).

map_field(Size) ->
    oneof([?LET({Name, Type}, {my_atom(), im_type(Size)}, {map_field_assoc, Name, Type}),
           ?LET({Name, Type}, {my_atom(), im_type(Size)}, {map_field_exact, Name, Type}),
           ?LET({KeyType, ValueType},
                {im_type(Size), im_type(Size)},
                {map_field_type_assoc, KeyType, ValueType}),
           ?LET({KeyType, ValueType},
                {im_type(Size), im_type(Size)},
                {map_field_type_exact, KeyType, ValueType})]).

%% Map generator
im_map() ->
    ?SIZED(Size, im_map(Size)).

im_map(Size) ->
    ?LET(Len,
         choose(0, Size),
         ?LET(Fields, vector(Len, map_field(Size)), #im_map{fields = Fields})).

%% Record generator
im_rec() ->
    ?SIZED(Size, im_rec(Size)).

im_rec(Size) ->
    ?LET(Len,
         choose(1, max(1, Size)),
         ?LET({Name, Fields, Arity},
              {my_atom(), non_empty(vector(Len, {my_atom(), im_type(Size)})), pos_integer()},
              #im_rec{name = Name,
                      fields = Fields,
                      arity = Arity})).

%% Type with variables generator
im_type_with_variables() ->
    ?SIZED(Size, im_type_with_variables(Size)).

im_type_with_variables(Size) ->
    ?LET(Len,
         choose(1, max(1, Size)),
         ?LET({Type, Vars},
              {im_type(Size), vector(Len, my_atom())},
              #im_type_with_variables{type = Type, vars = Vars})).

%% Function generator
im_function() ->
    ?SIZED(Size, im_function(Size)).

im_function(Size) ->
    oneof([?LET(Return, im_type(Size), #im_function{args = any, return = Return}),
           ?LET({Args, Return},
                {im_type(Size), im_type(Size)},
                #im_function{args = Args, return = Return})]).

%% Union generator
im_union() ->
    ?SIZED(Size, im_union(Size)).

im_union(Size) ->
    ?LET(Len,
         choose(1, max(1, Size)),
         ?LET(Types, non_empty(vector(Len, im_type(Size))), #im_union{types = Types})).

%% Literal generator
im_literal() ->
    ?SIZED(Size, im_literal(Size)).

im_literal(Size) ->
    ?LET(Value, resize(Size, term()), #im_literal{value = Value}).

%% Record reference generator
record_field(Size) ->
    ?LET({FieldName, Type}, {my_atom(), im_type(Size)}, {FieldName, Type}).

im_rec_ref() ->
    ?SIZED(Size, im_rec_ref(Size)).

im_rec_ref(Size) ->
    ?LET(Len,
         choose(0, Size),
         ?LET({RecordName, FieldTypes},
              {my_atom(), vector(Len, record_field(Size))},
              #im_rec_ref{record_name = RecordName, field_types = FieldTypes})).

%% Remote type generator
im_remote_type() ->
    ?SIZED(Size, im_remote_type(Size)).

im_remote_type(Size) ->
    ?LET(Len,
         choose(0, Size),
         ?LET({Module, Function, Args},
              {my_atom(), my_atom(), vector(Len, im_type(Size))},
              #im_remote_type{mfargs = {Module, Function, Args}})).

%% Maybe improper list generator
im_maybe_improper_list() ->
    ?SIZED(Size, im_maybe_improper_list(Size)).

im_maybe_improper_list(Size) ->
    Childsize = Size div 2,
    ?LET({Elements, Tail},
         {im_type(Childsize), im_type(Childsize)},
         #im_maybe_improper_list{elements = Elements, tail = Tail}).

%% Nonempty improper list generator
im_nonempty_improper_list() ->
    ?SIZED(Size, im_nonempty_improper_list(Size)).

im_nonempty_improper_list(Size) ->
    ?LET({Elements, Tail},
         {im_type(Size), im_type(Size)},
         #im_nonempty_improper_list{elements = Elements, tail = Tail}).

%% User type reference generator
im_user_type_ref() ->
    ?SIZED(Size, im_user_type_ref(Size)).

im_user_type_ref(Size) ->
    ?LET(Len,
         choose(0, Size),
         ?LET({TypeName, Variables},
              {my_atom(), vector(Len, im_type(Size))},
              #im_user_type_ref{type_name = TypeName, variables = Variables})).

%% Variable generator
im_var() ->
    ?LET(Name, my_atom(), #im_var{name = Name}).

%% Range generator
im_range() ->
    ?LET({Lower, Upper},
         {integer(), integer()},
         #im_range{type = integer,
                   lower_bound = min(Lower, Upper),
                   upper_bound = max(Lower, Upper)}).

%% List generator
im_list() ->
    ?SIZED(Size, im_list(Size)).

im_list(Size) ->
    ?LET(Type, im_type(Size), #im_list{type = Type}).

%% Nonempty list generator
im_nonempty_list() ->
    ?SIZED(Size, im_nonempty_list(Size)).

im_nonempty_list(Size) ->
    ?LET(Type, im_type(Size), #im_nonempty_list{type = Type}).

%% Main im_type generator - generates any possible im_type() value
im_type() ->
    ?SIZED(Size, im_type(Size)).

im_type(0) ->
    oneof([im_simple_type(), im_literal(1), im_var(), im_range()]);
im_type(Size) ->
    ChildSize = Size div 2,
    frequency([{10, im_simple_type()},
               {10, im_literal(Size)},
               {10, im_range()},
               {10, im_var()},
               {1, im_tuple(ChildSize)},
               {10, im_map(ChildSize)},
               {10, im_rec(ChildSize)},
               {10, im_type_with_variables(ChildSize)},
               {1, im_function(ChildSize)},
               {10, im_union(ChildSize)},
               {10, im_rec_ref(ChildSize)},
               {10, im_remote_type(ChildSize)},
               {10, im_maybe_improper_list(ChildSize)},
               {10, im_nonempty_improper_list(ChildSize)},
               {10, im_user_type_ref(ChildSize)},
               {10, im_list(ChildSize)},
               {10, im_nonempty_list(ChildSize)}]).

my_atom() ->
    oneof([atom1, atom2, atom3, atom4, atom5]).
