-module(ed_type_generators).

-include_lib("proper/include/proper.hrl").

-include("../include/erldantic_internal.hrl").

-export([
    ed_type/0,
    ed_simple_type/0,
    ed_tuple/0,
    ed_map/0,
    ed_rec/0,
    ed_type_with_variables/0,
    ed_function/0,
    ed_union/0,
    ed_literal/0,
    ed_rec_ref/0,
    ed_remote_type/0,
    ed_maybe_improper_list/0,
    ed_nonempty_improper_list/0,
    ed_user_type_ref/0,
    ed_var/0,
    ed_range/0,
    ed_list/0,
    ed_nonempty_list/0,
    map_field/0
]).

%% Simple type generator
simple_type_atom() ->
    frequency([
        {10, integer},
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
        {10, map}
    ]).

ed_simple_type() ->
    ?LET(Type, simple_type_atom(), #ed_simple_type{type = Type}).

%% Tuple generator
ed_tuple() ->
    ?SIZED(Size, ed_tuple(Size)).

ed_tuple(Size) ->
    ?LET(
        Len,
        choose(0, Size),
        oneof([
            #ed_tuple{fields = any},
            ?LET(Fields, vector(Len, ed_type(Size)), #ed_tuple{fields = Fields})
        ])
    ).

%% Map field generator
map_field() ->
    ?SIZED(Size, map_field(Size)).

map_field(Size) ->
    oneof([
        ?LET({Name, Type}, {my_atom(), ed_type(Size)}, {map_field_assoc, Name, Type}),
        ?LET({Name, Type}, {my_atom(), ed_type(Size)}, {map_field_exact, Name, Type}),
        ?LET(
            {KeyType, ValueType},
            {ed_type(Size), ed_type(Size)},
            {map_field_type_assoc, KeyType, ValueType}
        ),
        ?LET(
            {KeyType, ValueType},
            {ed_type(Size), ed_type(Size)},
            {map_field_type_exact, KeyType, ValueType}
        )
    ]).

%% Map generator
ed_map() ->
    ?SIZED(Size, ed_map(Size)).

ed_map(Size) ->
    ?LET(
        Len,
        choose(0, Size),
        ?LET(Fields, vector(Len, map_field(Size)), #ed_map{fields = Fields})
    ).

%% Record generator
ed_rec() ->
    ?SIZED(Size, ed_rec(Size)).

ed_rec(Size) ->
    ?LET(
        Len,
        choose(1, max(1, Size)),
        ?LET(
            {Name, Fields, Arity},
            {my_atom(), non_empty(vector(Len, {my_atom(), ed_type(Size)})), pos_integer()},
            #ed_rec{
                name = Name,
                fields = Fields,
                arity = Arity
            }
        )
    ).

%% Type with variables generator
ed_type_with_variables() ->
    ?SIZED(Size, ed_type_with_variables(Size)).

ed_type_with_variables(Size) ->
    ?LET(
        Len,
        choose(1, max(1, Size)),
        ?LET(
            {Type, Vars},
            {ed_type(Size), vector(Len, my_atom())},
            #ed_type_with_variables{type = Type, vars = Vars}
        )
    ).

%% Function generator
ed_function() ->
    ?SIZED(Size, ed_function(Size)).

ed_function(Size) ->
    oneof([
        ?LET(Return, ed_type(Size), #ed_function{args = any, return = Return}),
        ?LET(
            {Args, Return},
            {ed_type(Size), ed_type(Size)},
            #ed_function{args = Args, return = Return}
        )
    ]).

%% Union generator
ed_union() ->
    ?SIZED(Size, ed_union(Size)).

ed_union(Size) ->
    ?LET(
        Len,
        choose(1, max(1, Size)),
        ?LET(Types, non_empty(vector(Len, ed_type(Size))), #ed_union{types = Types})
    ).

%% Literal generator
ed_literal() ->
    ?SIZED(Size, ed_literal(Size)).

ed_literal(Size) ->
    ?LET(Value, resize(Size, term()), #ed_literal{value = Value}).

%% Record reference generator
record_field(Size) ->
    ?LET({FieldName, Type}, {my_atom(), ed_type(Size)}, {FieldName, Type}).

ed_rec_ref() ->
    ?SIZED(Size, ed_rec_ref(Size)).

ed_rec_ref(Size) ->
    ?LET(
        Len,
        choose(0, Size),
        ?LET(
            {RecordName, FieldTypes},
            {my_atom(), vector(Len, record_field(Size))},
            #ed_rec_ref{record_name = RecordName, field_types = FieldTypes}
        )
    ).

%% Remote type generator
ed_remote_type() ->
    ?SIZED(Size, ed_remote_type(Size)).

ed_remote_type(Size) ->
    ?LET(
        Len,
        choose(0, Size),
        ?LET(
            {Module, Function, Args},
            {my_atom(), my_atom(), vector(Len, ed_type(Size))},
            #ed_remote_type{mfargs = {Module, Function, Args}}
        )
    ).

%% Maybe improper list generator
ed_maybe_improper_list() ->
    ?SIZED(Size, ed_maybe_improper_list(Size)).

ed_maybe_improper_list(Size) ->
    Childsize = Size div 2,
    ?LET(
        {Elements, Tail},
        {ed_type(Childsize), ed_type(Childsize)},
        #ed_maybe_improper_list{elements = Elements, tail = Tail}
    ).

%% Nonempty improper list generator
ed_nonempty_improper_list() ->
    ?SIZED(Size, ed_nonempty_improper_list(Size)).

ed_nonempty_improper_list(Size) ->
    ?LET(
        {Elements, Tail},
        {ed_type(Size), ed_type(Size)},
        #ed_nonempty_improper_list{elements = Elements, tail = Tail}
    ).

%% User type reference generator
ed_user_type_ref() ->
    ?SIZED(Size, ed_user_type_ref(Size)).

ed_user_type_ref(Size) ->
    ?LET(
        Len,
        choose(0, Size),
        ?LET(
            {TypeName, Variables},
            {my_atom(), vector(Len, ed_type(Size))},
            #ed_user_type_ref{type_name = TypeName, variables = Variables}
        )
    ).

%% Variable generator
ed_var() ->
    ?LET(Name, my_atom(), #ed_var{name = Name}).

%% Range generator
ed_range() ->
    ?LET(
        {Lower, Upper},
        {integer(), integer()},
        #ed_range{
            type = integer,
            lower_bound = min(Lower, Upper),
            upper_bound = max(Lower, Upper)
        }
    ).

%% List generator
ed_list() ->
    ?SIZED(Size, ed_list(Size)).

ed_list(Size) ->
    ?LET(Type, ed_type(Size), #ed_list{type = Type}).

%% Nonempty list generator
ed_nonempty_list() ->
    ?SIZED(Size, ed_nonempty_list(Size)).

ed_nonempty_list(Size) ->
    ?LET(Type, ed_type(Size), #ed_nonempty_list{type = Type}).

%% Main ed_type generator - generates any possible ed_type() value
ed_type() ->
    ?SIZED(Size, ed_type(Size)).

ed_type(0) ->
    oneof([ed_simple_type(), ed_literal(1), ed_var(), ed_range()]);
ed_type(Size) ->
    ChildSize = Size div 2,
    frequency([
        {10, ed_simple_type()},
        {10, ed_literal(Size)},
        {10, ed_range()},
        {10, ed_var()},
        {1, ed_tuple(ChildSize)},
        {10, ed_map(ChildSize)},
        {10, ed_rec(ChildSize)},
        {10, ed_type_with_variables(ChildSize)},
        {1, ed_function(ChildSize)},
        {10, ed_union(ChildSize)},
        {10, ed_rec_ref(ChildSize)},
        {10, ed_remote_type(ChildSize)},
        {10, ed_maybe_improper_list(ChildSize)},
        {10, ed_nonempty_improper_list(ChildSize)},
        {10, ed_user_type_ref(ChildSize)},
        {10, ed_list(ChildSize)},
        {10, ed_nonempty_list(ChildSize)}
    ]).

my_atom() ->
    oneof([atom1, atom2, atom3, atom4, atom5]).
