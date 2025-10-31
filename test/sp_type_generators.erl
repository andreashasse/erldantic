-module(sp_type_generators).

-include_lib("proper/include/proper.hrl").

-include("../include/spectra_internal.hrl").

-export([
    sp_type/0,
    sp_simple_type/0,
    sp_tuple/0,
    sp_map/0,
    sp_rec/0,
    sp_type_with_variables/0,
    sp_function/0,
    sp_union/0,
    sp_literal/0,
    sp_rec_ref/0,
    sp_remote_type/0,
    sp_maybe_improper_list/0,
    sp_nonempty_improper_list/0,
    sp_user_type_ref/0,
    sp_var/0,
    sp_range/0,
    sp_list/0,
    sp_nonempty_list/0,
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

sp_simple_type() ->
    ?LET(Type, simple_type_atom(), #sp_simple_type{type = Type}).

%% Tuple generator
sp_tuple() ->
    ?SIZED(Size, sp_tuple(Size)).

sp_tuple(Size) ->
    ?LET(
        Len,
        choose(0, Size),
        oneof([
            #sp_tuple{fields = any},
            ?LET(Fields, vector(Len, sp_type(Size)), #sp_tuple{fields = Fields})
        ])
    ).

%% Map field generator
map_field() ->
    ?SIZED(Size, map_field(Size)).

map_field(Size) ->
    oneof([
        ?LET({Name, Type}, {my_atom(), sp_type(Size)}, #literal_map_field{
            kind = assoc,
            name = Name,
            binary_name = atom_to_binary(Name),
            val_type = Type
        }),
        ?LET({Name, Type}, {my_atom(), sp_type(Size)}, #literal_map_field{
            kind = exact,
            name = Name,
            binary_name = atom_to_binary(Name),
            val_type = Type
        }),
        ?LET(
            {KeyType, ValueType},
            {sp_type(Size), sp_type(Size)},
            #typed_map_field{
                kind = assoc_type,
                key_type = KeyType,
                val_type = ValueType
            }
        ),
        ?LET(
            {KeyType, ValueType},
            {sp_type(Size), sp_type(Size)},
            #typed_map_field{
                kind = exact_type,
                key_type = KeyType,
                val_type = ValueType
            }
        )
    ]).

%% Map generator
sp_map() ->
    ?SIZED(Size, sp_map(Size)).

sp_map(Size) ->
    ?LET(
        Len,
        choose(0, Size),
        ?LET(Fields, vector(Len, map_field(Size)), #sp_map{fields = Fields})
    ).

%% Record generator
sp_rec() ->
    ?SIZED(Size, sp_rec(Size)).

sp_rec(Size) ->
    ?LET(
        Len,
        choose(1, max(1, Size)),
        ?LET(
            {Name, Fields, Arity},
            {my_atom(), non_empty(vector(Len, {my_atom(), sp_type(Size)})), pos_integer()},
            #sp_rec{
                name = Name,
                fields = Fields,
                arity = Arity
            }
        )
    ).

%% Type with variables generator
sp_type_with_variables() ->
    ?SIZED(Size, sp_type_with_variables(Size)).

sp_type_with_variables(Size) ->
    ?LET(
        Len,
        choose(1, max(1, Size)),
        ?LET(
            {Type, Vars},
            {sp_type(Size), vector(Len, my_atom())},
            #sp_type_with_variables{type = Type, vars = Vars}
        )
    ).

%% Function generator
sp_function() ->
    ?SIZED(Size, sp_function(Size)).

sp_function(Size) ->
    oneof([
        ?LET(Return, sp_type(Size), #sp_function{args = any, return = Return}),
        ?LET(
            {Args, Return},
            {sp_type(Size), sp_type(Size)},
            #sp_function{args = Args, return = Return}
        )
    ]).

%% Union generator
sp_union() ->
    ?SIZED(Size, sp_union(Size)).

sp_union(Size) ->
    ?LET(
        Len,
        choose(1, max(1, Size)),
        ?LET(Types, non_empty(vector(Len, sp_type(Size))), #sp_union{types = Types})
    ).

%% Literal generator
sp_literal() ->
    ?SIZED(Size, sp_literal(Size)).

sp_literal(Size) ->
    ?LET(Value, resize(Size, term()), #sp_literal{value = Value}).

%% Record reference generator
record_field(Size) ->
    ?LET({FieldName, Type}, {my_atom(), sp_type(Size)}, {FieldName, Type}).

sp_rec_ref() ->
    ?SIZED(Size, sp_rec_ref(Size)).

sp_rec_ref(Size) ->
    ?LET(
        Len,
        choose(0, Size),
        ?LET(
            {RecordName, FieldTypes},
            {my_atom(), vector(Len, record_field(Size))},
            #sp_rec_ref{record_name = RecordName, field_types = FieldTypes}
        )
    ).

%% Remote type generator
sp_remote_type() ->
    ?SIZED(Size, sp_remote_type(Size)).

sp_remote_type(Size) ->
    ?LET(
        Len,
        choose(0, Size),
        ?LET(
            {Module, Function, Args},
            {my_atom(), my_atom(), vector(Len, sp_type(Size))},
            #sp_remote_type{mfargs = {Module, Function, Args}}
        )
    ).

%% Maybe improper list generator
sp_maybe_improper_list() ->
    ?SIZED(Size, sp_maybe_improper_list(Size)).

sp_maybe_improper_list(Size) ->
    Childsize = Size div 2,
    ?LET(
        {Elements, Tail},
        {sp_type(Childsize), sp_type(Childsize)},
        #sp_maybe_improper_list{elements = Elements, tail = Tail}
    ).

%% Nonempty improper list generator
sp_nonempty_improper_list() ->
    ?SIZED(Size, sp_nonempty_improper_list(Size)).

sp_nonempty_improper_list(Size) ->
    ?LET(
        {Elements, Tail},
        {sp_type(Size), sp_type(Size)},
        #sp_nonempty_improper_list{elements = Elements, tail = Tail}
    ).

%% User type reference generator
sp_user_type_ref() ->
    ?SIZED(Size, sp_user_type_ref(Size)).

sp_user_type_ref(Size) ->
    ?LET(
        Len,
        choose(0, Size),
        ?LET(
            {TypeName, Variables},
            {my_atom(), vector(Len, sp_type(Size))},
            #sp_user_type_ref{type_name = TypeName, variables = Variables}
        )
    ).

%% Variable generator
sp_var() ->
    ?LET(Name, my_atom(), #sp_var{name = Name}).

%% Range generator
sp_range() ->
    ?LET(
        {Lower, Upper},
        {integer(), integer()},
        #sp_range{
            type = integer,
            lower_bound = min(Lower, Upper),
            upper_bound = max(Lower, Upper)
        }
    ).

%% List generator
sp_list() ->
    ?SIZED(Size, sp_list(Size)).

sp_list(Size) ->
    ?LET(Type, sp_type(Size), #sp_list{type = Type}).

%% Nonempty list generator
sp_nonempty_list() ->
    ?SIZED(Size, sp_nonempty_list(Size)).

sp_nonempty_list(Size) ->
    ?LET(Type, sp_type(Size), #sp_nonempty_list{type = Type}).

%% Main sp_type generator - generates any possible sp_type() value
sp_type() ->
    ?SIZED(Size, sp_type(Size)).

sp_type(0) ->
    oneof([sp_simple_type(), sp_literal(1), sp_var(), sp_range()]);
sp_type(Size) ->
    ChildSize = Size div 2,
    frequency([
        {10, sp_simple_type()},
        {10, sp_literal(Size)},
        {10, sp_range()},
        {10, sp_var()},
        {1, sp_tuple(ChildSize)},
        {10, sp_map(ChildSize)},
        {10, sp_rec(ChildSize)},
        {10, sp_type_with_variables(ChildSize)},
        {1, sp_function(ChildSize)},
        {10, sp_union(ChildSize)},
        {10, sp_rec_ref(ChildSize)},
        {10, sp_remote_type(ChildSize)},
        {10, sp_maybe_improper_list(ChildSize)},
        {10, sp_nonempty_improper_list(ChildSize)},
        {10, sp_user_type_ref(ChildSize)},
        {10, sp_list(ChildSize)},
        {10, sp_nonempty_list(ChildSize)}
    ]).

my_atom() ->
    oneof([atom1, atom2, atom3, atom4, atom5]).
