-module(person).

-compile([{parse_transform, record_type_introspect}]).

-include_lib("person.hrl").

-export([new/4, get_name/1, get_age/1]).

-spec new(string(), string(), integer(), address_t()) -> #person{}.
new(First, Last, Age, Home) ->
    #person{name = #{first => First, last => Last},
            age = Age,
            home = Home}.

-spec get_name(#person{}) -> nonempty_string().
get_name(#person{name = #{first := First, last := Last}}) ->
    First ++ " " ++ Last.

-spec get_age(#person{}) -> integer().
get_age(#person{age = Age}) ->
    Age.
