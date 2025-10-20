-module(tst).
-moduledoc("my module doc").
-doc("type doc").
-type my_type() :: integer() | atom().

-export_type([my_type/0]).

-export([get_type/0]).

-doc("my doc get type").
-spec get_type() -> my_type().
get_type() ->
    42.
