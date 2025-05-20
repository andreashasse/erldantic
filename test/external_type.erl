-module(external_type).

-include("external_type.hrl").

-type result_t(ResultType) :: #result{value :: ResultType}.

-export_type([result_t/1]).
