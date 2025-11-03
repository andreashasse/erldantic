-module(spectra_json_schema_remote_type_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/spectra.hrl").
-include("external_type.hrl").

-compile(nowarn_unused_type).

-type user_with_account() :: #{
    name := string(),
    account := other:account()
}.

-type user_with_result() :: #{
    name := string(),
    result := external_type:result_t(string())
}.

-type organization() :: #{
    primary_account := other:account(),
    backup_account => other:account(),
    status := external_type:result_t(atom())
}.

-record(transaction, {
    id :: integer(),
    account :: other:account(),
    result :: external_type:result_t(binary())
}).

-type transaction() :: #transaction{}.

-record(optional_account_holder, {
    id :: integer(),
    name :: string(),
    account :: other:account() | undefined
}).

-type optional_account_holder() :: #optional_account_holder{}.

-type account_list() :: [other:account()].

-type account_or_string() :: other:account() | string().

simple_remote_type_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, user_with_account, 0}),
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{
                <<"name">> := _,
                <<"account">> := #{
                    type := <<"object">>,
                    properties := #{
                        <<"id">> := #{type := <<"string">>},
                        <<"balance">> := #{type := <<"integer">>}
                    }
                }
            }
        },
        Schema
    ).

remote_type_with_variables_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, user_with_result, 0}),
    ?assertMatch(
        #{
            properties := #{
                <<"result">> := #{
                    type := <<"object">>,
                    properties := #{
                        <<"value">> := #{type := <<"string">>},
                        <<"errors">> := _
                    }
                }
            }
        },
        Schema
    ).

multiple_remote_types_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, organization, 0}),
    #{
        properties := #{
            <<"primary_account">> := PrimaryAccount,
            <<"backup_account">> := BackupAccount
        },
        required := Required
    } = Schema,
    ?assertEqual(maps:get(type, PrimaryAccount), maps:get(type, BackupAccount)),
    ?assertMatch(
        #{
            <<"primary_account">> := #{type := <<"object">>},
            <<"backup_account">> := #{type := <<"object">>},
            <<"status">> := #{type := <<"object">>}
        },
        maps:get(properties, Schema)
    ),
    ?assert(lists:member(<<"primary_account">>, Required)),
    ?assertNot(lists:member(<<"backup_account">>, Required)),
    ?assert(lists:member(<<"status">>, Required)).

remote_type_in_record_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {record, transaction}),
    ?assertMatch(
        #{
            type := <<"object">>,
            properties := #{
                <<"id">> := _,
                <<"account">> := #{type := <<"object">>},
                <<"result">> := #{
                    type := <<"object">>,
                    properties := #{<<"value">> := #{type := <<"string">>}}
                }
            }
        },
        Schema
    ).

optional_remote_type_field_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {record, optional_account_holder}),
    #{properties := Props, required := Required} = Schema,
    ?assertMatch(
        #{<<"id">> := _, <<"name">> := _, <<"account">> := #{type := <<"object">>}}, Props
    ),
    ?assert(lists:member(<<"id">>, Required)),
    ?assert(lists:member(<<"name">>, Required)),
    ?assertNot(lists:member(<<"account">>, Required)).

remote_type_in_list_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, account_list, 0}),
    ?assertMatch(
        #{
            type := <<"array">>,
            items := #{
                type := <<"object">>,
                properties := #{<<"id">> := _, <<"balance">> := _}
            }
        },
        Schema
    ).

remote_type_in_union_test() ->
    {ok, Schema} = spectra_json_schema:to_schema(?MODULE, {type, account_or_string, 0}),
    #{oneOf := OneOf} = Schema,
    ?assertEqual(2, length(OneOf)),
    Types = [maps:get(type, S) || S <- OneOf],
    ?assert(lists:member(<<"object">>, Types)),
    ?assert(lists:member(<<"string">>, Types)).
