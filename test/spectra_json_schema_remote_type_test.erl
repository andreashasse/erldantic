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
    Result = spectra_json_schema:to_schema(?MODULE, {type, user_with_account, 0}),
    ?assertMatch({ok, _}, Result),
    {ok, Schema} = Result,
    ?assertEqual(<<"object">>, maps:get(type, Schema)),
    Props = maps:get(properties, Schema),
    ?assertMatch(#{name := _, account := _}, Props),
    AccountSchema = maps:get(account, Props),
    ?assertEqual(<<"object">>, maps:get(type, AccountSchema)),
    AccountProps = maps:get(properties, AccountSchema),
    ?assertMatch(#{id := #{type := <<"string">>}}, AccountProps),
    ?assertMatch(#{balance := #{type := <<"integer">>}}, AccountProps).

remote_type_with_variables_test() ->
    Result = spectra_json_schema:to_schema(?MODULE, {type, user_with_result, 0}),
    ?assertMatch({ok, _}, Result),
    {ok, Schema} = Result,
    Props = maps:get(properties, Schema),
    ResultSchema = maps:get(result, Props),
    ?assertEqual(<<"object">>, maps:get(type, ResultSchema)),
    ResultProps = maps:get(properties, ResultSchema),
    ?assertMatch(#{value := _, errors := _}, ResultProps),
    ValueSchema = maps:get(value, ResultProps),
    ?assertEqual(<<"string">>, maps:get(type, ValueSchema)).

multiple_remote_types_test() ->
    Result = spectra_json_schema:to_schema(?MODULE, {type, organization, 0}),
    ?assertMatch({ok, _}, Result),
    {ok, Schema} = Result,
    Props = maps:get(properties, Schema),
    PrimaryAccount = maps:get(primary_account, Props),
    BackupAccount = maps:get(backup_account, Props),
    ?assertEqual(maps:get(type, PrimaryAccount), maps:get(type, BackupAccount)),
    ?assertEqual(<<"object">>, maps:get(type, PrimaryAccount)),
    StatusSchema = maps:get(status, Props),
    ?assertEqual(<<"object">>, maps:get(type, StatusSchema)),
    Required = maps:get(required, Schema),
    ?assert(lists:member(primary_account, Required)),
    ?assertNot(lists:member(backup_account, Required)),
    ?assert(lists:member(status, Required)).

remote_type_in_record_test() ->
    Result = spectra_json_schema:to_schema(?MODULE, {record, transaction}),
    ?assertMatch({ok, _}, Result),
    {ok, Schema} = Result,
    ?assertEqual(<<"object">>, maps:get(type, Schema)),
    Props = maps:get(properties, Schema),
    ?assertMatch(#{id := _, account := _, result := _}, Props),
    AccountSchema = maps:get(account, Props),
    ?assertEqual(<<"object">>, maps:get(type, AccountSchema)),
    ResultSchema = maps:get(result, Props),
    ?assertEqual(<<"object">>, maps:get(type, ResultSchema)),
    ResultProps = maps:get(properties, ResultSchema),
    ValueSchema = maps:get(value, ResultProps),
    ?assertEqual(<<"string">>, maps:get(type, ValueSchema)).

optional_remote_type_field_test() ->
    Result = spectra_json_schema:to_schema(?MODULE, {record, optional_account_holder}),
    ?assertMatch({ok, _}, Result),
    {ok, Schema} = Result,
    Props = maps:get(properties, Schema),
    ?assertMatch(#{id := _, name := _, account := _}, Props),
    Required = maps:get(required, Schema),
    ?assert(lists:member(id, Required)),
    ?assert(lists:member(name, Required)),
    ?assertNot(lists:member(account, Required)),
    AccountSchema = maps:get(account, Props),
    ?assertEqual(<<"object">>, maps:get(type, AccountSchema)).

remote_type_in_list_test() ->
    Result = spectra_json_schema:to_schema(?MODULE, {type, account_list, 0}),
    ?assertMatch({ok, _}, Result),
    {ok, Schema} = Result,
    ?assertEqual(<<"array">>, maps:get(type, Schema)),
    Items = maps:get(items, Schema),
    ?assertEqual(<<"object">>, maps:get(type, Items)),
    ItemProps = maps:get(properties, Items),
    ?assertMatch(#{id := _, balance := _}, ItemProps).

remote_type_in_union_test() ->
    Result = spectra_json_schema:to_schema(?MODULE, {type, account_or_string, 0}),
    ?assertMatch({ok, _}, Result),
    {ok, Schema} = Result,
    OneOf = maps:get(oneOf, Schema),
    ?assertEqual(2, length(OneOf)),
    Types = [maps:get(type, S) || S <- OneOf],
    ?assert(lists:member(<<"object">>, Types)),
    ?assert(lists:member(<<"string">>, Types)).
