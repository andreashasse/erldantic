-module(remote_type_field_args_test).

-include_lib("eunit/include/eunit.hrl").

-include("external_type.hrl").

%% Test remote types with various field argument patterns

%% Simple remote type in map field
-type user_with_account() ::
    #{name := string(),
      account => other:account(),
      active := boolean()}.
%% Multiple remote types in different fields
-type organization() ::
    #{users := [other:account()],
      results := external_type:result_t(string()),
      metadata => external_type:result_t(integer())}.
%% Nested remote types with field arguments
-type complex_data() ::
    #{primary := #{account := other:account(), result => external_type:result_t(binary())},
      secondary => [#{id := string(), account => other:account()}]}.
%% Remote type with optional and required fields
-type mixed_requirements() ::
    #{required_account := other:account(),
      optional_result => external_type:result_t(atom()),
      required_list := [other:account()],
      optional_map => #{account => other:account()}}.

%% Tests for user_with_account type

user_with_account_valid_test() ->
    ValidData =
        #{name => "John Doe",
          account => #{id => "user123", balance => 1500},
          active => true},
    Expected =
        #{name => <<"John Doe">>,
          account => #{id => <<"user123">>, balance => 1500},
          active => true},
    ?assertEqual({ok, Expected}, to_json_user_with_account(ValidData)).

user_with_account_optional_field_test() ->
    ValidData = #{name => "Jane Doe", active => false},
    Expected = #{name => <<"Jane Doe">>, active => false},
    ?assertEqual({ok, Expected}, to_json_user_with_account(ValidData)).

user_with_account_invalid_remote_test() ->
    InvalidData =
        #{name => "John Doe",
          account => #{id => 123, balance => "invalid"}, % Invalid types
          active => true},
    Result = to_json_user_with_account(InvalidData),
    ?assertMatch({error, [_ | _]}, Result).

user_with_account_from_json_test() ->
    ValidJson =
        #{<<"name">> => <<"John Doe">>,
          <<"account">> => #{<<"id">> => <<"user123">>, <<"balance">> => 1500},
          <<"active">> => true},
    Expected =
        #{name => "John Doe",
          account => #{id => "user123", balance => 1500},
          active => true},
    ?assertEqual({ok, Expected}, from_json_user_with_account(ValidJson)).

%% Tests for organization type

organization_valid_test() ->
    ValidData =
        #{users => [#{id => "user1", balance => 100}, #{id => "user2", balance => 200}],
          results => #result{value = "success", errors = []},
          metadata => #result{value = 42, errors = []}},
    Result = to_json_organization(ValidData),
    ?assertMatch({ok, _}, Result),
    {ok, JsonData} = Result,
    ?assertEqual([#{id => <<"user1">>, balance => 100}, #{id => <<"user2">>, balance => 200}],
                 maps:get(users, JsonData)).

organization_required_only_test() ->
    ValidData =
        #{users => [#{id => "user1", balance => 100}],
          results => #result{value = "success", errors = []}},
    Result = to_json_organization(ValidData),
    ?assertMatch({ok, _}, Result).

organization_invalid_users_test() ->
    InvalidData =
        #{users => [#{id => 123, balance => "invalid"}], % Invalid user data
          results => #result{value = "success", errors = []}},
    Result = to_json_organization(InvalidData),
    ?assertMatch({error, [_ | _]}, Result).

organization_from_json_test() ->
    ValidJson =
        #{<<"users">> => [#{<<"id">> => <<"user1">>, <<"balance">> => 100}],
          <<"results">> => #{<<"value">> => <<"success">>, <<"errors">> => []}},
    Expected =
        #{users => [#{id => "user1", balance => 100}],
          results => #result{value = "success", errors = []}},
    ?assertEqual({ok, Expected}, from_json_organization(ValidJson)).

%% Tests for complex_data type

complex_data_valid_test() ->
    ValidData =
        #{primary =>
              #{account => #{id => "primary", balance => 1000},
                result => #result{value = <<"binary_data">>, errors = []}},
          secondary =>
              [#{id => "sec1", account => #{id => "acc1", balance => 500}},
               #{id => "sec2", account => #{id => "acc2", balance => 750}}]},
    Result = to_json_complex_data(ValidData),
    ?assertMatch({ok, _}, Result).

complex_data_minimal_test() ->
    ValidData = #{primary => #{account => #{id => "primary", balance => 1000}}},
    Result = to_json_complex_data(ValidData),
    ?assertMatch({ok, _}, Result).

complex_data_invalid_nested_test() ->
    InvalidData =
        #{primary =>
              #{account => #{id => 123, balance => "invalid"}, % Invalid types in account
                result => #result{value = <<"binary_data">>, errors = []}}},
    Result = to_json_complex_data(InvalidData),
    ?assertMatch({error, [_ | _]}, Result).

complex_data_from_json_test() ->
    ValidJson =
        #{<<"primary">> =>
              #{<<"account">> => #{<<"id">> => <<"primary">>, <<"balance">> => 1000}}},
    Expected = #{primary => #{account => #{id => "primary", balance => 1000}}},
    ?assertEqual({ok, Expected}, from_json_complex_data(ValidJson)).

%% Tests for mixed_requirements type

mixed_requirements_all_fields_test() ->
    ValidData =
        #{required_account => #{id => "req1", balance => 1000},
          optional_result => #result{value = success, errors = []},
          required_list => [#{id => "list1", balance => 100}, #{id => "list2", balance => 200}],
          optional_map => #{account => #{id => "opt1", balance => 300}}},
    Result = to_json_mixed_requirements(ValidData),
    ?assertMatch({ok, _}, Result).

mixed_requirements_required_only_test() ->
    ValidData =
        #{required_account => #{id => "req1", balance => 1000},
          required_list => [#{id => "list1", balance => 100}]},
    Result = to_json_mixed_requirements(ValidData),
    ?assertMatch({ok, _}, Result).

mixed_requirements_missing_required_test() ->
    InvalidData =
        #{optional_result => #result{value = success, errors = []},
          required_list => [#{id => "list1", balance => 100}]},
    % Missing required_account
    Result = to_json_mixed_requirements(InvalidData),
    ?assertMatch({error, [_ | _]}, Result).

mixed_requirements_from_json_test() ->
    ValidJson =
        #{<<"required_account">> => #{<<"id">> => <<"req1">>, <<"balance">> => 1000},
          <<"required_list">> => [#{<<"id">> => <<"list1">>, <<"balance">> => 100}]},
    Expected =
        #{required_account => #{id => "req1", balance => 1000},
          required_list => [#{id => "list1", balance => 100}]},
    ?assertEqual({ok, Expected}, from_json_mixed_requirements(ValidJson)).

%% JSON conversion functions

-spec to_json_user_with_account(term()) ->
                                   {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json_user_with_account(Data) ->
    erldantic_json:type_to_json(?MODULE, user_with_account, 0, Data).

-spec from_json_user_with_account(json:encode_value()) ->
                                     {ok, user_with_account()} | {error, [erldantic:error()]}.
from_json_user_with_account(Json) ->
    erldantic_json:type_from_json(?MODULE, user_with_account, 0, Json).

-spec to_json_organization(term()) ->
                              {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json_organization(Data) ->
    erldantic_json:type_to_json(?MODULE, organization, 0, Data).

-spec from_json_organization(json:encode_value()) ->
                                {ok, organization()} | {error, [erldantic:error()]}.
from_json_organization(Json) ->
    erldantic_json:type_from_json(?MODULE, organization, 0, Json).

-spec to_json_complex_data(term()) ->
                              {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json_complex_data(Data) ->
    erldantic_json:type_to_json(?MODULE, complex_data, 0, Data).

-spec from_json_complex_data(json:encode_value()) ->
                                {ok, complex_data()} | {error, [erldantic:error()]}.
from_json_complex_data(Json) ->
    erldantic_json:type_from_json(?MODULE, complex_data, 0, Json).

-spec to_json_mixed_requirements(term()) ->
                                    {ok, json:encode_value()} | {error, [erldantic:error()]}.
to_json_mixed_requirements(Data) ->
    erldantic_json:type_to_json(?MODULE, mixed_requirements, 0, Data).

-spec from_json_mixed_requirements(json:encode_value()) ->
                                      {ok, mixed_requirements()} | {error, [erldantic:error()]}.
from_json_mixed_requirements(Json) ->
    erldantic_json:type_from_json(?MODULE, mixed_requirements, 0, Json).
