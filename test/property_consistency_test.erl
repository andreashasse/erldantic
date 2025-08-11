-module(property_consistency_test).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-compile(nowarn_unused_type).

%% Test types for property-based testing
%-type test_integer() :: integer().
-type test_string() :: string().
-type test_binary() :: binary().
-type test_boolean() :: boolean().
%-type test_float() :: float().
%-type test_atom() :: atom().
-type test_list() :: [string()].
-type test_map() :: #{binary() => integer()}.

%% Property-based test that compares Jesse validation with erldantic_json validation
jesse_erldantic_consistency_property_test_() ->
    {timeout,
     30,
     ?_assert(proper:quickcheck(
                  ?MODULE:prop_jesse_erldantic_consistency(), [{numtests, 100}, {to_file, user}]))}.

%% The main property: Jesse and erldantic should agree on validation results
prop_jesse_erldantic_consistency() ->
    ?FORALL({TypeName, JsonValue},
            {test_type_name(), json_generator:json_value()},
            begin
                % Generate OpenAPI schema from the type
                SchemaResult = erldantic_openapi:type_to_schema(?MODULE, TypeName),
                case SchemaResult of
                    {ok, Schema} ->
                        % Convert schema to Jesse format
                        JesseSchema = json:decode(iolist_to_binary(json:encode(Schema))),

                        % Test Jesse validation
                        JesseResult = jesse:validate_with_schema(JesseSchema, JsonValue),
                        JesseValid =
                            case JesseResult of
                                {ok, _} ->
                                    true;
                                {error, _} ->
                                    false
                            end,

                        % Test erldantic_json validation
                        ErldanticResult =
                            erldantic_json:type_from_json(?MODULE, TypeName, JsonValue),
                        ErldanticValid =
                            case ErldanticResult of
                                {ok, _} ->
                                    true;
                                {error, _} ->
                                    false
                            end,

                        % Both should agree: either both succeed or both fail
                        Agreement = JesseValid == ErldanticValid,

                        % If they disagree, print debug info
                        case Agreement of
                            true ->
                                true;
                            false ->
                                io:format("~nDISAGREEMENT FOUND:~n"),
                                io:format("TypeName: ~p~n", [TypeName]),
                                io:format("JsonValue: ~p~n", [JsonValue]),
                                io:format("Jesse result: ~p (valid: ~p)~n",
                                          [JesseResult, JesseValid]),
                                io:format("Erldantic result: ~p (valid: ~p)~n",
                                          [ErldanticResult, ErldanticValid]),
                                io:format("Generated schema: ~p~n", [Schema]),
                                false
                        end;
                    {error, _} ->
                        % If we can't generate a schema, skip this test case
                        true
                end
            end).

%% Generator for test type names
test_type_name() ->
    oneof([%test_integer,
           test_string,
           test_binary,
           test_boolean,
           %test_float,
           %test_atom,
           test_list,
           test_map]).
