-module(pr45_comment_issues_test).

-include_lib("eunit/include/eunit.hrl").

%% Test module specifically for the issues identified in PR #45 comments
%% https://github.com/andreashasse/erldantic/pull/45

-compile(nowarn_unused_type).

%% Test types that highlight the specific PR comment issues
-type my_binary() :: binary().
-type my_nonempty_binary() :: nonempty_binary().
-type my_atom_literal() :: ok.
-type my_term() :: term().
-type my_pid_literal() :: pid().

%% Issue 1: "format => <<\"binary\">>" - This can't be right?
%% Location: src/erldantic_json_schema.erl:76
binary_format_issue_test() ->
    {ok, BinarySchema} = erldantic_json_schema:to_schema(?MODULE, {type, my_binary, 0}),
    ?assertEqual(#{type => <<"string">>}, BinarySchema),

    %% FIXED: Now correctly generates simple string type for binary()
    %% This matches how erldantic_json handles binary - as regular JSON strings
    %% No more problematic "binary" format that isn't standard JSON Schema
    ok.

%% Issue 2: "Again with the weird? format" (binary format + minLength)
%% Location: src/erldantic_json_schema.erl:80
binary_format_with_minlength_issue_test() ->
    {ok, NonEmptyBinarySchema} =
        erldantic_json_schema:to_schema(?MODULE, {type, my_nonempty_binary, 0}),
    Expected = #{type => <<"string">>, minLength => 1},
    ?assertEqual(Expected, NonEmptyBinarySchema),

    %% FIXED: Now correctly generates string type with minLength for nonempty_binary()
    %% This makes semantic sense - minLength applies to the JSON string length
    %% No more confusing "binary" format
    ok.

%% Issue 3: "Is this valid json schema?" (empty object for term type)
%% Location: src/erldantic_json_schema.erl:103
empty_schema_for_term_test() ->
    {ok, TermSchema} = erldantic_json_schema:to_schema(?MODULE, {type, my_term, 0}),
    ?assertEqual(#{}, TermSchema),

    %% Actually, this is CORRECT! Empty object {} in JSON Schema means "any valid JSON value"
    %% This is the proper way to represent Erlang's term() type
    %% Comment should be marked as resolved - this is valid JSON Schema
    ok.

%% Issue 4: "most literal values doesn't translate well to json"
%% Location: src/erldantic_json_schema.erl:105
literal_values_translation_issue_test() ->
    {ok, AtomLiteralSchema} =
        erldantic_json_schema:to_schema(?MODULE, {type, my_atom_literal, 0}),
    ?assertEqual(#{enum => [<<"ok">>]}, AtomLiteralSchema),

    %% FIXED: Now correctly converts atom literals to binary strings in enum
    %% - The enum contains the binary string <<"ok">> instead of raw atom 'ok'
    %% - This matches how erldantic_json handles atoms - as UTF-8 encoded binaries
    %% - JSON Schema validators can now properly validate JSON string "ok" against this schema
    ok.

%% Demonstration of correct schemas that should be generated
correct_schemas_test() ->
    %% What binary schema should look like (simple string for JSON compatibility):
    CorrectBinarySchema = #{type => <<"string">>},

    %% What atom literal schema should look like:
    CorrectAtomLiteralSchema = #{enum => [<<"ok">>]}, %% String, not atom

    %% Current implementation now matches these correct schemas
    {ok, CurrentBinary} = erldantic_json_schema:to_schema(?MODULE, {type, my_binary, 0}),
    {ok, CurrentAtom} = erldantic_json_schema:to_schema(?MODULE, {type, my_atom_literal, 0}),

    ?assertEqual(CorrectBinarySchema, CurrentBinary),
    ?assertEqual(CorrectAtomLiteralSchema, CurrentAtom),

    ok.

%% Test with actual JSON Schema validator (Jesse) to show validation problems
json_schema_validator_issues_test() ->
    %% Test atom literal with Jesse
    {ok, AtomSchema} = erldantic_json_schema:to_schema(?MODULE, {type, my_atom_literal, 0}),

    %% Convert schema to Jesse format
    JesseSchema = json:decode(iolist_to_binary(json:encode(AtomSchema))),

    %% Now the schema correctly has enum: ["ok"] instead of enum: [ok]
    %% Validate JSON string "ok" against the corrected schema
    Result = jesse:validate_with_schema(JesseSchema, <<"ok">>),

    %% This should now work correctly since atoms are converted to binary strings
    ?assertEqual({ok, <<"ok">>}, Result),

    ok.

%% Summary of issues found in PR #45:
summary_test() ->
    %% Issues found and fixed:
    %% 1. ✓ FIXED: Binary type now generates simple string type (matches erldantic_json behavior)
    %% 2. ✓ FIXED: nonempty_binary generates string with minLength (semantically correct)
    %% 3. ✓ CORRECT: Empty schema {} for term() is valid JSON Schema - comment resolved
    %% 4. ✓ FIXED: Atom literals now convert to binary strings in enum arrays
    %% 5. N/A: Complex literals (tuples, etc.) are not supported by the type system
    ok.
