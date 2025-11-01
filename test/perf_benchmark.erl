-module(perf_benchmark).

-compile(nowarn_unused_type).

-export([run/0]).

-type position() :: 'Developer' | 'Manager' | 'Analyst' | 'Designer'.
-type department() :: 'Marketing' | 'Sales' | 'IT' | 'Finance' | 'HR'.
-type project_status() :: 'Ongoing' | 'Completed' | 'On Hold'.
-type task_status() :: 'Pending' | 'In Progress' | 'Done'.
-type subtask_status() :: 'Not Started' | 'In Progress' | 'Completed'.
-type relation() :: 'Spouse' | 'Child' | 'Parent'.

%% Maps with optional fields
-type contact() :: #{
    email => binary(),
    phone => binary()
}.

-type family_member() :: #{
    relation := relation(),
    name := binary(),
    age => pos_integer(),
    contact := contact()
}.

%% Records for structured data
-record(address, {
    street :: binary(),
    city :: binary(),
    zip_code :: binary(),
    country :: binary()
}).

-record(work_history_entry, {
    company :: binary(),
    role :: binary(),
    duration :: binary()
}).

-record(subtask, {
    subtask_id :: binary(),
    name :: binary(),
    status :: subtask_status()
}).

-record(task, {
    task_id :: binary(),
    description :: binary(),
    status :: task_status(),
    subtasks :: [#subtask{}]
}).

-record(project, {
    project_id :: binary(),
    name :: binary(),
    status :: project_status(),
    tasks :: [#task{}]
}).

-record(employee, {
    employee_id :: pos_integer(),
    name :: binary(),
    age :: pos_integer(),
    position :: position(),
    department :: department(),
    manager :: binary(),
    contact :: contact(),
    address :: #address{},
    family :: [family_member()],
    work_history :: [#work_history_entry{}],
    projects :: [#project{}]
}).

-type employees() :: [#employee{}].

run() ->
    io:format("~n=== Spectra Performance Benchmark ===~n"),
    io:format("Loading sample data from test/sample10k.json...~n"),

    {ok, JsonBinary} = file:read_file("test/sample10k.json"),
    Size = byte_size(JsonBinary),
    io:format("File size: ~p bytes (~.2f KB)~n~n", [Size, Size / 1024]),

    % First decode to get the Erlang data for encoding benchmark
    {ok, Employees} = spectra:decode(json, ?MODULE, employees, JsonBinary),

    % Run decode benchmark
    io:format("=== JSON Decode Benchmark ===~n"),
    run_decode_benchmark(JsonBinary),

    % Run encode benchmark
    io:format("~n=== JSON Encode Benchmark ===~n"),
    run_encode_benchmark(Employees),

    ok.

run_decode_benchmark(JsonBinary) ->
    DecodeFun = fun() ->
        {ok, _} = spectra:decode(json, ?MODULE, employees, JsonBinary)
    end,
    run_benchmark(DecodeFun, "decode").

run_encode_benchmark(Employees) ->
    EncodeFun = fun() ->
        {ok, _} = spectra:encode(json, ?MODULE, employees, Employees)
    end,
    run_benchmark(EncodeFun, "encode").

run_benchmark(Fun, Operation) ->
    % Warm up
    io:format("Warming up (5 iterations)...~n"),
    _ = [Fun() || _ <- lists:seq(1, 5)],

    % Run benchmark
    Iterations = 100,
    io:format("Running benchmark (~p iterations)...~n", [Iterations]),

    Times = [
        begin
            {Time, _Result} = timer:tc(Fun),
            Time
        end
     || _ <- lists:seq(1, Iterations)
    ],

    SortedTimes = lists:sort(Times),
    MinTime = lists:min(Times),
    MaxTime = lists:max(Times),
    MeanTime = lists:sum(Times) / length(Times),
    MedianTime = lists:nth(length(Times) div 2, SortedTimes),

    Variance = lists:sum([math:pow(T - MeanTime, 2) || T <- Times]) / length(Times),
    StdDev = math:sqrt(Variance),

    io:format("~n--- Results ---~n"),
    io:format("Mean time:   ~.2f ms~n", [MeanTime / 1000]),
    io:format("Median time: ~.2f ms~n", [MedianTime / 1000]),
    io:format("Min time:    ~.2f ms~n", [MinTime / 1000]),
    io:format("Max time:    ~.2f ms~n", [MaxTime / 1000]),
    io:format("Std dev:     ~.2f ms~n", [StdDev / 1000]),
    io:format("Throughput:  ~.2f ~ss/sec~n", [1000000 / MeanTime, Operation]),

    ok.
