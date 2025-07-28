# Claude Code Instructions

## Project Overview
erldantic - A data validation library for Erlang inspired by Pydantic

## Commands to run after each change
```bash
make format
make build-test
```

## Common Tasks
  - Add test: Create new test file in test/ directory
  - Run specific test: rebar3 eunit --module=test_module_name

## Development Guidelines
  - Don't define types in .hrl files. Types that the user of this library should use should be defined in erldantic.erl
  - When using a type that is defined in the same file, you don't have to prefix it with the module name