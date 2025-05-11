# Claude Code Instructions

## Project Overview
erldantic - A data validation library for Erlang inspired by Pydantic

## Commands to run after each change
```bash
rebar3 do compile, format, eunit, cover
```

## Common Tasks
  - Add test: Create new test file in test/ directory
  - Run specific test: rebar3 eunit --module=test_module_name