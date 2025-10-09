# Claude Code Instructions

## Project Overview
impala - A data validation library for Erlang inspired by Pydantic

## Commands to run after each change
```bash
make format
make build-test
```

## Common Tasks
  - Add test: Create new test file in test/ directory
  - Run specific test: rebar3 eunit --module=test_module_name

## Development Guidelines
  - Don't define types in .hrl files. Types that the user of this library should use should be defined in impala.erl
  - When using a type that is defined in the same file, you don't have to prefix it with the module name

## Library Architecture
  - The library uses Erlang types during runtime to support type-safe serialization and deserialization
  - Types are extracted in `impala_abstract_code` into an internal format (`impala:im_type()`)
  - Different modules can use the extracted type data to:
    * Serialize data
    * Deserialize data
    * Generate schemas
  - Supports multiple formats including:
    * JSON
    * Open API Spec
    * DynamoDB's JSON flavor
