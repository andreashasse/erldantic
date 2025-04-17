# erldantic


This is Pydantic, but for erlang. Hopefully for Elixir and Gleam in the future!

## TODO PoC
- [ ] All types expect `pid()`, `port()`, `Fun` and `reference()`.
    - [X] List
    - [ ] Float
    - [ ] Atom
    - [ ] Boolean
    - [ ] Number? or will it always show up as integer()|float()?
    - [ ] Integer range
    - [ ] non_empty ... binary, bitstring,
    - [ ] any()/term() and dynamic()? none()?
    - [x] non_neg_integer
    - [ ] pos_integer
    - [ ] neg_integer
- [ ] Good error messages
  - [X] Path of the error
  - [ ] Test the path with Adams library for nested access.
- [ ] Add type spec to generated functions.
- [ ] Document special behaviour for `undefined`.

## TODO PoC FastApi
- [ ] How should I get example values into the mix? Some macro ?spec(example = 2, type = integer) that I write to integer and keep the example separate? Can that be done without IDEs going bananas?
  - [ ] An alternative is to add an example value to the router in the PoC
- [ ] Get types from function specs.
- [ ] Declerative router for Elli/Cowboy
- [ ] OpenAPI spec

## TODO Later
- [ ] Handle references beween modules: Can I get the same info by looking at a beam with debug_info? or should I export a __erldantic_info__/0 function?
- [ ] Only generate to_json from_json if not pressent. Call to_json / from_json instead of looking up type in type_info. This will allow user to override generate for specific DTs.
- [ ] to/from yaml
- [ ] to property based test generator

## Not in scope for now
- [ ] to/from Dynamo DB
- [ ] to/from grpc
- [ ] to/from sql


## Build
    $ rebar3 compile
