## TODO PoC
- [ ] Things found in OTP
  - [ ] The type `nil`
  - [ ] Having a atom as default value for a record field
```erlang
-record(docs_v1, {anno,
                  beam_language = erlang,
                  ...}).
```

- [ ] Error message on not supported json types:
  - [ ] tuple, pid, port, fun, reference
- [ ] Cleanup erldantic internal types. Use records instead of tagged tuples. #a_type can not have any type in type field.

## TODO Release
- [ ] Run the thing on lots of modules to see if the abstract code can handle all type attributes.
- [x] Document special behaviour for `undefined`.
- [ ] All other types expect `pid()`, `port()`, `Fun` and `reference()`.
    - [ ] Number? or will it always show up as integer()|float()?
    - [ ] non_empty ... binary, bitstring,
    - [ ] any()/term() and dynamic()? none()?
- [ ] Test error message path with Adams library for nested access

## TODO If I go with the parse transform:
- [ ] Add type spec to generated functions.

## TODO PoC FastApi
- [ ] How should I get example values into the mix? Some macro ?spec(example = 2, type = integer) that I write to integer and keep the example separate? Can that be done without IDEs going bananas?
  - [ ] An alternative is to add an example value to the router in the PoC
- [ ] Get types from function specs.
- [ ] Declerative router for Elli/Cowboy
- [ ] OpenAPI spec

## TODO Later
- [x] Handle references beween modules: Can I get the same info by looking at a beam with debug_info? or should I export a __erldantic_info__/0 function?
- [ ] Only generate to_json from_json if not pressent. Call to_json / from_json instead of looking up type in type_info. This will allow user to override generate for specific DTs.
- [ ] to/from yaml
- [ ] to property based test generator


## Not in scope for now
- [ ] to/from Dynamo DB
- [ ] to/from grpc
- [ ] to/from sql
