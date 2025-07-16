## TODO PoC
  - [ ] More tests
    - [x] Test that I can parse (but don't use for now) default values on record fields
    - [ ] -opaque my_opaq_type() :: Type.

## TODO Release
- [ ] Run the thing on lots of modules to see if the abstract code can handle all type attributes.
- [ ] Cleanup erldantic internal types. Use records instead of tagged tuples.
- [ ] #a_type can not have any type in type field.
- [ ] Test error message path with Adams library for nested access
- [ ] Support otp 28
  - [ ] -nominal my_nominal_type() :: Type.

## TODO PoC FastApi
- [ ] Types for functions in erldantic_abstract_code
- [ ] How should I get example values into the mix? Some macro ?spec(example = 2, type = integer) that I write to integer and keep the example separate? Can that be done without IDEs going bananas?
  - [ ] An alternative is to add an example value to the router in the PoC
- [ ] Get types from function specs.
- [ ] Declerative router for Elli/Cowboy
- [ ] OpenAPI spec

## Not in scope for now
- [ ] to/from Dynamo DB
- [ ] to/from yaml

## Maybe never in scope?
- [ ] to/from grpc
- [ ] to/from sql
