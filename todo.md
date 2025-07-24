## TODO PoC
- [ ] Better README

## TODO Release
- [ ] Run the thing on lots of modules to see if the abstract code can handle all type attributes.
- [ ] Cleanup erldantic internal types. Use records instead of tagged tuples.
- [ ] #a_type can not have any type in type field.
- [ ] Test error message path with Adams library for nested access
- [ ] Support otp 28
  - [ ] -nominal my_nominal_type() :: Type.
- [ ] remove the no_pt everywhere.

## TODO Performance improvements
- [ ] For each literal atom, convert to binary in erldantic_abstract_code, so that we don't have to do binary_to_existing_atom so see that the values match
- [ ] Do work in erldantic_abstract_code
  - [ ] user_type_ref -> just switches to another type with args, can be done in  erldantic_abstract_code.
  - [ ] do record_apply_args in erldantic_abstract_code.
  - [ ] erldantic_json:type_replace_vars can also be done in erldantic_abstract_code

## TODO PoC FastApi
- [ ] Types for functions in erldantic_abstract_code
- [ ] How should I get example values into the mix? Some macro ?spec(example = 2, type = integer) that I write to integer and keep the example separate? Can that be done without IDEs going bananas?
  - [ ] An alternative is to add an example value to the router in the PoC
- [ ] Get types from function specs.
- [ ] Declerative router for Elli/Cowboy
- [ ] OpenAPI spec

## For completeness
- [ ] handle functions and all types of lists in type_replace_vars/3

## Not in scope for now
- [ ] to/from Dynamo DB
- [ ] to/from yaml

## Maybe never in scope?
- [ ] to/from grpc
- [ ] to/from sql
