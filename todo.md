## TODO minor fixes
- [ ] Test error message path better
- [ ] Support otp 28
  - [ ] -nominal my_nominal_type() :: Type.

## TODO Performance improvements
- [ ] For each literal atom, convert to binary in erldantic_abstract_code, so that we don't have to do binary_to_existing_atom so see that the values match
- [ ] Do work in erldantic_abstract_code
  - [ ] user_type_ref -> just switches to another type with args, can be done in  erldantic_abstract_code.
  - [ ] do record_apply_args in erldantic_abstract_code.
  - [ ] erldantic_json:type_replace_vars can also be done in erldantic_abstract_code

## TODO PoC FastApi (elli_openapi)
- [ ] How should I get example values into the mix? Some macro ?spec(example = 2, type = integer) that I write to integer and keep the example separate? Some module attribute? Can that be done without IDEs going bananas?
  - [ ] An alternative is to add an example value to the router

## Not in scope for now
- [ ] to/from Dynamo DB
- [ ] to/from yaml
- [ ] to/from xml (and xml schemas?)

## Maybe never in scope?
- [ ] to/from grpc: Versioned protocols should probably never be generated?
- [ ] to/from sql: There are so many options that the developer should care about (at least when creating tables), that it doesn't make sense to generate it.
