# Type Info Restructuring - Remaining Migration Steps

## Overview
This document outlines the remaining steps to complete the migration from the old flat map-based `type_info()` structure to the new organized `erldantic_type_info` module approach.

## ✅ Completed Work

1. **Core Infrastructure**
   - ✅ Added `#type_info{}` record with separate fields for types, records, and functions
   - ✅ Created `erldantic_type_info` module with complete accessor API
   - ✅ Updated `erldantic:type_info()` type definition
   - ✅ Removed `#ed_function_spec{}` from `ed_type()` union
   - ✅ Simplified `ed_type_reference()` to only include type references
   - ✅ Updated `erldantic_abstract_code:types_in_module/1` to return new structure

2. **Test Updates**
   - ✅ Fully updated `test/function_specs_test.erl` (4 tests passing)

## 🔄 Remaining Steps

### Step 1: Update Remaining Test Files

The following test files need to be updated from the old pattern:
```erlang
Types = erldantic_abstract_code:types_in_module(?MODULE),
Type = maps:get({type, TypeName, Arity}, Types)
```

To the new pattern:
```erlang
TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),
{ok, Type} = erldantic_type_info:get_type(TypeInfo, TypeName, Arity)
```

**Files to update:**
- `test/simple_types_test.erl` - Multiple type access patterns
- `test/tuple_test.erl` - Type access patterns
- `test/same_type_name_test.erl` - Type access patterns
- `test/same_name_type_test.erl` - Type access patterns
- `test/record_type_alias_test.erl` - Type access patterns
- `test/record_test.erl` - Record access patterns (use `get_record/2`)
- `test/record_arg_type_test.erl` - Type access patterns
- `test/opaque_test.erl` - Type access patterns
- `test/maybe_improper_list_test.erl` - Type access patterns
- `test/map_arg_type_test.erl` - Type access patterns
- `test/integer_literal_test.erl` - Type access patterns
- `test/fun_test.erl` - Type access patterns
- `test/external_type_test.erl` - Type access patterns

### Step 2: Update Consumer Modules (if any)

Check if any modules other than tests consume `erldantic_abstract_code:types_in_module/1` directly:
- `src/erldantic_util.erl` - Only passes through, likely no changes needed
- `src/erldantic_module_types.erl` - Only passes through, likely no changes needed

Search for any other direct usage:
```bash
grep -r "types_in_module" src/ --include="*.erl"
```

### Step 3: Update erldantic_json and Other Core Modules

Find modules that might be using the old `{type, Name, Arity}` key pattern:
```bash
grep -r "maps:get.*{type," src/ --include="*.erl"
grep -r "maps:find.*{type," src/ --include="*.erl"
```

These will need to be updated to use `erldantic_type_info` accessors.

### Step 4: Systematic Test Updates

For each test file, the update pattern is:

1. **Variable rename**: `Types -> TypeInfo`
2. **Type access**: `maps:get({type, Name, Arity}, Types)` → `{ok, Type} = erldantic_type_info:get_type(TypeInfo, Name, Arity)`
3. **Record access**: `maps:get({record, Name}, Types)` → `{ok, Record} = erldantic_type_info:get_record(TypeInfo, Name)`

### Step 5: Verification

1. **Compile check**: `rebar3 compile`
2. **Full test suite**: `rebar3 eunit`
3. **Type checking**: `make build-test` (includes eqwalizer)
4. **Property-based tests**: `rebar3 proper`

### Step 6: Clean-up (Optional)

1. Remove `to_legacy_map/1` function if no longer needed
2. Add deprecation warnings for any legacy patterns
3. Update documentation

## Migration Script Template

For test files, this sed command pattern can help automate updates:

```bash
# Update variable name
sed -i 's/Types = erldantic_abstract_code:types_in_module/TypeInfo = erldantic_abstract_code:types_in_module/g' test/*.erl

# Update type access pattern (requires manual review)
# maps:get({type, NAME, ARITY}, Types) -> erldantic_type_info:get_type(TypeInfo, NAME, ARITY)
```

**Note**: Manual review is required because the new API returns `{ok, Value} | error` which may require updating assertion patterns.

## Testing Strategy

1. Update files one by one
2. Test each file individually: `rebar3 eunit --module=MODULE_NAME`
3. Verify no regressions in functionality
4. Check that error handling works correctly with the new `{ok, Value} | error` return pattern

## Risk Mitigation

- Keep backup of original files
- Test incrementally to catch issues early
- Use `erldantic_type_info:to_legacy_map/1` for temporary compatibility if needed
- Ensure all existing functionality is preserved

## Success Criteria

- ✅ All tests pass
- ✅ No compilation errors or warnings
- ✅ Type checking passes
- ✅ Property-based tests pass
- ✅ Clean separation of types, records, and functions
- ✅ Improved API usability and type safety