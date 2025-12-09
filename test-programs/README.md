# Tiger Test Programs

This directory contains Tiger source files for testing compiler implementations across different languages (OCaml, Lean, Rust).

## Directory Structure

- `valid/` - Programs that should typecheck and compile successfully
- `type-errors/` - Programs that should produce specific type errors

## Valid Programs

These programs test correctly-typed Tiger code for implemented features:

1. **literals.tig** - Integer literal
2. **string_literal.tig** - String literal  
3. **arithmetic.tig** - Arithmetic operations (+, -, *, /)
4. **comparisons.tig** - Comparison operations (<, <=, >, >=, =, <>)
5. **if_then_else.tig** - If-then-else with matching branch types
6. **if_then_unit.tig** - If-then without else (must be unit type)
7. **while_loop.tig** - While loop with unit body
8. **sequence.tig** - Sequence of expressions
9. **function_no_params.tig** - Function with no parameters (requires let - not yet tested)
10. **function_with_params.tig** - Function with parameters (requires let - not yet tested)
11. **recursive_function.tig** - Recursive function (requires let - not yet tested)
12. **mutually_recursive.tig** - Mutually recursive functions (requires let - not yet tested)
13. **variable_declaration.tig** - Variable declaration with type annotation (requires let - not yet tested)
14. **variable_no_type.tig** - Variable declaration with type inference (requires let - not yet tested)

## Type Error Programs

These programs test that the type checker correctly detects and reports errors:

1. **undefined_variable.tig** - Reference to undefined variable
2. **undefined_function.tig** - Call to undefined function
3. **if_branches_mismatch.tig** - If branches with different types
4. **if_then_not_unit.tig** - If-then without else that doesn't produce unit
5. **if_condition_not_int.tig** - If condition that isn't int
6. **while_condition_not_int.tig** - While condition that isn't int
7. **while_body_not_unit.tig** - While body that doesn't produce unit
8. **arithmetic_on_string.tig** - Arithmetic on non-int types
9. **incompatible_equality.tig** - Equality comparison of incompatible types
10. **wrong_var_type.tig** - Variable type mismatch (requires let - not yet tested)
11. **wrong_arg_count.tig** - Function call with wrong number of arguments (requires let - not yet tested)
12. **wrong_arg_type.tig** - Function call with wrong argument type (requires let - not yet tested)
13. **function_return_mismatch.tig** - Function return type mismatch (requires let - not yet tested)
14. **assign_incompatible.tig** - Assignment type mismatch (requires let - not yet tested)
15. **variable_as_function.tig** - Using a variable as a function (requires let - not yet tested)

## Running Tests

### OCaml Implementation

```bash
cd TigerBookOcaml
dune exec test/integration/test_integration.exe
```

### Future Implementations

The test files in this directory can be used by Lean and Rust implementations once they have lexer, parser, and type checker components.

## Notes

- Currently, `let` expressions are not implemented, so tests requiring them are commented out
- The `()` syntax parses as an empty sequence (nil type), not unit
- Use `break` or assignment statements to produce unit type values
- Test files use Tiger comment syntax: `/* comment */`

