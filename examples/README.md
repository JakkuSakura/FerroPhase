# FerroPhase Examples

All example programs live in this directory with sequential numeric prefixes. Each file carries a documentation header and shebang so it can be run directly or through the CLI.

## Running Examples

```bash
# Make executable (one-time)
chmod +x examples/01_const_eval_basics.fp

# Execute directly
./examples/01_const_eval_basics.fp

# Or via the CLI
fp run examples/01_const_eval_basics.fp
```

## Catalog Overview

### Core Language Features

| Example | Focus | Demonstrates |
|---------|-------|--------------|
| `01_const_eval_basics.fp` | Const evaluation | Arithmetic, const blocks, struct defaults |
| `02_string_processing.fp` | String processing (future) | Placeholder for string intrinsics |
| `03_control_flow.fp` | Control flow | Nested if/else, boolean logic, runtime conditions |
| `04_struct_introspection.fp` | Type introspection | `sizeof!`, `field_count!`, `hasfield!`, transpilation |
| `05_struct_generation.fp` | Config-driven structs | Const toggles, conditional defaults |
| `06_struct_methods.fp` | Struct methods | `impl` blocks, methods, field access |

### Advanced Features

| Example | Focus | Demonstrates |
|---------|-------|--------------|
| `07_compile_time_validation.fp` | Static validation | Constraints, size checks, introspection |
| `08_error_tolerance.fp` | Error handling | Multiple diagnostics without abort |
| `09_metaprogramming_patterns.fp` | Code generation | Schema-driven structs, protocol enums |
| `10_higher_order_functions.fp` | Higher-order functions | Function passing, generics, composition |
| `11_print_showcase.fp` | Printing | Variadic `print`, formatting |
| `12_specialization_basics.fp` | Function specialization | Inlining, optimization, monomorphization |

### Type System

| Example | Focus | Demonstrates |
|---------|-------|--------------|
| `13_pattern_matching.fp` | Pattern matching | `match`, guards, destructuring |
| `14_loops.fp` | Loops | `while`, `for`, `loop`, break, continue |
| `15_type_arithmetic.fp` | Type arithmetic (future) | Type-level operations, struct composition |
| `16_enums.fp` | Enums | Unit, tuple, struct variants, discriminants |
| `17_traits.fp` | Traits | Trait bounds, default methods, impl |
| `18_generics.fp` | Generics | Type parameters, monomorphization |

**Note:** Examples marked "(future)" are placeholders for upcoming features.

See `docs/ConstEval.md` for background on the showcased features.
