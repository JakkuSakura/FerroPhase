# FerroPhase Examples

All example programs live in this directory with sequential numeric prefixes. Each file carries a documentation header
and shebang so it can be run directly or through the CLI.

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

| Example                      | Focus                      | Demonstrates                                          |
|------------------------------|----------------------------|-------------------------------------------------------|
| `01_const_eval_basics.fp`    | Const evaluation           | Arithmetic, const blocks, struct defaults             |
| `02_string_processing.fp`    | String processing           | Const string intrinsics and slicing                   |
| `03_control_flow.fp`         | Control flow               | Nested if/else, boolean logic, runtime conditions     |
| `04_struct_introspection.fp` | Type introspection         | `sizeof!`, `field_count!`, `hasfield!`, transpilation |
| `05_struct_generation.fp`    | Config-driven structs      | Const toggles, conditional defaults                   |
| `06_struct_methods.fp`       | Struct methods             | `impl` blocks, methods, field access                  |

### Advanced Features

| Example                          | Focus                   | Demonstrates                             |
|----------------------------------|-------------------------|------------------------------------------|
| `07_compile_time_validation.fp`  | Static validation       | Constraints, size checks, introspection  |
| `08_metaprogramming_patterns.fp` | Code generation         | Schema-driven structs, protocol enums    |
| `09_higher_order_functions.fp`   | Higher-order functions  | Function passing, generics, composition  |
| `10_print_showcase.fp`           | Printing                | Variadic `print`, formatting             |
| `11_specialization_basics.fp`    | Function specialization | Inlining, optimization, monomorphization |
| `18_comptime_collections.fp`     | Const collections       | `Vec`/`HashMap` construction in const    |
| `20_quote_splice.fp`             | Quote/splice            | Typed quote tokens, const block splicing |
| `21_build_blocks.fp`             | Build blocks            | Item generation with `quote<item>` lists |
| `23_runtime_collections.fp`      | Runtime collections     | List/map indexing and linear search      |

### Type System

| Example                  | Focus            | Demonstrates                                |
|--------------------------|------------------|---------------------------------------------|
| `12_pattern_matching.fp` | Pattern matching | `match`, guards, destructuring              |
| `13_loops.fp`            | Loops            | `while`, `for`, `loop`, break, continue     |
| `14_type_arithmetic.fp`  | Type arithmetic  | Type-level operations, struct composition   |
| `15_enums.fp`            | Enums            | Unit, tuple, struct variants, discriminants |
| `16_traits.fp`           | Traits           | Trait bounds, default methods, impl         |
| `17_generics.fp`         | Generics         | Type parameters, monomorphization           |

### Interoperability

| Example                | Focus                       | Demonstrates                                                  |
|------------------------|-----------------------------|---------------------------------------------------------------|
| `19_wit_interface.wit` | WebAssembly Interface Types | Defining interfaces & records consumable via the WIT frontend |

See `docs/ConstEval.md` for background on the showcased features.
