# FerroPhase Examples

All example programs now live directly in this directory with sequential numeric prefixes. Each file carries a documentation header and a shebang so it can be run either directly or through the CLI.

## Running Samples

```bash
# Make executable (one-time)
chmod +x examples/01_const_eval_basics.fp

# Execute directly
./examples/01_const_eval_basics.fp

# Or via the CLI
fp run examples/01_const_eval_basics.fp
```

To iterate through the full catalog:

```bash
for example in examples/[0-9][0-9]_*.fp; do
    echo "Running $example..."
    "$example"
    echo
done
```

## Catalog Overview

| Example | Focus | Demonstrates |
|---------|-------|--------------|
| `01_const_eval_basics.fp` | Const evaluation essentials | Arithmetic, branching, struct defaults |
| `02_string_processing.fp` | Compile-time strings | Concatenation, search, formatting |
| `03_control_flow.fp` | Control-flow tour | Nested `if`, boolean logic, numeric guards |
| `04_struct_introspection.fp` | Type analysis | `sizeof!`, `field_count!`, `hasfield!` |
| `05_struct_generation.fp` | Config-driven types | `t!` macro, feature toggles, vector specialisation |
| `06_struct_methods.fp` | Struct behaviour | `impl` blocks, method receivers, runtime math |
| `07_compile_time_validation.fp` | Static validation | Custom diagnostics, constraint checking |
| `08_error_tolerance.fp` | Error aggregation | Multiple diagnostics without aborting evaluation |
| `09_metaprogramming_patterns.fp` | Code-generation patterns | Schema-driven structs, protocol enums |
| `10_transpile_structs.fp` | Transpilation demo | Type reflection feeding external targets |
| `11_print_showcase.fp` | Unified printing demo | Variadic `print`, namespace calls, runtime formatting |

The auxiliary files (`rust_structs.*`, `transpile_example.*`) are kept alongside the examples because they serve as transpilation targets for `10_transpile_structs.fp`.

See `docs/ConstEval.md` for background on the showcased features.
