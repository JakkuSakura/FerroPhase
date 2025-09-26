# FerroPhase Examples

This directory contains example programs demonstrating FerroPhase's const evaluation and metaprogramming capabilities.

## Running Examples

### Using Shebang (Recommended)

All examples include a shebang line for direct execution:

```bash
# Make executable (if not already)
chmod +x examples/01_basic_const_evaluation.fp

# Run directly
./examples/01_basic_const_evaluation.fp

# Or from anywhere
examples/01_basic_const_evaluation.fp
```

### Using fp CLI

```bash
# Compile and run
fp run examples/01_basic_const_evaluation.fp

# Just compile
fp compile examples/01_basic_const_evaluation.fp --target rust

# Evaluate const expressions only
fp eval examples/01_basic_const_evaluation.fp
```

## Examples Overview

| Example | Focus | Demonstrates |
|---------|-------|-------------|
| `01_basic_const_evaluation.fp` | Const evaluation essentials | Arithmetic, branching, struct defaults |
| `02_string_processing.fp` | Compile-time strings | Concatenation, search, formatting |
| `03_control_flow.fp` | Control flow tour | Nested `if`, boolean logic, numeric guards |
| `04_struct_introspection.fp` | Type analysis | `sizeof!`, `field_count!`, `hasfield!` |
| `05_struct_generation.fp` | Config-driven types | `t!` macro, feature toggles, vector specialisation |
| `06_compile_time_validation.fp` | Static validation | Custom diagnostics, constraint checking |
| `07_metaprogramming_patterns.fp` | Code generation patterns | Schema driven structs, protocol enums |
| `08_struct_methods.fp` | Struct behaviour | `impl` blocks, method receivers, runtime math |

## Const Evaluation Features

### Highlights covered in the examples
- ✅ Const arithmetic, branching, and aggregation (01)
- ✅ Compile-time string processing and templating (02)
- ✅ Control-flow analysis and boolean logic (03)
- ✅ Introspection macros such as `sizeof!`, `field_count!`, `hasfield!` (04)
- ✅ `t!` macro for configuration-driven struct generation (05)
- ✅ Custom compile-time diagnostics with `compile_error!` / `compile_warning!` (06)
- ✅ Schema- and protocol-driven code generation patterns (07)
- ✅ `impl` blocks, method receivers, and runtime struct usage (08)

### Longer-term roadmap
- 🔄 Ergonomic declarative type syntax
- 🔄 Trait-style constraints for generated implementations
- 🔄 Richer metaprogramming intrinsics with side-effect tracking
- 🔄 Tooling to surface example output in documentation builds

## Running All Examples

```bash
# Run all examples in sequence
for example in examples/*.fp; do
    echo "Running $example..."
    $example
    echo
done
```

## Example Development

When creating new examples:

1. **Add shebang**: `#!/usr/bin/env fp run`
2. **Include documentation**: Explain what the example demonstrates
3. **Use const blocks**: Show compile-time computation
4. **Add future sections**: Describe @ intrinsic capabilities when available
5. **Make executable**: `chmod +x new_example.fp`

## Implementation Status

The examples demonstrate FerroPhase's const evaluation vision. Some features require:
- Macro system for introspection functions
- Side effect tracking system for metaprogramming
- 3-phase const evaluation implementation
- Declarative type syntax support

See `docs/ConstEval.md` for implementation details.
