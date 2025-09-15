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
| `01_basic_const_evaluation.fp` | Basic const computation | Arithmetic, conditionals, struct defaults |
| `02_string_processing.fp` | String operations | Concatenation, length calculations |
| `03_struct_introspection.fp` | Type analysis | Size calculation, field enumeration |
| `04_dynamic_struct_creation.fp` | Configuration-driven structs | Feature flags, conditional fields |
| `05_parametric_structs.fp` | Template-like generation | Dimension-based vectors |
| `06_config_driven_generation.fp` | Platform-specific code | Target platform adaptation |
| `07_compile_time_validation.fp` | Static assertions | Size limits, constraint checking |
| `08_generic_specialization.fp` | Type-based optimization | Container specialization |

## Const Evaluation Features

### Current Capabilities
- ✅ Basic arithmetic and boolean operations
- ✅ Conditional compilation with `if`/`else`
- ✅ String concatenation with `concat!()`
- ✅ Struct creation and field access
- ✅ Configuration-driven struct generation

### Future Capabilities (Pending Implementation)
- 🔄 `sizeof!(Type)` - type size introspection
- 🔄 `field_count!(Type)` - field enumeration
- 🔄 `hasfield!(Type, "name")` - field existence checks
- 🔄 `type Name = { ... }` - declarative struct creation
- 🔄 `compile_error!("message")` - compile-time errors
- 🔄 `type T = { ...Base, extra: Type }` - type inheritance

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