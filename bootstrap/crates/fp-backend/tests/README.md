# FerroPhase Optimization & Const Evaluation Tests

This directory contains comprehensive tests for FerroPhase's optimization and const evaluation systems.

## Test Organization

### Core Infrastructure Tests
- **test_basic_evaluation.rs** - Basic interpreter functionality (arithmetic, comparisons, etc.)
- **test_interpreter.rs** - Core interpreter features and expression evaluation

### Const Evaluation System Tests
- **test_const_evaluation_pure.rs** - Pure const evaluation without side effects
- **test_const_eval_comprehensive.rs** - Full const evaluation pipeline tests
- **test_const_eval_phases.rs** - Individual phase testing for the 10-pass architecture
- **test_language_with_const_eval.rs** - Language integration with const evaluation

### Specialized Feature Tests
- **test_struct_creation_const_eval.rs** - Parametric struct creation and metaprogramming
- **test_query_interpreter.rs** - Query system integration
- **test_specializer.rs** - Generic specialization

### Architecture Phase Tests
- **test_pass3_generic_contexts.rs** - Generic context handling (Pass 3)
- **test_pass5_type_validation.rs** - Type system validation (Pass 5)  
- **test_pass8_code_generation.rs** - Code generation and side effects (Pass 8)
- **test_pass9_final_validation.rs** - Final validation and consistency (Pass 9)

## Test Categories

### 🟢 Working Tests (Passing)
- Basic evaluation: `test_basic_evaluation.rs` ✅ (10/10 tests)
- Const eval phases: `test_const_eval_phases.rs` ✅ (19/21 tests) - 2 minor failures
- All other test files ✅

### 🔧 Tests Needing Fixes
- `test_const_eval_phases.rs`:
  - `test_edge_cases_nested_scopes` - Context resolution issue
  - `test_phase3_code_generation_ast_modification` - Missing struct type

### 🚧 Tests To Enhance
- Add more comprehensive const evaluation scenarios
- Add metaprogramming intrinsic tests
- Add side effect tracking tests
- Add iterative evaluation tests

## Running Tests

```bash
# Run all optimization tests
cargo test -p fp-backend

# Run specific test categories
cargo test -p fp-backend test_basic_evaluation
cargo test -p fp-backend test_const_eval_phases
cargo test -p fp-backend test_struct_creation_const_eval

# Run with output
cargo test -p fp-backend -- --nocapture
```

## Development Notes

- Tests use `FerroPhaseParser` helpers for convenient AST construction
- Context management through `SharedScopedContext` for scoped variable resolution
- Current interpreter handles basic expressions well, const evaluation intrinsics need implementation
