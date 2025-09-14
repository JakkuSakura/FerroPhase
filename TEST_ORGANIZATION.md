# Test Organization

This document describes how tests are organized across the FerroPhase crates.

## Test Structure

### fp-rust-lang/tests/
**Focus: Language parsing and syntax accuracy**
- `test_language_parsing.rs` - Tests that Rust language constructs parse correctly into AST nodes
- Validates parsing accuracy for literals, operators, expressions, precedence
- Ensures roundtrip parsing (parse -> print -> parse) works correctly

### fp-core/tests/
**Focus: Core AST and context functionality**  
- `test_core_ast.rs` - Tests pure fp-core capabilities without optimization infrastructure
- Validates AST value creation, comparison, parsing, serialization
- Tests context management and basic error handling
- Focuses on core data structures and functionality

### fp-optimize/tests/
**Focus: Optimization-specific functionality**
- `test_const_evaluation_pure.rs` - Pure const evaluation without basic language features
- `test_language_with_const_eval.rs` - Integration tests showing language features WITH const evaluation
- `test_basic_evaluation.rs` - Basic language evaluation through optimization infrastructure  
- `test_interpreter.rs` - Interpreter integration with optimization passes
- Various pass-specific tests (const eval phases, type validation, etc.)

## Test Philosophy

**Separation of Concerns:**
- **Parsing** (fp-rust-lang): "Does this syntax parse correctly?"
- **Core functionality** (fp-core): "Do the core AST and context systems work correctly?"
- **Evaluation & Optimization** (fp-optimize): "Does this expression evaluate/optimize correctly?"

**Basic Language Features Testing:**
- **Core functionality**: Tests in fp-core focus on AST manipulation and data structures
- **Runtime evaluation**: Tests in fp-optimize demonstrate expression evaluation through optimization infrastructure  
- **Const evaluation**: Tests in fp-optimize demonstrate compile-time optimization and folding

This structure ensures clear boundaries and helps identify where issues occur in the compilation pipeline.