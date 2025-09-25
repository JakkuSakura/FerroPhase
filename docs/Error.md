# Error Handling and Multiple Error Reporting

## Current State Analysis

### Error Handling Architecture

FerroPhase currently uses a traditional Rust error handling approach with:

- **Core Error Types**: Located in `crates/fp-core/src/error.rs` and `crates/fp-optimize/src/error.rs`
- **CLI Diagnostics**: Enhanced error reporting with `miette` in `crates/fp-cli/src/diagnostics.rs`
- **Pipeline Integration**: Error propagation through compilation stages in `crates/fp-cli/src/pipeline.rs`

### Current Error Flow

The compilation pipeline follows this pattern:

```
Source → Parse → AST → HIR → THIR → MIR → LIR → LLVM IR → Binary
   ↓       ↓      ↓     ↓      ↓      ↓     ↓        ↓        ↓
 I/O    Syntax  Transform errors propagate with early returns
```

Each stage uses `.map_err()` and the `?` operator, causing **immediate termination** on the first error.

### Key Error Types

1. **SyntaxError**: Parse failures (`fp-core/src/error.rs:7`)
2. **OptimizationError**: Transformation and optimization failures (`fp-core/src/error.rs:10-23`)
3. **RuntimeError**: Interpretation failures (`fp-core/src/error.rs:32`)
4. **FerroPhaseError**: CLI-level diagnostics with source spans (`fp-cli/src/diagnostics.rs:25-75`)

### Current Problems

1. **Early Termination**: First error stops entire compilation
2. **Limited Context**: Users see only one error at a time
3. **Poor Development Experience**: Fix-one-error-at-a-time cycle is inefficient
4. **Incomplete Analysis**: Later stages may contain related errors that remain hidden

## Proposed Multiple Error Collection System

### Design Principles

1. **Error Tolerance**: Continue compilation through recoverable errors
2. **Comprehensive Reporting**: Collect and report multiple errors simultaneously
3. **Quality Diagnostics**: Maintain rich error context with source spans
4. **Performance**: Minimize overhead of error collection

### Core Components

#### 1. Error Collector

```rust
/// Central error collection system
pub struct ErrorCollector {
    errors: Vec<CompilationError>,
    warnings: Vec<CompilationWarning>,
    max_errors: usize,
    continue_on_error: bool,
}

#[derive(Debug, Clone)]
pub struct CompilationError {
    pub error_type: ErrorType,
    pub stage: CompilationStage,
    pub span: Option<Span>,
    pub message: String,
    pub code: Option<String>,
    pub source_context: Option<String>,
    pub suggestions: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum CompilationStage {
    Parse,
    AstToHir,
    HirToThir,
    ThirToMir,
    MirToLir,
    LirToLlvm,
    Codegen,
}

#[derive(Debug, Clone)]
pub enum ErrorType {
    Syntax,
    Type,
    Semantic,
    Optimization,
    Codegen,
}
```

#### 2. Error Collection API

```rust
impl ErrorCollector {
    pub fn new() -> Self { ... }
    
    pub fn with_max_errors(max_errors: usize) -> Self { ... }
    
    /// Add an error and decide whether to continue
    pub fn add_error(&mut self, error: CompilationError) -> ContinueCompilation {
        self.errors.push(error);
        if self.errors.len() >= self.max_errors {
            ContinueCompilation::Stop
        } else {
            ContinueCompilation::Continue
        }
    }
    
    pub fn add_warning(&mut self, warning: CompilationWarning) { ... }
    
    pub fn has_errors(&self) -> bool { ... }
    
    pub fn error_count(&self) -> usize { ... }
    
    pub fn into_report(self) -> CompilationReport { ... }
}

pub enum ContinueCompilation {
    Continue,
    Stop,
}
```

#### 3. Pipeline Integration

Update the pipeline to thread the error collector through each stage:

```rust
impl Pipeline {
    pub fn execute_with_error_collection(
        &self,
        input: PipelineInput,
        options: PipelineOptions,
    ) -> Result<(PipelineOutput, CompilationReport), CliError> {
        let mut collector = ErrorCollector::new()
            .with_max_errors(options.max_errors.unwrap_or(10));
            
        // Each stage now takes &mut ErrorCollector
        let ast = self.parse_with_errors(input, &mut collector)?;
        let hir = self.ast_to_hir_with_errors(ast, &mut collector);
        let thir = self.hir_to_thir_with_errors(hir, &mut collector);
        // ... continue through pipeline
        
        let report = collector.into_report();
        
        if report.has_fatal_errors() {
            Err(CliError::CompilationFailed(report))
        } else {
            Ok((output, report))
        }
    }
}
```

#### 4. Transformation Layer Changes

Update each transformation to support error recovery:

```rust
// Before: Early return on error
impl HirGenerator {
    pub fn transform(&mut self, expr: &BExpr) -> Result<hir::Expr, Error> {
        match self.lower_expr(expr) {
            Ok(hir_expr) => Ok(hir_expr),
            Err(e) => Err(e), // Early return - no other errors seen
        }
    }
}

// After: Collect errors and attempt recovery
impl HirGenerator {
    pub fn transform_with_errors(
        &mut self, 
        expr: &BExpr, 
        collector: &mut ErrorCollector
    ) -> Option<hir::Expr> {
        match self.lower_expr(expr) {
            Ok(hir_expr) => Some(hir_expr),
            Err(e) => {
                collector.add_error(CompilationError {
                    error_type: ErrorType::Semantic,
                    stage: CompilationStage::AstToHir,
                    span: Some(expr.span),
                    message: e.to_string(),
                    code: Some("E0001".to_string()),
                    source_context: None,
                    suggestions: vec!["Try checking your syntax".to_string()],
                });
                
                // Attempt recovery with a placeholder/skip
                self.attempt_recovery(expr)
            }
        }
    }
}
```

### Error Recovery Strategies

#### 1. Placeholder Insertion
For missing or malformed constructs, insert placeholder values:

```rust
// Type errors: Insert `unknown` type
// Missing expressions: Insert unit `()`
// Failed function calls: Insert error sentinel value
```

#### 2. Skip and Continue
Skip problematic items while processing others:

```rust
// Skip malformed functions but process others in the file  
// Skip problematic struct fields but process the struct
```

#### 3. Best-Effort Inference
Attempt reasonable defaults when information is missing:

```rust
// Infer variable types from usage context
// Assume missing return types are `()`
// Use context for missing generic parameters
```

### Reporting System

#### 1. Error Grouping
Group related errors to reduce noise:

```rust
pub struct ErrorGroup {
    pub primary_error: CompilationError,
    pub related_errors: Vec<CompilationError>,
    pub common_cause: Option<String>,
}
```

#### 2. Progressive Disclosure
Show high-priority errors first, with option to see all:

```
Error: Type mismatch in function 'calculate'
  --> src/main.fp:15:12
15 |   let x = "hello" + 42;
   |           ^^^^^^^^^^^^^ expected number, found string + number

Error: Undefined variable 'result'
  --> src/main.fp:18:5  
18 |   result
   |   ^^^^^^ not found in this scope

Found 2 errors (showing first 2)
Run with --show-all-errors to see all 7 errors
```

#### 3. Contextual Information
Enhance errors with helpful context:

```rust
impl CompilationError {
    pub fn with_source_context(mut self, context: String) -> Self {
        self.source_context = Some(context);
        self
    }
    
    pub fn with_suggestion(mut self, suggestion: String) -> Self {
        self.suggestions.push(suggestion);
        self
    }
    
    pub fn with_error_code(mut self, code: String) -> Self {
        self.code = Some(code);
        self
    }
}
```

## Implementation Plan

### Phase 1: Core Infrastructure
1. Implement `ErrorCollector` and related types in `fp-core/src/error.rs`
2. Add error recovery helpers and strategies
3. Update `Result` types to include error collection context

### Phase 2: Parser Integration  
1. Modify parser to continue on syntax errors where possible
2. Add error recovery in parsing (skip to next statement/expression)
3. Collect parse errors in `ErrorCollector`

### Phase 3: Transformation Pipeline
1. Update each transformation stage (`AST→HIR`, `HIR→THIR`, etc.)
2. Add error recovery strategies specific to each transformation
3. Thread `ErrorCollector` through the entire pipeline

### Phase 4: Enhanced Reporting
1. Implement error grouping and prioritization
2. Add CLI flags for error display options (`--max-errors`, `--show-all-errors`)
3. Integrate with existing `miette` diagnostic system

### Phase 5: Testing and Refinement
1. Add comprehensive test cases for multiple error scenarios
2. Benchmark performance impact of error collection
3. Refine error recovery strategies based on real-world usage

## Configuration Options

Users should be able to control error behavior:

```toml
# FerroPhase.toml
[compilation]
max_errors = 10              # Stop after N errors
continue_on_error = true     # Whether to attempt error recovery  
show_all_errors = false      # Show all errors vs. progressive disclosure
error_detail_level = "normal" # "minimal" | "normal" | "verbose"
```

CLI flags:
```bash
fp compile --max-errors 20 --show-all-errors src/main.fp
fp compile --no-error-recovery src/main.fp  # Use current behavior
```

## Benefits

1. **Better Developer Experience**: See multiple issues at once
2. **Faster Development Cycle**: Fix several problems in one iteration  
3. **Better Error Quality**: Context from later stages can improve earlier error messages
4. **Educational Value**: Understanding how errors relate to each other
5. **IDE Integration**: Better support for real-time error highlighting

## Potential Challenges

1. **Complexity**: Error recovery adds complexity to every transformation
2. **Performance**: Error collection may slow down compilation
3. **Error Quality**: Recovery attempts might produce confusing secondary errors
4. **Testing**: Need comprehensive test coverage for error scenarios

## Migration Strategy

- Implement alongside existing error handling (feature flag)
- Make error collection opt-in initially (`--collect-errors` flag)  
- Gradually migrate stages to support error collection
- Eventually make multiple error collection the default behavior

The goal is to provide a robust, user-friendly error system that helps developers identify and fix multiple issues efficiently while maintaining the high-quality diagnostic information FerroPhase already provides.