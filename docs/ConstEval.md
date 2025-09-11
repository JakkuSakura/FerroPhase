# FerroPhase Const Evaluation: Complete Design & Implementation Plan

## Executive Summary

**Architecture Decision:** AST-centric const evaluation with direct source-to-source compilation

```
Source Code → AST (enriched with const evaluation) → Target Source Code
```

**Key Insight:** AST interpreters are perfect for const evaluation - they're simple, provide excellent error reporting, and handle metaprogramming naturally.

# Refined Mini Passes for Const Evaluation

Based on the type system integration and interpreter reuse architecture, here are the refined mini passes:

## Phase 1: Setup & Discovery

### Pass 1: **Type System Integration Setup**
- Establish bidirectional connection between const evaluator and type system
- Initialize shared type registry and query interfaces
- Set up type introspection capabilities (`@sizeof`, `@reflect_fields` infrastructure)
- Cache frequently accessed type information

### Pass 2: **Const Discovery & Dependency Analysis**
- Find all const blocks, const expressions, and intrinsic calls
- Build dependency graph between const blocks
- Identify type dependencies (which const blocks need type information)
- Compute initial evaluation order with topological sort
- Detect circular dependencies early

### Pass 3: **Generic Context Preparation**
- Identify generic parameters that will be specialized by const evaluation
- Set up evaluation contexts for different generic instantiations
- Prepare type variable mappings for const evaluation scope

## Phase 2: Iterative Evaluation & Feedback

### Pass 4: **Const Evaluation with Type Queries** *(Iterative)*
- Evaluate const blocks in dependency order
- Query type system for introspection operations
- Execute metaprogramming intrinsics
- Collect generated types, fields, methods, and impl blocks
- **May generate new const blocks** → triggers iteration

### Pass 5: **Type System Update & Validation** *(Iterative)*
- Feed generated types back to type system
- Validate new type definitions for consistency
- Update type registry with new types
- Resolve any type conflicts or errors
- **If new types affect existing const blocks** → triggers iteration

### Pass 6: **Dependency Re-analysis** *(Conditional)*
- Re-analyze dependencies if new const blocks were generated
- Update evaluation order if dependency graph changed
- Check for new circular dependencies
- **Only runs if Pass 4-5 generated new const constructs**

## Phase 3: Specialization & Finalization

### Pass 7: **Generic Specialization**
- Use const evaluation results to specialize generic types
- Create concrete type instances based on const parameters
- Propagate specializations through type hierarchy
- Update type system with specialized instances

### Pass 8: **Code Generation & AST Modification**
- Apply all collected side effects to AST
- Add generated fields to structs
- Insert generated methods and impl blocks
- Create new type definitions from metaprogramming
- Maintain source location tracking for generated code

### Pass 9: **Final Type Validation & Integration**
- Perform final type checking with all generated types
- Ensure all type references are resolved
- Validate generic constraints with specialized types
- Freeze type system state for runtime compilation

### Pass 10: **Const Cleanup & Optimization**
- Remove evaluated const blocks (no longer needed)
- Replace const expressions with computed literal values
- Eliminate dead const variables
- Optimize generated code patterns

## Key Architectural Features

### **Iterative Execution**
Passes 4-6 form an **iterative loop** because const evaluation can generate new const blocks:
```
┌─> Pass 4: Const Evaluation ────┐
│                                ▼
└── Pass 6: Re-analysis ◄── Pass 5: Type Update
    (if changes)
```

### **Shared State Management**
```
Type System ◄─────────────────► Const Evaluator
     ▲                               ▲
     │                               │
     ▼                               ▼
Pass Results Storage ◄────────► Dependency Graph
```

### **Bailout Conditions**
- **Maximum iterations**: Prevent infinite const evaluation loops
- **Dependency cycles**: Detected and reported as errors
- **Type validation failures**: Stop evaluation if type system becomes inconsistent

### **Incremental Updates**
- Only re-evaluate const blocks affected by type system changes
- Cache evaluation results when possible
- Track which passes need to re-run based on changes

## Pass Coordination Example

```
Initial State: 3 const blocks discovered

Iteration 1:
  Pass 4: Evaluates blocks A, B, C → generates new type T
  Pass 5: Adds type T to type system → validates successfully
  Pass 6: No new const blocks → continue

Iteration 2: (skipped - no new const blocks)

Pass 7: Specializes generic G<X> with const value 42 → G<42>
Pass 8: Adds generated methods to struct S
Pass 9: Final validation of all types
Pass 10: Removes const blocks, optimizes
```

## Core AST Infrastructure

### Enhanced AST Design

```rust
// AST nodes carry both syntax and semantic information
#[derive(Debug, Clone)]
struct BlockStmt {
    // Core statement types
    kind: BlockStmtKind,

    // Const evaluation state
    const_evaluation_state: ConstEvalState,

    // Source mapping for errors
    span: Span,
}
#[derive(Debug, Clone)]
enum BlockStmtKind {
    Let { name: String, ty: Option<AstType>, value: AstExpr },
    Expr(AstExpr),

    // Regular control flow - no special const variants
    // Const evaluation works with existing AST structures
}
#[derive(Debug, Clone)]
enum ConstEvalState {
    NotEvaluated,
    Evaluating,      // Prevent infinite recursion
    Evaluated,
    Error(String),
}
```
### Step 2: Core Value System

```rust
#[derive(Debug, Clone, PartialEq)]
enum AstValue {
    // Primitives
    Int(ValueInt),
    Bool(ValueBool),
    Decimal(ValueDecimal),
    String(ValueString),
    Unit(ValueUnit),

    // Composite
    List(ValueList),
    Struct(ValueStruct),
    Tuple(ValueTuple),

    // Meta-values for code generation
    Type(AstType),
    Expr(Box<AstExpr>),           // Generated code
    FieldDescriptor {
        name: String,
        type_id: TypeId,
        attributes: Vec<String>,
    },
}
impl AstValue {
    fn as_bool(&self) -> Result<bool, Error> { /* ... */ }
    fn as_list(&self) -> Result<&ValueList, Error> { /* ... */ }
    fn as_type(&self) -> Result<&AstType, Error> { /* ... */ }
    // ... other conversions
}
## Phase 2: AST Interpreter Core

### Step 3: Basic Interpreter Structure

```rust
struct ConstInterpreter {
    // Execution context
    scopes: ScopeChain,
    call_stack: Vec<CallFrame>,

    // Performance optimizations
    node_cache: HashMap<ExprId, AstValue>,

    // Type system integration
    type_registry: TypeRegistry,

    // Error reporting
    error_reporter: ErrorReporter,
}
impl ConstInterpreter {
    pub fn new() -> Self { /* ... */ }

    // Main evaluation entry points
    pub fn evaluate_block(&mut self, block: &ExprBlock) -> Result<AstValue, Error>;
    pub fn evaluate_expression(&mut self, expr: &AstExpr) -> Result<AstValue, Error>;
    pub fn evaluate_statement(&mut self, stmt: &BlockStmt) -> Result<(), Error>;
}
#[derive(Debug)]
struct EvalResult {
    value: AstValue,           // Final value
    side_effects: Vec<SideEffect>,       // Code generation side effects
}
#[derive(Debug)]
enum SideEffect {
    GenerateField { name: String, type_id: TypeId },
    GenerateMethod { name: String, body: ValueFunction },
    GenerateImpl { trait_name: String, methods: Vec<ValueFunction> },
}
### Step 4: Expression Evaluation

```rust
impl ConstInterpreter {
    fn evaluate_expression(&mut self, expr: &AstExpr) -> Result<AstValue, Error> {
        // Check cache first
        if let Some(cached) = self.node_cache.get(&expr.id()) {
            return Ok(cached.clone());
        }

        let result = match expr {
            AstExpr::Value(val) => self.evaluate_value(val),
            AstExpr::Locator(loc) => self.evaluate_locator(loc),
            AstExpr::BinOp(binop) => {
                let l = self.evaluate_expression(&binop.lhs)?;
                let r = self.evaluate_expression(&binop.rhs)?;
                self.apply_binary_op(binop.kind, l, r, binop.span)
            }
            AstExpr::Invoke(invoke) => {
                self.evaluate_function_call(&invoke.target, &invoke.args, invoke.span)
            }
            AstExpr::If(if_expr) => {
                let condition = self.evaluate_expression(&if_expr.cond)?;
                if condition.as_bool()? {
                    self.evaluate_expression(&if_expr.then)
                } else if let Some(else_branch) = &if_expr.elze {
                    self.evaluate_expression(else_branch)
                } else {
                    Ok(AstValue::unit())
                }
            }
            AstExpr::Block(block) => self.evaluate_block(block),
            // Handle other expression types...
            _ => Err(Error::Generic("Unsupported expression type".to_string()))
        }?;

        // Cache pure expressions
        if self.is_pure_expression(expr) {
            self.node_cache.insert(expr.id(), result.clone());
        }

        Ok(result)
    }

    fn apply_binary_op(&self, op: BinOpKind, left: AstValue, right: AstValue, span: Span)
        -> Result<AstValue, Error> {
        match (op, left, right) {
            (BinOpKind::Add, AstValue::Int(l), AstValue::Int(r)) => Ok(AstValue::Int(ValueInt::new(l.get() + r.get()))),
            (BinOpKind::Add, AstValue::String(l), AstValue::String(r)) => {
                let mut s = l.get().clone();
                s.push_str(&r.get());
                Ok(AstValue::String(ValueString::new(s)))
            }
            (BinOpKind::Eq, l, r) => Ok(AstValue::Bool(ValueBool::new(l == r))),
            (BinOpKind::Lt, AstValue::Int(l), AstValue::Int(r)) => Ok(AstValue::Bool(ValueBool::new(l.get() < r.get()))),
            // ... more operations
            _ => Err(Error::Generic("Invalid binary operation".to_string()))
        }
    }
}
## Phase 3: Metaprogramming Intrinsics

### Step 5: Core Intrinsic Functions

```rust
impl ConstInterpreter {
    fn evaluate_intrinsic(&mut self, name: &str, args: &[AstExpr], span: Span)
        -> Result<AstValue, Error> {
        match name {
            // Type introspection
            "@sizeof" => {
                let type_arg = self.evaluate_expression(&args[0])?;
                let type_id = type_arg.as_type()?;
                let size = self.type_registry.get_size(type_id)?;
                Ok(AstValue::Int(ValueInt::new(size as i64)))
            }

            "@reflect_fields" => {
                let type_arg = self.evaluate_expression(&args[0])?;
                let type_id = type_arg.as_type()?;
                let fields = self.type_registry.get_fields(type_id)?;

                let field_values: Vec<AstValue> = fields.into_iter()
                    .map(|field| AstValue::FieldDescriptor {
                        name: field.name,
                        type_id: field.type_id,
                        attributes: field.attributes,
                    })
                    .collect();

                Ok(AstValue::List(ValueList::new(field_values)))
            }

            "@hasmethod" => {
                let type_arg = self.evaluate_expression(&args[0])?;
                let method_name = self.evaluate_expression(&args[1])?.as_string()?;
                let type_id = type_arg.as_type()?;

                let has_method = self.type_registry.type_has_method(type_id, &method_name);
                Ok(AstValue::Bool(ValueBool::new(has_method)))
            }

            // Code generation
            "@addfield" => {
                let field_name = self.evaluate_expression(&args[0])?.as_string()?;
                let field_type = self.evaluate_expression(&args[1])?.as_type()?;

                // Record side effect for code generation
                self.current_context_mut().side_effects.push(
                    SideEffect::GenerateField {
                        name: field_name,
                        type_id: field_type,
                    }
                );

                Ok(AstValue::Unit(ValueUnit))
            }

            "@generate_method" => {
                let method_name = self.evaluate_expression(&args[0])?.as_string()?;
                let method_body = self.evaluate_expression(&args[1])?.as_string()?;

                // Parse method body and create AST
                let body_ast = self.parse_method_body(&method_body, span)?;

                self.current_context_mut().side_effects.push(
                    SideEffect::GenerateMethod {
                        name: method_name,
                        body: body_ast,
                    }
                );

                Ok(AstValue::Unit(ValueUnit))
            }

            _ => Err(Error::Generic(format!("Unknown intrinsic: {}", name)))
        }
    }
}
### Step 6: Code Generation Integration

```rust
struct CodeGenerator {
    side_effects: Vec<SideEffect>,
}
impl CodeGenerator {
    fn apply_side_effects(&mut self, target_node: &mut AstNode, effects: Vec<SideEffect>)
        -> Result<(), Error> {
        for effect in effects {
            match effect {
                SideEffect::GenerateField { name, type_id } => {
                    if let AstNode::Item(AstItem::DefStruct(ref mut struct_item)) = target_node {
                        // Add field to struct definition
                        // This would modify the struct's fields
                    }
                }

                SideEffect::GenerateMethod { name, body } => {
                    if let AstNode::Item(AstItem::DefStruct(ref mut struct_item)) = target_node {
                        // Add method to struct
                    }
                }

                SideEffect::GenerateImpl { trait_name, methods } => {
                    // Generate impl block
                    let impl_item = ItemImpl {
                        trait_ty: Some(trait_name.into()),
                        self_ty: target_node.get_type_expr(),
                        items: methods.into_iter().map(|m| AstItem::DefFunction(m)).collect(),
                    };

                    // Add to parent scope
                    self.pending_impls.push(AstItem::Impl(impl_item));
                }
            }
        }
        Ok(())
    }
}
## Phase 4: Integration with Compiler Pipeline

### Step 7: Compiler Integration

```rust
struct FerroPhaseCompiler {
    parser: Parser,
    const_evaluator: ConstEvaluator,
    code_generators: HashMap<Target, Box<dyn CodeGenerator>>,
}
impl FerroPhaseCompiler {
    pub fn compile(&mut self, source: &str, target: Target) -> Result<String, Error> {
        // 1. Parse to AST
        let mut ast = self.parser.parse(source)?;

        // 2. Enrich AST with const evaluation
        self.const_evaluator.evaluate_all(&mut ast)?;

        // 3. Generate target code
        let generator = self.code_generators.get_mut(&target).unwrap();
        generator.generate(&ast)
    }
}
struct ConstEvaluator {
    interpreter: ConstInterpreter,
    code_generator: CodeGenerator,
}
impl ConstEvaluator {
    fn evaluate_all(&mut self, ast: &mut AstFile) -> Result<(), Error> {
        for item in &mut ast.items {
            self.evaluate_item(item)?;
        }
        Ok(())
    }

    fn evaluate_item(&mut self, item: &mut AstItem) -> Result<(), Error> {
        match item {
            AstItem::DefFunction(func) => {
                self.evaluate_function_body(&mut func.body)?;
            }

            AstItem::DefStruct(struct_item) => {
                // Evaluate const blocks in struct definition
                self.evaluate_struct_const_blocks(struct_item)?;
            }

            AstItem::DefType(type_item) => {
                // Generate concrete instances based on usage
                self.instantiate_type_function(type_item)?;
            }
            _ => {}
        }
        Ok(())
    }
}
## Phase 5: Performance & Optimization

### Step 8: Caching System

```rust
struct ConstEvaluationCache {
    // Expression results cache
    expression_cache: HashMap<ExprId, CachedResult>,

    // Function specialization cache
    function_specialization_cache: HashMap<(FunctionId, Vec<AstValue>), ValueFunction>,

    // Type introspection cache
    type_info_cache: HashMap<TypeId, TypeInfo>,
}
#[derive(Clone)]
struct CachedResult {
    value: AstValue,
    dependencies: HashSet<String>,  // Variable names this result depends on
    is_pure: bool,                  // Can be cached indefinitely
}
impl ConstEvaluationCache {
    fn get_expression_result(&self, node_id: ExprId, current_scope: &Scope) -> Option<AstValue> {
        let cached = self.expression_cache.get(&node_id)?;

        // Check if dependencies are still valid
        for dep in &cached.dependencies {
            if current_scope.get_variable_version(dep) != cached.dependency_versions.get(dep)? {
                return None; // Dependency changed, cache invalid
            }
        }

        Some(cached.value.clone())
    }
}
### Step 9: Error Handling & Diagnostics

```rust
// Const evaluation errors are handled using the existing Error type from fp-core
// Specific const evaluation errors can be added as variants to the Error enum
// or handled as Generic errors with descriptive messages
```

## Implementation Benefits

This refined architecture provides:

- ✅ **Type system integration** through dedicated setup and update passes
- ✅ **Feedback loop handling** via iterative passes 4-6
- ✅ **Incremental processing** with bailout conditions
- ✅ **Clear separation** between evaluation, integration, and cleanup
- ✅ **Extensibility** - easy to add new intrinsics or type operations
- ✅ **Debuggability** - each pass has clear inputs/outputs and responsibilities

## Implementation Timeline

### Sprint 1: Foundation (Passes 1-3)
- Type system integration setup and bidirectional communication
- Const discovery with dependency graph construction
- Generic context preparation for specialization
- **Milestone:** Complete type system integration infrastructure

### Sprint 2: Iterative Core (Passes 4-6)
- Const evaluation with type queries and intrinsic execution
- Type system updates and validation feedback loops
- Dependency re-analysis with incremental updates
- **Milestone:** Working iterative const evaluation with type feedback

### Sprint 3: Specialization (Passes 7-8)
- Generic specialization based on const results
- Code generation and AST modification with side effects
- Source location tracking for generated code
- **Milestone:** Full metaprogramming capabilities

### Sprint 4: Finalization (Passes 9-10)
- Final type validation and integration
- Const cleanup and optimization
- Performance tuning and edge case handling
- **Milestone:** Production-ready const evaluation system

## Success Metrics

- **Functionality:** Can evaluate complex const expressions with metaprogramming
- **Type Integration:** Seamless bidirectional communication with type system
- **Performance:** Iterative evaluation converges efficiently
- **Extensibility:** Easy to add new passes, intrinsics, and type operations
- **Reliability:** Handles circular dependencies, type conflicts, and edge cases
- **Debuggability:** Clear pass-by-pass execution with detailed error reporting

## Key Architectural Insight

**Const evaluation isn't a single pass** - it's a **controlled feedback loop** between const evaluation and type system that eventually converges to a stable state. The key insight: **passes transform, queries analyze**, and the **iterative loop** handles the complex interactions between compile-time evaluation and type system evolution.

This design provides the foundation for sophisticated metaprogramming capabilities while maintaining clear architectural boundaries and predictable execution semantics.

