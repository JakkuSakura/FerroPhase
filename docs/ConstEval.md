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
/// AstValue represents runtime values and type references during const evaluation.
/// 
/// IMPORTANT ARCHITECTURE PRINCIPLE:
/// AstValue should contain references to other structs (via Type(AstType::Struct)),
/// not define struct fields directly within AstValue variants.
/// 
/// - Struct instances: AstValue::Struct(ValueStruct) contains TypeStruct + field values
/// - Struct types: AstValue::Type(AstType::Struct(TypeStruct)) references type definitions
/// - This separation maintains clean architecture and enables proper type system integration
#[derive(Debug, Clone, PartialEq)]
enum AstValue {
    // Primitives
    Int(ValueInt),
    Bool(ValueBool),
    Decimal(ValueDecimal),
    String(ValueString),
    Unit(ValueUnit),

    // Composite values (contain references to types, not fields directly)
    List(ValueList),
    Struct(ValueStruct),          // Contains TypeStruct reference + ValueStructural
    Tuple(ValueTuple),

    // Type system integration
    Type(AstType),                // References to types, including AstType::Struct(TypeStruct)
    Expr(Box<AstExpr>),           // Generated code expressions
}
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
## Phase 3.5: Parametric Struct Creation - Structural Generics

### Core Parametric Struct Creation

FerroPhase supports **parametric struct creation** - a powerful form of structural generics where struct layout, fields, and types are determined by compile-time parameters and constants. This enables creating struct variants based on parameters, similar to generics but operating at the structural level.

#### Basic Parametric Struct Creation

```rust
// Parametric struct creation function - like a struct generator
const fn create_vector_struct<const DIM: usize, T>() -> Type {
    let mut vec_struct = @create_struct(format!("Vector{}D<{}>", DIM, T::name()));
    
    // Create fields based on dimension parameter
    match DIM {
        1 => @addfield(vec_struct, "x", T),
        2 => {
            @addfield(vec_struct, "x", T);
            @addfield(vec_struct, "y", T);
        },
        3 => {
            @addfield(vec_struct, "x", T);
            @addfield(vec_struct, "y", T);
            @addfield(vec_struct, "z", T);
        },
        4 => {
            @addfield(vec_struct, "x", T);
            @addfield(vec_struct, "y", T);
            @addfield(vec_struct, "z", T);
            @addfield(vec_struct, "w", T);
        },
        _ => {
            // Dynamic field creation for higher dimensions
            for i in 0..DIM {
                let field_name = format!("dim_{}", i);
                @addfield(vec_struct, field_name, T);
            }
        }
    }
    
    vec_struct
}

// Create specific vector types using parameters
type Vector2D_f32 = create_vector_struct<2, f32>();
type Vector3D_i64 = create_vector_struct<3, i64>();
type Vector8D_f64 = create_vector_struct<8, f64>();

// Generated structs:
// struct Vector2D<f32> { x: f32, y: f32 }
// struct Vector3D<i64> { x: i64, y: i64, z: i64 }
// struct Vector8D<f64> { dim_0: f64, dim_1: f64, ..., dim_7: f64 }
```

#### Parameter-Driven Field Selection

```rust
// Struct creation based on capability parameters
const fn create_config_struct(
    enable_logging: bool,
    enable_metrics: bool, 
    enable_caching: bool,
    log_level: &str
) -> Type {
    let mut config = @create_struct("Config");
    
    // Core fields always present
    @addfield(config, "app_name", String);
    @addfield(config, "port", u16);
    
    // Parameter-driven field inclusion
    if enable_logging {
        @addfield(config, "log_enabled", bool);
        
        // Nested parameter decisions
        match log_level {
            "debug" => {
                @addfield(config, "log_level", LogLevel);
                @addfield(config, "debug_info", String);
                @addfield(config, "stack_trace", bool);
            },
            "info" => {
                @addfield(config, "log_level", LogLevel);
                @addfield(config, "log_format", String);
            },
            _ => {
                @addfield(config, "log_level", LogLevel);
            }
        }
    }
    
    if enable_metrics {
        @addfield(config, "metrics_endpoint", String);
        @addfield(config, "collection_interval", Duration);
    }
    
    if enable_caching {
        @addfield(config, "cache_size", usize);
        @addfield(config, "cache_strategy", CacheStrategy);
    }
    
    config
}

// Usage with different parameter combinations
type DebugConfig = create_config_struct(true, false, true, "debug");
type ProductionConfig = create_config_struct(true, true, true, "info");
type MinimalConfig = create_config_struct(false, false, false, "");
```

#### Type-Based Parametric Creation

```rust
// Create struct based on type characteristics
const fn create_container_struct<T>() -> Type {
    let mut container = @create_struct(format!("Container<{}>", T::name()));
    
    // Core container fields
    @addfield(container, "data", T);
    @addfield(container, "capacity", usize);
    @addfield(container, "len", usize);
    
    // Type-specific optimizations
    if T::is_copy() {
        @addfield(container, "inline_buffer", [T; 8]); // Small buffer optimization
    }
    
    if T::size() <= 8 {
        @addfield(container, "small_size_flag", bool);
    }
    
    if T::needs_drop() {
        @addfield(container, "drop_guard", DropGuard<T>);
    }
    
    // Add methods based on type capabilities
    if T::implements(PartialOrd) {
        @addmethod(container, "sort", |&mut self| { self.data.sort() });
        @addmethod(container, "binary_search", |&self, item: &T| -> Option<usize> {
            self.data.binary_search(item).ok()
        });
    }
    
    if T::implements(Hash) {
        @addmethod(container, "to_hash_set", |&self| -> HashSet<T> {
            self.data.iter().cloned().collect()
        });
    }
    
    container
}

// Generated specialized containers
type IntContainer = create_container_struct<i32>();     // With inline_buffer, small_size_flag
type StringContainer = create_container_struct<String>(); // With drop_guard, no inline_buffer
type HashableContainer = create_container_struct<u64>(); // With sort, binary_search, to_hash_set
```

#### Array-Like Parametric Structs

```rust
// Create array-like structs with compile-time size
const fn create_fixed_array<T, const N: usize>() -> Type {
    let mut array_struct = @create_struct(format!("Array{}x{}", N, T::name()));
    
    // Choose implementation based on size
    if N <= 4 {
        // Small arrays: individual named fields
        let field_names = ["x", "y", "z", "w"];
        for i in 0..N {
            @addfield(array_struct, field_names[i], T);
        }
        
        // Add swizzling methods for small vectors
        if N >= 2 {
            @addmethod(array_struct, "xy", |&self| -> (T, T) { (self.x, self.y) });
        }
        if N >= 3 {
            @addmethod(array_struct, "xyz", |&self| -> (T, T, T) { (self.x, self.y, self.z) });
        }
    } else if N <= 16 {
        // Medium arrays: indexed fields
        for i in 0..N {
            @addfield(array_struct, format!("field_{}", i), T);
        }
    } else {
        // Large arrays: use internal array
        @addfield(array_struct, "data", [T; N]);
        
        // Add efficient indexing
        @addmethod(array_struct, "get", |&self, index: usize| -> Option<&T> {
            self.data.get(index)
        });
    }
    
    // Add common methods regardless of size
    @addmethod(array_struct, "len", |&self| -> usize { N });
    @addmethod(array_struct, "as_slice", |&self| -> &[T] { 
        unsafe { std::slice::from_raw_parts(self as *const _ as *const T, N) }
    });
    
    array_struct
}

// Usage examples
type Vec2 = create_fixed_array<f32, 2>();  // Fields: x, y + xy() method
type Vec3 = create_fixed_array<f64, 3>();  // Fields: x, y, z + xyz() method  
type Matrix4x4 = create_fixed_array<f32, 16>(); // Fields: field_0..field_15
type LargeBuffer = create_fixed_array<u8, 1024>(); // Field: data + get() method
```

#### Conditional Struct Assembly

```rust
// Struct with conditional fields based on compile-time config
const CONFIG_STRUCT: Type = {
    let mut builder = @create_struct("ConfigStruct");
    
    @addfield(builder, "name", String);
    
    if DEBUG_MODE {
        @addfield(builder, "debug_info", String);
        @addfield(builder, "line_number", i64);
    }
    
    if FEATURE_NETWORKING {
        @addfield(builder, "socket", NetworkSocket);
    }
    
    builder
};
```

#### Struct Field Introspection and Modification

```rust
// Reflect on existing struct
const STRUCT_INFO: Vec<FieldDescriptor> = {
    @reflect_fields(Point)
};

// Create modified version of existing struct
const EXTENDED_POINT: Type = {
    let mut builder = @clone_struct(Point);
    @addfield(builder, "z", i64);
    @addfield(builder, "name", String);
    builder
};

// Generate Point3D from Point
```

#### Compile-Time Struct Validation

```rust
// Validate struct at compile time
const VALIDATED_STRUCT: Type = {
    let mut builder = @create_struct("ValidatedStruct");
    
    @addfield(builder, "id", u64);
    @addfield(builder, "data", Vec<u8>);
    
    // Compile-time validation
    if @sizeof(builder) > MAX_STRUCT_SIZE {
        @compile_error("Struct too large!");
    }
    
    if !@hasfield(builder, "id") {
        @compile_error("Missing required 'id' field!");
    }
    
    builder
};
```

### Enhanced Intrinsic Functions for Struct Creation

```rust
impl ConstInterpreter {
    fn evaluate_struct_intrinsic(&mut self, name: &str, args: &[AstExpr], span: Span)
        -> Result<AstValue, Error> {
        match name {
            // Struct creation intrinsics
            "@create_struct" => {
                let struct_name = self.evaluate_expression(&args[0])?.as_string()?;
                
                // Create a new TypeStruct in the type registry
                let type_struct = TypeStruct::new(struct_name.clone(), Vec::new());
                let type_id = self.type_registry.register_type(type_struct.clone())?;
                
                // Return reference to the type, not fields directly in AstValue
                Ok(AstValue::Type(AstType::Struct(type_struct)))
            }

            "@clone_struct" => {
                let source_type = self.evaluate_expression(&args[0])?.as_type()?;
                
                // Clone the TypeStruct definition
                if let AstType::Struct(source_struct) = source_type {
                    let cloned_name = format!("Clone_{}", source_struct.name);
                    let cloned_fields = source_struct.fields.clone();
                    let cloned_struct = TypeStruct::new(cloned_name, cloned_fields);
                    
                    self.type_registry.register_type(cloned_struct.clone())?;
                    Ok(AstValue::Type(AstType::Struct(cloned_struct)))
                } else {
                    Err(Error::Generic("Expected struct type for cloning".to_string()))
                }
            }

            "@addfield" => {
                let type_value = self.evaluate_expression(&args[0])?;
                let field_name = self.evaluate_expression(&args[1])?.as_string()?;
                let field_type = self.evaluate_expression(&args[2])?.as_type()?;

                // Modify the TypeStruct in the type registry, not AstValue directly
                if let AstValue::Type(AstType::Struct(ref mut struct_type)) = type_value {
                    struct_type.add_field(field_name.clone(), field_type);
                    
                    // Record side effect for code generation  
                    self.current_context_mut().side_effects.push(
                        SideEffect::GenerateField {
                            name: field_name,
                            type_id: field_type.id(),
                        }
                    );
                }

                Ok(type_value)
            }

            "@hasfield" => {
                let struct_value = self.evaluate_expression(&args[0])?;
                let field_name = self.evaluate_expression(&args[1])?.as_string()?;

                let has_field = match struct_value {
                    AstValue::Type(AstType::Struct(struct_type)) => {
                        struct_type.has_field(&field_name)
                    }
                    AstValue::Struct(struct_instance) => {
                        struct_instance.ty.has_field(&field_name)
                    }
                    _ => {
                        // For other types, check via type registry
                        if let Ok(type_id) = struct_value.get_type_id() {
                            self.type_registry.type_has_field(type_id, &field_name)
                        } else {
                            false
                        }
                    }
                };

                Ok(AstValue::Bool(ValueBool::new(has_field)))
            }

            "@field_type" => {
                let struct_value = self.evaluate_expression(&args[0])?;
                let field_name = self.evaluate_expression(&args[1])?.as_string()?;

                let field_type = match struct_value {
                    AstValue::Type(AstType::Struct(struct_type)) => {
                        struct_type.get_field_type(&field_name)
                            .ok_or_else(|| Error::Generic(format!("Field '{}' not found", field_name)))?
                    }
                    AstValue::Struct(struct_instance) => {
                        struct_instance.ty.get_field_type(&field_name)
                            .ok_or_else(|| Error::Generic(format!("Field '{}' not found", field_name)))?
                    }
                    _ => {
                        return Err(Error::Generic("Expected struct type or instance".to_string()));
                    }
                };

                Ok(AstValue::Type(field_type))
            }

            "@field_count" => {
                let struct_value = self.evaluate_expression(&args[0])?;
                
                let count = match struct_value {
                    AstValue::Type(AstType::Struct(struct_type)) => struct_type.fields.len(),
                    AstValue::Struct(struct_instance) => struct_instance.structural.fields.len(),
                    _ => {
                        let type_id = struct_value.get_type_id()?;
                        self.type_registry.get_field_count(type_id)?
                    }
                };

                Ok(AstValue::Int(ValueInt::new(count as i64)))
            }

            "@struct_size" => {
                let struct_value = self.evaluate_expression(&args[0])?;
                let size = match struct_value {
                    AstValue::Type(AstType::Struct(struct_type)) => {
                        // Calculate size from TypeStruct definition
                        struct_type.calculate_size(&self.type_registry)?
                    }
                    AstValue::Struct(struct_instance) => {
                        // Get size from the struct's type
                        struct_instance.ty.calculate_size(&self.type_registry)?
                    }
                    _ => {
                        // Fallback to type registry lookup
                        let type_id = struct_value.get_type_id()?;
                        self.type_registry.get_size(type_id)?
                    }
                };

                Ok(AstValue::Int(ValueInt::new(size as i64)))
            }

            _ => Err(Error::Generic(format!("Unknown struct intrinsic: {}", name)))
        }
    }
}
```

### Struct Creation Examples

#### Example 1: Configuration-Driven Struct Generation

```rust
// Compile-time configuration
const ENABLE_LOGGING: bool = true;
const ENABLE_METRICS: bool = false;
const MAX_CONNECTIONS: i64 = 100;

// Generate struct based on configuration
const SERVER_CONFIG: Type = {
    let mut config = @create_struct("ServerConfig");
    
    // Always include basic fields
    @addfield(config, "port", u16);
    @addfield(config, "max_connections", i64);
    
    // Conditional fields based on features
    if ENABLE_LOGGING {
        @addfield(config, "log_level", LogLevel);
        @addfield(config, "log_file", String);
    }
    
    if ENABLE_METRICS {
        @addfield(config, "metrics_endpoint", String);
        @addfield(config, "metrics_interval", Duration);
    }
    
    config
};

// Create instance with computed values
const DEFAULT_SERVER_CONFIG: ServerConfig = {
    let mut config = ServerConfig {
        port: 8080,
        max_connections: MAX_CONNECTIONS,
    };
    
    if ENABLE_LOGGING {
        config.log_level = LogLevel::Info;
        config.log_file = "/var/log/server.log";
    }
    
    config
};
```

#### Example 2: Generic Struct Specialization

```rust
// Generic struct template
struct Container<T> {
    data: T,
    size: usize,
}

// Specialize for specific types at compile time
const STRING_CONTAINER: Type = {
    let mut container = @specialize(Container, String);
    
    // Add specialized methods
    @addmethod(container, "len", || self.data.len());
    @addmethod(container, "is_empty", || self.data.is_empty());
    
    container
};

const INT_CONTAINER: Type = {
    let mut container = @specialize(Container, i64);
    
    // Add numeric-specific methods
    @addmethod(container, "abs", || self.data.abs());
    @addmethod(container, "is_positive", || self.data > 0);
    
    container
};
```

#### Example 3: Struct Field Transformation

```rust
// Transform existing struct
const ENHANCED_POINT: Type = {
    let original_fields = @reflect_fields(Point);
    let mut enhanced = @create_struct("EnhancedPoint");
    
    // Copy all original fields
    for field in original_fields {
        @addfield(enhanced, field.name, field.type_id);
    }
    
    // Add computed fields
    @addfield(enhanced, "magnitude", f64);
    @addfield(enhanced, "angle", f64);
    @addfield(enhanced, "quadrant", i32);
    
    enhanced
};

// Generate implementation
const ENHANCED_POINT_IMPL: Impl = {
    let mut impl_block = @create_impl(EnhancedPoint);
    
    @addmethod(impl_block, "new", |x: i64, y: i64| {
        let magnitude = ((x * x + y * y) as f64).sqrt();
        let angle = (y as f64).atan2(x as f64);
        let quadrant = match (x >= 0, y >= 0) {
            (true, true) => 1,
            (false, true) => 2,
            (false, false) => 3,
            (true, false) => 4,
        };
        
        EnhancedPoint { x, y, magnitude, angle, quadrant }
    });
    
    impl_block
};
```

### Integration with Type System

The struct creation system integrates seamlessly with the type system through the iterative evaluation passes:

1. **Pass 4**: Struct creation intrinsics execute and generate `StructConstructor` values
2. **Pass 5**: Type system receives new struct definitions and validates them
3. **Pass 8**: Code generation applies struct creation side effects to AST
4. **Pass 9**: Final validation ensures all struct references are resolved

This enables sophisticated compile-time struct manipulation while maintaining type safety and performance.

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

