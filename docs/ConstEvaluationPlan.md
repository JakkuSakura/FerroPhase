# FerroPhase Const Evaluation Implementation Plan

## Overview

This document outlines the comprehensive plan to implement const evaluation support in FerroPhase, building on the existing interpreter infrastructure with metaprogramming intrinsics and compile-time computation capabilities.

## Current Status Analysis

### ✅ **Existing Strengths**
- **Solid Interpreter Foundation**: Current `InterpreterPass` handles expressions, types, and basic evaluation well
- **Proper Architecture**: Clean separation between `AstValue::Struct(ValueStruct)` and `TypeStruct` definitions  
- **Working Test Suite**: Most tests pass (19/21 in const eval phases, 10/10 in basic evaluation)
- **Type System Integration**: Good type inference and checking via `typing.rs`
- **Context Management**: Effective scoping through `SharedScopedContext`

### 🔧 **Missing for Const Evaluation**
- **Metaprogramming Intrinsics**: No `@sizeof`, `@create_struct`, `@addfield`, etc.
- **Side Effect Tracking**: No code generation capabilities
- **Iterative Evaluation**: No multi-pass const evaluation pipeline
- **Compile-Time Validation**: No `@compile_error` or validation intrinsics

## Implementation Plan

### Phase 1: Core Intrinsic Functions 🎯

**Objective**: Implement basic metaprogramming intrinsics to enable struct creation and introspection.

#### Intrinsic Functions to Implement

| Intrinsic | Purpose | Return Type | Implementation Priority |
|-----------|---------|-------------|------------------------|
| `@sizeof(type)` | Get size of type in bytes | `AstValue::Int` | **High** |
| `@create_struct(name)` | Create new struct builder | `AstValue::Type(AstType::Struct)` | **High** |
| `@addfield(struct, name, type)` | Add field to struct | Modified struct type | **High** |
| `@reflect_fields(type)` | Get field descriptors | `AstValue::List` | **High** |
| `@hasfield(struct, name)` | Check if field exists | `AstValue::Bool` | **Medium** |
| `@field_count(struct)` | Get number of fields | `AstValue::Int` | **Medium** |
| `@field_type(struct, name)` | Get field type | `AstValue::Type` | **Medium** |
| `@struct_size(struct)` | Get struct size | `AstValue::Int` | **Medium** |
| `@clone_struct(type)` | Clone struct definition | `AstValue::Type(AstType::Struct)` | **Low** |

#### Implementation Location

Update `fp-core/src/ops/builtins.rs` to add new intrinsic functions:

```rust
// Add to builtin function definitions
pub fn builtin_create_struct() -> BuiltinFn { /* ... */ }
pub fn builtin_addfield() -> BuiltinFn { /* ... */ }
pub fn builtin_sizeof() -> BuiltinFn { /* ... */ }
// etc.
```

#### Integration Points

Update `fp-optimize/src/pass/interpret/mod.rs` to register new intrinsics:

```rust
// In interpret_ident function
"@create_struct" if resolve => Ok(AstValue::any(builtin_create_struct())),
"@addfield" if resolve => Ok(AstValue::any(builtin_addfield())),
"@sizeof" if resolve => Ok(AstValue::any(builtin_sizeof())),
```

### Phase 2: Side Effect Tracking System 📝

**Objective**: Add side effect tracking for code generation and struct modification.

#### Side Effect Types

```rust
#[derive(Debug, Clone)]
pub enum SideEffect {
    /// Generate a new struct field
    GenerateField { 
        struct_name: String, 
        field_name: String, 
        field_type: AstType 
    },
    
    /// Generate a method for a struct
    GenerateMethod { 
        struct_name: String, 
        method_name: String, 
        method_body: AstExpr 
    },
    
    /// Generate an impl block
    GenerateImpl { 
        struct_name: String, 
        trait_name: Option<String>, 
        methods: Vec<MethodDef> 
    },
    
    /// Generate derive attribute
    GenerateDerive { 
        struct_name: String, 
        derive_trait: String 
    },
    
    /// Compile-time error
    CompileError { 
        message: String, 
        span: Option<Span> 
    },
    
    /// Compile-time warning  
    CompileWarning { 
        message: String, 
        span: Option<Span> 
    },
}
```

#### Integration with Interpreter

```rust
pub struct InterpreterContext {
    /// Accumulated side effects
    pub side_effects: Vec<SideEffect>,
    
    /// Modified types during evaluation
    pub modified_types: HashMap<String, TypeStruct>,
    
    /// Generated code fragments
    pub generated_code: Vec<AstItem>,
}
```

### Phase 3: Iterative Evaluation Passes 🔄

**Objective**: Implement multi-pass evaluation system for complex const expressions.

#### Pass Architecture

```rust
pub struct ConstEvaluationPass {
    max_iterations: usize,
    current_iteration: usize,
    changed: bool,
    
    /// Dependencies between const blocks
    dependencies: HashMap<u64, HashSet<u64>>,
    
    /// Evaluation results cache
    cache: HashMap<u64, AstValue>,
}
```

#### Evaluation Loop

```rust
impl ConstEvaluationPass {
    pub fn evaluate_iteratively(&mut self, 
        exprs: Vec<AstExpr>, 
        ctx: &SharedScopedContext
    ) -> Result<Vec<AstValue>> {
        
        for iteration in 0..self.max_iterations {
            self.current_iteration = iteration;
            self.changed = false;
            
            // Evaluate all expressions
            for expr in &exprs {
                if self.try_evaluate_expr(expr, ctx)? {
                    self.changed = true;
                }
            }
            
            // If no changes, we're done
            if !self.changed {
                break;
            }
        }
        
        Ok(self.get_final_results())
    }
}
```

### Phase 4: Compile-Time Validation 🛡️

**Objective**: Add compile-time error checking and validation intrinsics.

#### Validation Intrinsics

```rust
// Add to builtin functions
pub fn builtin_compile_error() -> BuiltinFn {
    BuiltinFn::new_with_ident("@compile_error".into(), |args, _ctx| {
        if let Some(AstValue::String(msg)) = args.get(0) {
            return Err(CompileTimeError::UserError(msg.value.clone()));
        }
        Err(CompileTimeError::InvalidArguments)
    })
}

pub fn builtin_compile_warning() -> BuiltinFn {
    BuiltinFn::new_with_ident("@compile_warning".into(), |args, ctx| {
        if let Some(AstValue::String(msg)) = args.get(0) {
            ctx.emit_warning(msg.value.clone());
        }
        Ok(AstValue::unit())
    })
}
```

#### Integration with Type System

```rust
impl TypeStruct {
    /// Validate struct at compile time
    pub fn validate_at_compile_time(&self) -> Result<()> {
        // Check field count limits
        if self.fields.len() > MAX_FIELDS {
            return Err(CompileTimeError::TooManyFields(self.fields.len()));
        }
        
        // Check size constraints
        let size = self.calculate_size()?;
        if size > MAX_STRUCT_SIZE {
            return Err(CompileTimeError::StructTooLarge(size));
        }
        
        // Check field name conflicts
        let mut seen_names = HashSet::new();
        for field in &self.fields {
            if !seen_names.insert(&field.name) {
                return Err(CompileTimeError::DuplicateField(field.name.clone()));
            }
        }
        
        Ok(())
    }
}
```

## Implementation Steps

### Step 1: Basic Intrinsic Implementation (Week 1-2)

1. **Add core intrinsic functions** to `builtins.rs`
   - `@sizeof`, `@create_struct`, `@addfield`
   - `@reflect_fields`, `@hasfield`, `@field_count`

2. **Update interpreter** to register new intrinsics

3. **Test basic functionality** with existing test suite

4. **Fix failing tests** in `test_const_eval_phases.rs`

### Step 2: Side Effect System (Week 3)

1. **Implement SideEffect enum** and tracking system

2. **Update intrinsics** to generate side effects

3. **Add code generation** from side effects to AST

4. **Test struct creation and modification**

### Step 3: Iterative Evaluation (Week 4)

1. **Implement multi-pass evaluation** system

2. **Add dependency tracking** between const blocks

3. **Implement evaluation cache** for performance

4. **Test complex const expressions** with dependencies

### Step 4: Validation and Polish (Week 5)

1. **Add compile-time validation** intrinsics

2. **Implement error reporting** and warnings

3. **Performance optimization** for large const expressions

4. **Documentation and examples** completion

## Testing Strategy

### Unit Tests
- Individual intrinsic function testing
- Side effect generation validation
- Type system integration tests
- Error handling and validation

### Integration Tests  
- Complex const evaluation scenarios
- Multi-pass dependency resolution
- Real-world metaprogramming examples
- Performance benchmarks

### Example Applications
- Configuration-driven struct generation
- Parametric container creation
- Compile-time validation systems
- Cross-platform adaptation examples

## Success Criteria

### Functional Requirements ✅
- [ ] All intrinsic functions implemented and tested
- [ ] Side effect tracking system operational
- [ ] Multi-pass evaluation working correctly
- [ ] Compile-time validation and error reporting
- [ ] Integration with existing interpreter seamless

### Performance Requirements ⚡
- [ ] Const evaluation performance acceptable (<100ms for typical expressions)
- [ ] Memory usage reasonable for complex metaprogramming
- [ ] Caching system effective for repeated evaluations

### Quality Requirements 📋
- [ ] All existing tests continue to pass
- [ ] New test coverage >90% for const evaluation features
- [ ] Documentation complete and comprehensive
- [ ] Examples demonstrate real-world usage

## Risk Mitigation

### Technical Risks
- **Complex dependency resolution**: Start with simple cases, build up complexity
- **Performance issues**: Implement caching and optimization early
- **Type system integration**: Extensive testing of type modifications

### Compatibility Risks  
- **Breaking existing functionality**: Maintain backward compatibility
- **Test regression**: Comprehensive test suite validation
- **API stability**: Careful design of intrinsic function interfaces

## Future Enhancements

### Advanced Features
- **Generic const functions** for more sophisticated metaprogramming
- **Const trait evaluation** for type-based specialization  
- **Cross-crate const evaluation** for library integration
- **IDE integration** for const evaluation visualization

### Performance Optimizations
- **Parallel const evaluation** for independent expressions
- **Incremental compilation** with const expression caching
- **LLVM integration** for compile-time optimization

## Conclusion

This implementation plan provides a structured approach to adding comprehensive const evaluation support to FerroPhase. By building on the existing interpreter foundation and adding metaprogramming intrinsics, side effect tracking, and multi-pass evaluation, we can achieve powerful compile-time computation capabilities while maintaining system stability and performance.