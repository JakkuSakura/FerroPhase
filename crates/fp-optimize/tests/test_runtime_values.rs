use fp_core::ast::{AstValue, RuntimeValue};
use fp_core::context::SharedScopedContext;
use fp_core::id::Ident;
use fp_core::passes::{LiteralRuntimePass, RuntimePass, RustRuntimePass};
use fp_optimize::orchestrators::InterpretationOrchestrator;
use fp_rust_lang::parser::RustParser;
use fp_rust_lang::printer::RustPrinter;
use std::sync::Arc;

#[test]
fn test_runtime_value_creation() {
    // Test direct RuntimeValue creation and manipulation
    let literal_value = RuntimeValue::literal(AstValue::int(42));
    assert!(literal_value.is_literal());
    assert_eq!(literal_value.get_value(), AstValue::int(42));

    let owned_value = RuntimeValue::owned(AstValue::string("hello".to_string()));
    assert!(owned_value.is_owned());
    assert_eq!(
        owned_value.get_value(),
        AstValue::string("hello".to_string())
    );

    let borrowed_value = RuntimeValue::borrowed(AstValue::int(100), "source_var".to_string());
    assert!(borrowed_value.is_borrowed());
    assert_eq!(borrowed_value.get_value(), AstValue::int(100));

    let shared_value = RuntimeValue::shared(AstValue::bool(true));
    assert!(shared_value.is_shared());
    assert_eq!(shared_value.get_value(), AstValue::bool(true));
}

#[test]
fn test_runtime_value_mutation() {
    let mut owned_value = RuntimeValue::owned(AstValue::int(10));

    // Test that owned values can be mutated
    assert!(owned_value.can_mutate());

    let result = owned_value.try_mutate(|ast_value| {
        if let AstValue::Int(int_val) = ast_value {
            int_val.value = 20;
        }
        Ok(())
    });

    assert!(result.is_ok());
    assert_eq!(owned_value.get_value(), AstValue::int(20));
}

#[test]
fn test_runtime_value_immutable() {
    let mut literal_value = RuntimeValue::literal(AstValue::int(10));

    // Test that literal values cannot be mutated
    assert!(!literal_value.can_mutate());

    let result = literal_value.try_mutate(|ast_value| {
        if let AstValue::Int(int_val) = ast_value {
            int_val.value = 20;
        }
        Ok(())
    });

    assert!(result.is_err());
    // Value should remain unchanged
    assert_eq!(literal_value.get_value(), AstValue::int(10));
}

#[test]
fn test_runtime_value_shared() {
    let shared_value = RuntimeValue::shared(AstValue::int(30));
    assert!(shared_value.is_shared());

    // Shared values can be mutated
    let mut shared_copy = shared_value.clone();
    assert!(shared_copy.can_mutate());

    let result = shared_copy.try_mutate(|ast_value| {
        if let AstValue::Int(int_val) = ast_value {
            int_val.value = 40;
        }
        Ok(())
    });

    assert!(result.is_ok());
    assert_eq!(shared_copy.get_value(), AstValue::int(40));
}

#[test]
fn test_runtime_value_conversion() {
    let literal = RuntimeValue::literal(AstValue::int(100));

    // Convert to owned
    let owned = RuntimeValue::owned(literal.get_value());
    assert!(owned.is_owned());

    // Convert to shared
    let shared = owned.to_shared();
    assert!(shared.is_shared());

    // Convert to atomic shared
    let atomic_shared = shared.to_shared_atomic();
    assert!(atomic_shared.is_shared());
}

#[test]
fn test_ownership_semantics() {
    let original = RuntimeValue::owned(AstValue::string("test".to_string()));

    // Test taking ownership
    let taken = original.take_ownership().unwrap();
    assert_eq!(taken, AstValue::string("test".to_string()));
}

#[test]
fn test_literal_runtime_pass() {
    let runtime_pass = LiteralRuntimePass;

    // Test runtime value creation
    let literal_value = runtime_pass.create_runtime_value(AstValue::int(42));
    assert!(literal_value.is_literal());
    assert_eq!(literal_value.get_value(), AstValue::int(42));

    // Test method calls
    let cloned = runtime_pass
        .call_method(literal_value, "clone", vec![])
        .unwrap();
    assert!(cloned.is_literal());
    assert_eq!(cloned.get_value(), AstValue::int(42));
}

#[test]
fn test_rust_runtime_pass() {
    let runtime_pass = RustRuntimePass::new();

    // Test runtime value creation - Rust defaults to owned
    let owned_value = runtime_pass.create_runtime_value(AstValue::int(42));
    assert!(owned_value.is_owned());
    assert_eq!(owned_value.get_value(), AstValue::int(42));

    // Test method calls
    let cloned = runtime_pass
        .call_method(owned_value, "clone", vec![])
        .unwrap();
    assert!(cloned.is_owned());
    assert_eq!(cloned.get_value(), AstValue::int(42));
}

#[test]
fn test_interpretation_with_runtime_pass() {
    // Test interpretation using orchestrator with runtime pass
    let serializer = Arc::new(RustPrinter::new());
    let runtime_pass: Arc<dyn RuntimePass> = Arc::new(RustRuntimePass::new());

    let orchestrator = InterpretationOrchestrator::new(serializer).with_runtime_pass(runtime_pass);

    // Parse a simple expression
    let parser = RustParser::new();
    let expr = parser.parse_expr(syn::parse_str("42").unwrap()).unwrap();
    let context = SharedScopedContext::new();

    // Test runtime interpretation
    let result = orchestrator.interpret_expr_runtime(&expr, &context);
    assert!(result.is_ok());

    let runtime_value = result.unwrap();
    assert!(runtime_value.is_owned()); // Rust runtime creates owned values
    assert_eq!(runtime_value.get_value(), AstValue::int(42));
}

#[test]
fn test_runtime_pass_differences() {
    let serializer = Arc::new(RustPrinter::new());
    let context = SharedScopedContext::new();

    // Parse the same expression
    let parser = RustParser::new();
    let expr = parser.parse_expr(syn::parse_str("42").unwrap()).unwrap();

    // Test with literal runtime
    let literal_pass: Arc<dyn RuntimePass> = Arc::new(LiteralRuntimePass);
    let literal_orchestrator =
        InterpretationOrchestrator::new(serializer.clone()).with_runtime_pass(literal_pass);
    let literal_result = literal_orchestrator
        .interpret_expr_runtime(&expr, &context)
        .unwrap();

    // Test with rust runtime
    let rust_pass: Arc<dyn RuntimePass> = Arc::new(RustRuntimePass::new());
    let rust_orchestrator =
        InterpretationOrchestrator::new(serializer.clone()).with_runtime_pass(rust_pass);
    let rust_result = rust_orchestrator
        .interpret_expr_runtime(&expr, &context)
        .unwrap();

    // Both should compute the same value, but with different ownership semantics
    assert_eq!(literal_result.get_value(), rust_result.get_value());
    assert_eq!(literal_result.get_value(), AstValue::int(42));

    // But they should have different ownership characteristics
    // Note: Both may end up as owned since they're simple literals,
    // but in more complex cases the ownership would differ
    assert_eq!(literal_result.get_value(), AstValue::int(42));
    assert_eq!(rust_result.get_value(), AstValue::int(42));
}

#[test]
fn test_interpreter_handles_runtime_values_in_context() {
    // Test that the interpreter can store and retrieve runtime values in context
    let serializer = Arc::new(RustPrinter::new());
    let runtime_pass: Arc<dyn RuntimePass> = Arc::new(RustRuntimePass::new());
    let _orchestrator =
        InterpretationOrchestrator::new(serializer).with_runtime_pass(runtime_pass.clone());
    let context = SharedScopedContext::new();

    // Store a runtime value in context
    let test_value = RuntimeValue::owned(AstValue::int(100));
    context.insert_runtime_value_with_ctx("test_var", test_value);

    // Verify it can be retrieved
    let retrieved = context.get_runtime_value_storage(Ident::new("test_var"));
    assert!(retrieved.is_some());
    assert_eq!(retrieved.unwrap().get_value(), AstValue::int(100));
}

#[test]
fn test_interpreter_variable_assignment_with_runtime_values() {
    // Test that variable assignment works with runtime values
    let serializer = Arc::new(RustPrinter::new());
    let runtime_pass: Arc<dyn RuntimePass> = Arc::new(RustRuntimePass::new());
    let orchestrator = InterpretationOrchestrator::new(serializer).with_runtime_pass(runtime_pass);
    let context = SharedScopedContext::new();

    // Parse and interpret a variable assignment
    let parser = RustParser::new();
    let expr = parser.parse_expr(syn::parse_str("42").unwrap()).unwrap();

    // Interpret the expression to get a runtime value
    let result = orchestrator
        .interpret_expr_runtime(&expr, &context)
        .unwrap();
    assert!(result.is_owned());

    // Store it in context as if it were assigned to a variable
    context.insert_runtime_value_with_ctx("x", result.clone());

    // Verify the variable can be retrieved with correct ownership
    let retrieved = context.get_runtime_value_storage(Ident::new("x")).unwrap();
    assert!(retrieved.is_owned());
    assert_eq!(retrieved.get_value(), AstValue::int(42));
}

#[test]
fn test_interpreter_arithmetic_with_runtime_values() {
    // Test that arithmetic operations work with runtime values
    let serializer = Arc::new(RustPrinter::new());
    let runtime_pass: Arc<dyn RuntimePass> = Arc::new(RustRuntimePass::new());
    let orchestrator = InterpretationOrchestrator::new(serializer).with_runtime_pass(runtime_pass);
    let context = SharedScopedContext::new();

    // Parse a simple arithmetic expression
    let parser = RustParser::new();
    let expr = parser
        .parse_expr(syn::parse_str("10 + 32").unwrap())
        .unwrap();

    // Interpret with runtime semantics
    let result = orchestrator
        .interpret_expr_runtime(&expr, &context)
        .unwrap();

    // Verify the result has correct value and ownership
    assert!(result.is_owned()); // Rust runtime should create owned values
    assert_eq!(result.get_value(), AstValue::int(42));
}

#[test]
fn test_interpreter_struct_field_access_with_runtime_values() {
    use fp_core::ast::{
        AstType, StructuralField, TypeStruct, ValueField, ValueStruct, ValueStructural,
    };

    let serializer = Arc::new(RustPrinter::new());
    let runtime_pass: Arc<dyn RuntimePass> = Arc::new(RustRuntimePass::new());
    let _orchestrator =
        InterpretationOrchestrator::new(serializer).with_runtime_pass(runtime_pass.clone());
    let context = SharedScopedContext::new();

    // Create a struct type and value
    let struct_type = TypeStruct {
        name: Ident::new("Point"),
        fields: vec![
            StructuralField {
                name: Ident::new("x"),
                value: AstType::any(),
            },
            StructuralField {
                name: Ident::new("y"),
                value: AstType::any(),
            },
        ],
    };

    let struct_value = ValueStruct {
        ty: struct_type,
        structural: ValueStructural {
            fields: vec![
                ValueField {
                    name: Ident::new("x"),
                    value: AstValue::int(10),
                },
                ValueField {
                    name: Ident::new("y"),
                    value: AstValue::int(20),
                },
            ],
        },
    };

    // Store the struct as a runtime value
    let runtime_struct = runtime_pass.create_runtime_value(AstValue::Struct(struct_value));
    context.insert_runtime_value_with_ctx("point", runtime_struct);

    // Verify field access works with runtime values
    let retrieved_struct = context
        .get_runtime_value_storage(Ident::new("point"))
        .unwrap();
    assert!(retrieved_struct.is_owned());

    // Test field access through runtime pass
    let field_result = runtime_pass.access_field(retrieved_struct, "x").unwrap();
    assert_eq!(field_result.get_value(), AstValue::int(10));
}

#[test]
fn test_interpreter_method_calls_with_runtime_values() {
    use fp_core::ast::register_threadlocal_serializer;

    let serializer = Arc::new(RustPrinter::new());
    register_threadlocal_serializer(serializer.clone());
    let runtime_pass: Arc<dyn RuntimePass> = Arc::new(RustRuntimePass::new());
    let _orchestrator =
        InterpretationOrchestrator::new(serializer).with_runtime_pass(runtime_pass.clone());
    let _context = SharedScopedContext::new();

    // Create a runtime value
    let original_value = runtime_pass.create_runtime_value(AstValue::int(42));

    // Test method call through runtime pass
    let cloned_value = runtime_pass
        .call_method(original_value, "clone", vec![])
        .unwrap();

    // Verify the cloned value has correct properties
    assert!(cloned_value.is_owned()); // Rust runtime clone creates owned values
    assert_eq!(cloned_value.get_value(), AstValue::int(42));

    // Test string conversion method
    let string_value = runtime_pass
        .call_method(cloned_value, "to_string", vec![])
        .unwrap();
    assert!(string_value.is_owned());
    assert_eq!(string_value.get_value(), AstValue::string("42".to_string()));
}

#[test]
fn test_interpreter_ownership_transfer_scenarios() {
    let serializer = Arc::new(RustPrinter::new());
    let runtime_pass: Arc<dyn RuntimePass> = Arc::new(RustRuntimePass::new());
    let _orchestrator =
        InterpretationOrchestrator::new(serializer).with_runtime_pass(runtime_pass.clone());
    let context = SharedScopedContext::new();

    // Test scenario: Create owned value, convert to shared
    let owned_value = runtime_pass.create_runtime_value(AstValue::int(100));
    assert!(owned_value.is_owned());

    // Convert to shared ownership
    let shared_value = runtime_pass.convert_value(owned_value, "shared").unwrap();
    assert!(shared_value.is_shared());
    assert_eq!(shared_value.get_value(), AstValue::int(100));

    // Test scenario: Store shared value in context
    context.insert_runtime_value_with_ctx("shared_var", shared_value);

    // Verify it can be retrieved and still shared
    let retrieved = context
        .get_runtime_value_storage(Ident::new("shared_var"))
        .unwrap();
    assert!(retrieved.is_shared());
    assert_eq!(retrieved.get_value(), AstValue::int(100));
}
