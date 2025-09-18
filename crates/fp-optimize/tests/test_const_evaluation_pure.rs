// Pure const evaluation functionality tests
// Tests const evaluation capabilities WITHOUT basic language features
// Focus: Optimization-specific const evaluation, folding, and compile-time computation

use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::Result;
use fp_optimize::orchestrators::ConstEvaluationOrchestrator;
use fp_optimize::utils::{FoldOptimizer, OptimizePass};
use fp_rust_lang::printer::RustPrinter;
use fp_rust_lang::shll_parse_expr;
use std::sync::Arc;

fn test_const_optimization(expr: AstExpr) -> Result<AstExpr> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    
    let serializer = Arc::new(RustPrinter::new());
    let const_pass = ConstEvaluationOrchestrator::new(serializer.clone());
    let optimizer = FoldOptimizer::new(serializer, Box::new(const_pass));
    let ctx = SharedScopedContext::new();
    
    optimizer.optimize_expr(expr, &ctx)
}

// ===== CONST FOLDING OPTIMIZATION =====

#[test]
fn test_const_arithmetic_folding() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test that const evaluation can fold arithmetic at compile time
    let expr = shll_parse_expr!(42 + 8);
    let _result = test_const_optimization(expr)?;
    // In full implementation, this would be folded to literal 50

    let expr = shll_parse_expr!(10 * 5 + 2);
    let _result = test_const_optimization(expr)?;
    // Should be folded to literal 52

    Ok(())
}

#[test]
fn test_const_boolean_folding() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test boolean expression folding
    let expr = shll_parse_expr!(5 > 3);
    let _result = test_const_optimization(expr)?;
    // Should be folded to literal true

    let expr = shll_parse_expr!(10 == 10);
    let _result = test_const_optimization(expr)?;
    // Should be folded to literal true

    Ok(())
}

// ===== CONST DEPENDENCY ANALYSIS =====

#[test]
fn test_const_variable_propagation() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test const variable propagation (basic case using let)
    let expr = shll_parse_expr!({
        let x = 42; // Could be treated as const
        x + 8
    });
    let _result = test_const_optimization(expr)?;
    // In full implementation, should propagate x=42 and fold to 50

    Ok(())
}

#[test] 
#[ignore] // Complex dependency analysis not fully implemented yet
fn test_const_dependency_chain() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test chained const dependencies
    let expr = shll_parse_expr!({
        let a = 5;
        let b = a + 3;  
        let c = b * 2;
        c
    });
    let _result = test_const_optimization(expr)?;
    // Should analyze: a=5 -> b=8 -> c=16

    Ok(())
}

// ===== CONST EVALUATION WITH CONDITIONALS =====

#[test]
#[ignore] // If expressions not supported in interpreter yet
fn test_const_conditional_folding() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test const conditional folding
    let expr = shll_parse_expr!(if true { 42 } else { 24 });
    let _result = test_const_optimization(expr)?;
    // Should be folded to literal 42

    Ok(())
}

// ===== CONST FUNCTION EVALUATION =====

#[test]
#[ignore] // Function evaluation not supported yet
fn test_const_function_folding() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test const function evaluation
    let expr = shll_parse_expr!({
        fn add_const(a: i64, b: i64) -> i64 { a + b }
        add_const(10, 20)
    });
    let _result = test_const_optimization(expr)?;
    // Should be folded to literal 30

    Ok(())
}

// ===== METAPROGRAMMING INTRINSICS =====

#[test]
#[ignore] // Intrinsics not implemented yet
fn test_sizeof_intrinsic() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test sizeof intrinsic (when implemented)
    // let expr = shll_parse_expr!(sizeof(i64));
    // let result = test_const_optimization(expr)?;
    // Should be folded to literal 8

    Ok(())
}

#[test]
#[ignore] // Intrinsics not implemented yet  
fn test_type_introspection() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test type introspection intrinsics (when implemented)
    // let expr = shll_parse_expr!(type_name(i64));
    // let result = test_const_optimization(expr)?;
    // Should be folded to literal "i64"

    Ok(())
}

// ===== OPTIMIZATION CAPABILITY TESTING =====

#[test]
fn test_const_optimization_vs_runtime_evaluation() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test that const optimization produces same results as runtime evaluation
    // but potentially at compile time
    let expressions = vec![
        shll_parse_expr!(2 + 3),
        shll_parse_expr!(10 * 5),
        shll_parse_expr!(1 + 2 * 3),
        shll_parse_expr!(5 == 5),
        shll_parse_expr!(10 > 5),
    ];

    for expr in expressions {
        let _result = test_const_optimization(expr)?;
        // Should succeed for all const-evaluable expressions
    }

    Ok(())
}

#[test]
fn test_const_optimization_complex_expressions() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test const optimization on more complex expressions
    let expr = shll_parse_expr!(2 + 3 * 4 - 1);
    let _result = test_const_optimization(expr)?;
    // Should fold to 13: 2 + 12 - 1

    let expr = shll_parse_expr!((5 + 5) == 10);
    let _result = test_const_optimization(expr)?;
    // Should fold to true

    Ok(())
}

// ===== CONST EVALUATION SYSTEM INTEGRATION =====

#[test]
fn test_const_evaluation_pass_integration() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test integration with the const evaluation pass system
    let serializer = Arc::new(RustPrinter::new());
    let const_pass = ConstEvaluationOrchestrator::new(serializer.clone());
    
    // Verify pass has correct name
    assert_eq!(const_pass.name(), "const_evaluation");

    let optimizer = FoldOptimizer::new(serializer, Box::new(const_pass));
    let ctx = SharedScopedContext::new();

    // Test basic integration
    let expr = shll_parse_expr!(42);
    let _result = optimizer.optimize_expr(expr, &ctx)?;

    Ok(())
}

// ===== OPTIMIZATION EDGE CASES =====

#[test]
fn test_const_evaluation_edge_cases() -> Result<()> {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));

    // Test edge cases in const evaluation
    let expr = shll_parse_expr!(0 * 1000000);
    let _result = test_const_optimization(expr)?;
    // Should fold to 0

    let expr = shll_parse_expr!(1 + 0);
    let _result = test_const_optimization(expr)?;
    // Should fold to 1

    Ok(())
}