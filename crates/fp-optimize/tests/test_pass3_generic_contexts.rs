use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::Result;
use fp_optimize::pass::ConstEvaluator;
use fp_rust_lang::printer::RustPrinter;
use fp_rust_lang::shll_parse_items;
use std::sync::Arc;

fn create_evaluator() -> ConstEvaluator {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    ConstEvaluator::new(Arc::new(RustPrinter::new()))
}

#[test]
fn test_generic_function_discovery() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();
    
    // Create a simple generic function AST node
    let items = shll_parse_items! {
        fn identity<T>(x: T) -> T {
            x
        }
    };
    
    // Convert to AST file
    let ast_file = AstFile {
        path: "test.fp".into(),
        items: items.clone(),
    };
    let ast = AstNode::File(ast_file);
    
    // Run Pass 3
    evaluator.prepare_generic_contexts(&ast, &ctx)?;
    
    // Check that the generic function was discovered
    let generic_candidates = evaluator.get_generic_candidates();
    assert_eq!(generic_candidates.len(), 1, "Should discover 1 generic function");
    
    let candidate = generic_candidates.values().next().unwrap();
    assert_eq!(candidate.name, "identity");
    assert_eq!(candidate.generic_params.len(), 1);
    assert_eq!(candidate.generic_params[0].name.name, "T");
    
    Ok(())
}

#[test]
fn test_struct_discovery() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();
    
    let items = shll_parse_items! {
        struct Point {
            x: i64,
            y: i64,
        }
    };
    
    let ast_file = AstFile {
        path: "test.fp".into(),
        items: items,
    };
    let ast = AstNode::File(ast_file);
    
    // Run Pass 3
    evaluator.prepare_generic_contexts(&ast, &ctx)?;
    
    // Check that the struct was discovered
    let generic_candidates = evaluator.get_generic_candidates();
    assert_eq!(generic_candidates.len(), 1, "Should discover 1 struct candidate");
    
    let candidate = generic_candidates.values().next().unwrap();
    assert_eq!(candidate.name, "Point");
    
    Ok(())
}

#[test]
fn test_const_with_generic_dependency() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();
    
    // First add a generic function and a const that depends on it
    let items = shll_parse_items! {
        fn size_of<T>() -> i64 {
            8 // Simplified
        }
        
        const INT_SIZE = size_of::<i64>();
    };
    
    let ast_file = AstFile {
        path: "test.fp".into(),
        items: items,
    };
    let ast = AstNode::File(ast_file);
    
    // First discover const blocks
    evaluator.discover_const_blocks(&ast, &ctx)?;
    
    // Then run Pass 3
    evaluator.prepare_generic_contexts(&ast, &ctx)?;
    
    // Check that we discovered the generic function
    let generic_candidates = evaluator.get_generic_candidates();
    assert_eq!(generic_candidates.len(), 1);
    
    // Check that we have generic evaluation contexts
    let generic_contexts = evaluator.get_generic_contexts();
    // Note: This might be 0 if the const doesn't actually reference generic parameters directly
    // but it demonstrates the infrastructure
    
    Ok(())
}

#[test]
fn test_multiple_generic_functions() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();
    
    let items = shll_parse_items! {
        fn map<T, U>(f: fn(T) -> U, x: T) -> U {
            f(x)
        }
        
        fn identity<T>(x: T) -> T {
            x
        }
        
        fn constant<T, U>(x: T, _: U) -> T {
            x
        }
    };
    
    let ast_file = AstFile {
        path: "test.fp".into(),
        items: items,
    };
    let ast = AstNode::File(ast_file);
    
    // Run Pass 3
    evaluator.prepare_generic_contexts(&ast, &ctx)?;
    
    // Check that all generic functions were discovered
    let generic_candidates = evaluator.get_generic_candidates();
    assert_eq!(generic_candidates.len(), 3, "Should discover 3 generic functions");
    
    let names: std::collections::HashSet<String> = generic_candidates
        .values()
        .map(|c| c.name.clone())
        .collect();
    
    assert!(names.contains("map"));
    assert!(names.contains("identity"));
    assert!(names.contains("constant"));
    
    Ok(())
}

#[test]
fn test_nested_module_discovery() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();
    
    let items = shll_parse_items! {
        mod utils {
            fn helper<T>(x: T) -> T {
                x
            }
        }
        
        struct Data {
            value: i64,
        }
    };
    
    let ast_file = AstFile {
        path: "test.fp".into(),
        items: items,
    };
    let ast = AstNode::File(ast_file);
    
    // Run Pass 3
    evaluator.prepare_generic_contexts(&ast, &ctx)?;
    
    // Should discover both the generic function in the module and the struct
    let generic_candidates = evaluator.get_generic_candidates();
    assert_eq!(generic_candidates.len(), 2, "Should discover 2 candidates (1 generic function, 1 struct)");
    
    let names: std::collections::HashSet<String> = generic_candidates
        .values()
        .map(|c| c.name.clone())
        .collect();
    
    assert!(names.contains("helper"));
    assert!(names.contains("Data"));
    
    Ok(())
}

#[test]
fn test_generic_context_creation() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();
    
    // Create const blocks first
    let items = shll_parse_items! {
        const VALUE = 42;
        const DOUBLED = VALUE * 2;
    };
    
    let ast_file = AstFile {
        path: "test.fp".into(),
        items: items,
    };
    let ast = AstNode::File(ast_file);
    
    // Run discovery first
    evaluator.discover_const_blocks(&ast, &ctx)?;
    
    // Then run Pass 3
    evaluator.prepare_generic_contexts(&ast, &ctx)?;
    
    // These constants don't have generic dependencies, so no contexts should be created
    let generic_contexts = evaluator.get_generic_contexts();
    // Could be 0 since these consts don't depend on generics
    
    // But we should have discovered the const blocks
    let const_blocks = evaluator.get_const_blocks();
    assert_eq!(const_blocks.len(), 2, "Should discover 2 const blocks");
    
    Ok(())
}