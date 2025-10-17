//! Integration tests for fp-clang

use fp_clang::{
    ast::{Declaration, Type},
    ClangCodegen, ClangParser, CompileOptions, Standard,
};
use std::fs;
use tempfile::TempDir;

#[test]
fn test_parse_simple_c_file() {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("test.c");

    let c_code = r#"
int add(int a, int b) {
    return a + b;
}
"#;

    fs::write(&c_file, c_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let options = CompileOptions::default();

    let module = parser.parse_to_llvm_ir(&c_file, &options).unwrap();

    // Verify we got a module
    assert_eq!(module.functions.len(), 1);
    assert_eq!(module.functions[0].name, "add");
    assert_eq!(module.functions[0].parameters.len(), 2);
}

#[test]
fn test_parse_translation_unit_basic() {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("ast_sample.c");

    let c_code = r#"
typedef int MyInt;

struct Point {
    int x;
    int y;
};

enum Color {
    Red = 1,
    Green,
    Blue,
};

int add(int a, int b) {
    return a + b;
}
"#;

    fs::write(&c_file, c_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let options = CompileOptions::default();
    let translation_unit = parser.parse_translation_unit(&c_file, &options).unwrap();

    assert_eq!(translation_unit.file_path, c_file.to_string_lossy());

    let mut saw_function = false;
    let mut saw_struct = false;
    let mut saw_enum = false;
    let mut saw_typedef = false;

    for decl in &translation_unit.declarations {
        match decl {
            Declaration::Function(func) if func.name == "add" => {
                saw_function = true;
                assert_eq!(func.parameters.len(), 2);
                assert!(func.is_definition);
            }
            Declaration::Struct(strukt) if strukt.name.as_deref() == Some("Point") => {
                saw_struct = true;
                assert_eq!(strukt.fields.len(), 2);
            }
            Declaration::Enum(enm) if enm.name.as_deref() == Some("Color") => {
                saw_enum = true;
                assert_eq!(enm.enumerators.len(), 3);
            }
            Declaration::Typedef(td) if td.name == "MyInt" => {
                saw_typedef = true;
            }
            _ => {}
        }
    }

    assert!(saw_function);
    assert!(saw_struct);
    assert!(saw_enum);
    assert!(saw_typedef);
}

#[test]
fn test_parse_translation_unit_references() {
    let temp_dir = TempDir::new().unwrap();
    let cpp_file = temp_dir.path().join("refs.cpp");

    let cpp_code = r#"
int& identity_ref(int& value) {
    return value;
}

const int& identity_cref(const int& value) {
    return value;
}

int&& forward_ref(int&& value) {
    return static_cast<int&&>(value);
}
"#;

    fs::write(&cpp_file, cpp_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let mut options = CompileOptions::default();
    options.standard = Some(Standard::Cxx11);

    let translation_unit = parser.parse_translation_unit(&cpp_file, &options).unwrap();

    let identity_ref = find_function(&translation_unit, "identity_ref");
    match &identity_ref.return_type {
        Type::Reference { base, is_rvalue } => {
            assert!(!is_rvalue);
            assert!(matches!(base.as_ref(), Type::Int));
        }
        other => panic!("unexpected return type: {:?}", other),
    }

    match &identity_ref.parameters[0].param_type {
        Type::Reference { base, is_rvalue } => {
            assert!(!is_rvalue);
            assert!(matches!(base.as_ref(), Type::Int));
        }
        other => panic!("unexpected parameter type: {:?}", other),
    }

    let identity_cref = find_function(&translation_unit, "identity_cref");
    match &identity_cref.parameters[0].param_type {
        Type::Reference { base, is_rvalue } => {
            assert!(!is_rvalue);
            match base.as_ref() {
                Type::Qualified { base, is_const, .. } => {
                    assert!(*is_const);
                    assert!(matches!(base.as_ref(), Type::Int));
                }
                other => panic!("unexpected base type: {:?}", other),
            }
        }
        other => panic!("unexpected parameter type: {:?}", other),
    }

    let forward_ref = find_function(&translation_unit, "forward_ref");
    match &forward_ref.return_type {
        Type::Reference { base, is_rvalue } => {
            assert!(*is_rvalue);
            assert!(matches!(base.as_ref(), Type::Int));
        }
        other => panic!("unexpected return type: {:?}", other),
    }

    match &forward_ref.parameters[0].param_type {
        Type::Reference { base, is_rvalue } => {
            assert!(*is_rvalue);
            assert!(matches!(base.as_ref(), Type::Int));
        }
        other => panic!("unexpected parameter type: {:?}", other),
    }
}

fn find_function<'a>(
    translation_unit: &'a fp_clang::ast::TranslationUnit,
    name: &str,
) -> &'a fp_clang::ast::FunctionDecl {
    translation_unit
        .declarations
        .iter()
        .find_map(|decl| match decl {
            Declaration::Function(func) if func.name == name => Some(func),
            _ => None,
        })
        .unwrap_or_else(|| panic!("function {} not found", name))
}

#[test]
fn test_parse_function_pointer_types() {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("func_ptr.c");

    let c_code = r#"
int invoke(int (*fn)(int, int), int a, int b) {
    return fn(a, b);
}

int add(int x, int y) {
    return x + y;
}
"#;

    fs::write(&c_file, c_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let options = CompileOptions::default();

    let translation_unit = parser.parse_translation_unit(&c_file, &options).unwrap();

    let invoke = find_function(&translation_unit, "invoke");
    let ptr_param = &invoke.parameters[0].param_type;
    match ptr_param {
        Type::Pointer(inner) => match inner.as_ref() {
            Type::Function {
                return_type,
                params,
                is_variadic,
            } => {
                assert!(matches!(return_type.as_ref(), Type::Int));
                assert_eq!(params.len(), 2);
                assert!(params.iter().all(|p| matches!(p, Type::Int)));
                assert!(!is_variadic);
            }
            other => panic!("unexpected pointer target: {:?}", other),
        },
        other => panic!("expected pointer to function, got {:?}", other),
    }
}

#[test]
fn test_example_parse_c_file_shapes() {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("example.c");

    let c_code = r#"
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

int main() {
    int result = add(5, 3);
    printf("Result: %d\n", result);
    return 0;
}
"#;

    fs::write(&c_file, c_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let mut options = CompileOptions::default();
    options.standard = Some(Standard::C11);
    options.optimization = Some("0".to_string());
    options.debug = true;

    let module = parser.parse_to_llvm_ir(&c_file, &options).unwrap();

    let names: Vec<_> = module.functions.iter().map(|f| f.name.clone()).collect();
    assert!(names.contains(&"add".to_string()));
    assert!(names.iter().any(|name| name.contains("main")));
    assert!(!module.global_vars.is_empty());

    let ir_text = parser.compile_to_ir_text(&c_file, &options).unwrap();
    assert!(ir_text.contains("define i32 @add"));
    assert!(ir_text.contains("printf"));
}

#[test]
fn test_compile_with_standard() {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("test_std.c");

    let c_code = r#"
#include <stdbool.h>

bool is_positive(int n) {
    return n > 0;
}
"#;

    fs::write(&c_file, c_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let mut options = CompileOptions::default();
    options.standard = Some(Standard::C99);

    let module = parser.parse_to_llvm_ir(&c_file, &options).unwrap();

    // Verify we got a module with the function
    assert!(!module.functions.is_empty());
}

#[test]
fn test_compile_to_ir_text() {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("test_text.c");

    let c_code = r#"
int multiply(int a, int b) {
    return a * b;
}
"#;

    fs::write(&c_file, c_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let options = CompileOptions::default();

    let ir_text = parser.compile_to_ir_text(&c_file, &options).unwrap();

    // Verify IR contains expected elements
    assert!(ir_text.contains("define"));
    assert!(ir_text.contains("multiply"));
    assert!(ir_text.contains("ret"));
}

#[test]
fn test_compile_with_optimization() {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("test_opt.c");

    let c_code = r#"
int square(int x) {
    return x * x;
}
"#;

    fs::write(&c_file, c_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let mut options = CompileOptions::default();
    options.optimization = Some("2".to_string());

    let module = parser.parse_to_llvm_ir(&c_file, &options).unwrap();

    // Verify we got a module
    assert!(!module.functions.is_empty());
    assert_eq!(module.functions[0].name, "square");
}

#[test]
fn test_example_extract_headers_declarations() {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("mylib.c");

    let c_code = r#"
#include <stdarg.h>

int add(int a, int b) {
    return a + b;
}

int subtract(int a, int b) {
    return a - b;
}

double multiply(double a, double b) {
    return a * b;
}

void log_message(const char* format, ...) {
}

typedef struct {
    int x;
    int y;
} Point;

Point create_point(int x, int y) {
    Point p;
    p.x = x;
    p.y = y;
    return p;
}
"#;

    fs::write(&c_file, c_code).unwrap();

    let codegen = ClangCodegen::new().unwrap();
    let options = CompileOptions::default();

    let signatures = codegen.extract_declarations(&c_file, &options).unwrap();
    assert_eq!(signatures.len(), 5);

    let names: Vec<_> = signatures.iter().map(|sig| sig.name.clone()).collect();
    assert!(names.contains(&"add".to_string()));
    assert!(names.contains(&"subtract".to_string()));
    assert!(names.contains(&"multiply".to_string()));
    assert!(names.iter().any(|name| name == "log_message"));
    assert!(signatures.iter().any(|sig| sig.is_variadic));
}

#[test]
fn test_variadic_function() {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("test_variadic.c");

    let c_code = r#"
#include <stdarg.h>

int sum(int count, ...) {
    va_list args;
    va_start(args, count);
    int total = 0;
    for (int i = 0; i < count; i++) {
        total += va_arg(args, int);
    }
    va_end(args);
    return total;
}
"#;

    fs::write(&c_file, c_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let options = CompileOptions::default();

    let module = parser.parse_to_llvm_ir(&c_file, &options).unwrap();

    // Verify variadic function
    assert_eq!(module.functions.len(), 1);
    assert!(module.functions[0].is_var_arg);
}

#[test]
fn test_cpp_compilation() {
    let temp_dir = TempDir::new().unwrap();
    let cpp_file = temp_dir.path().join("test.cpp");

    let cpp_code = r#"
class Calculator {
public:
    int add(int a, int b) {
        return a + b;
    }
};

extern "C" int calculate(int a, int b) {
    Calculator calc;
    return calc.add(a, b);
}
"#;

    fs::write(&cpp_file, cpp_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let mut options = CompileOptions::default();
    options.standard = Some(Standard::Cxx11);

    let module = parser.parse_to_llvm_ir(&cpp_file, &options).unwrap();

    // Verify we got functions (C++ mangles names)
    assert!(!module.functions.is_empty());
}

#[test]
fn test_clang_version() {
    let parser = ClangParser::new().unwrap();
    let version = parser.version().unwrap();

    // Just verify we got some version string
    assert!(version.contains("clang") || version.contains("LLVM"));
}

#[test]
fn test_invalid_file() {
    let parser = ClangParser::new().unwrap();
    let options = CompileOptions::default();

    let result = parser.parse_to_llvm_ir(std::path::Path::new("nonexistent.c"), &options);
    assert!(result.is_err());
}

#[test]
fn test_compilation_error() {
    let temp_dir = TempDir::new().unwrap();
    let c_file = temp_dir.path().join("error.c");

    // Invalid C code
    let c_code = r#"
int broken() {
    return undefined_variable;
}
"#;

    fs::write(&c_file, c_code).unwrap();

    let parser = ClangParser::new().unwrap();
    let options = CompileOptions::default();

    let result = parser.parse_to_llvm_ir(&c_file, &options);
    assert!(result.is_err());
}
