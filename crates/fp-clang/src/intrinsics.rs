//! Shared C runtime intrinsic declarations.
//!
//! The clang frontend and LLVM backend both need to refer to the same set of
//! C runtime helper declarations (e.g. `printf`, `puts`, allocation helpers).
//! Keeping the definitions here avoids duplicating the signature catalogue
//! across crates.

use fp_core::lir;
use std::collections::HashMap;
use std::sync::LazyLock;

/// Signature metadata for a C runtime intrinsic.
#[derive(Debug, Clone)]
pub struct IntrinsicSignature {
    pub name: &'static str,
    pub params: Vec<lir::LirType>,
    pub return_type: lir::LirType,
    pub is_variadic: bool,
}

impl IntrinsicSignature {
    fn new(
        name: &'static str,
        params: Vec<lir::LirType>,
        return_type: lir::LirType,
        is_variadic: bool,
    ) -> Self {
        Self {
            name,
            params,
            return_type,
            is_variadic,
        }
    }
}

fn ptr() -> lir::LirType {
    lir::LirType::Ptr(Box::new(lir::LirType::I8))
}

fn i32() -> lir::LirType {
    lir::LirType::I32
}

fn i64() -> lir::LirType {
    lir::LirType::I64
}

fn f32() -> lir::LirType {
    lir::LirType::F32
}

fn f64() -> lir::LirType {
    lir::LirType::F64
}

fn void() -> lir::LirType {
    lir::LirType::Void
}

static RUNTIME_DECLS: LazyLock<HashMap<&'static str, IntrinsicSignature>> = LazyLock::new(|| {
    let mut map: HashMap<&'static str, IntrinsicSignature> = HashMap::new();

    map.insert(
        "puts",
        IntrinsicSignature::new("puts", vec![ptr()], i32(), false),
    );
    map.insert(
        "printf",
        IntrinsicSignature::new("printf", vec![ptr()], i32(), true),
    );
    map.insert(
        "fprintf",
        IntrinsicSignature::new("fprintf", vec![ptr(), ptr()], i32(), true),
    );
    map.insert(
        "sprintf",
        IntrinsicSignature::new("sprintf", vec![ptr(), ptr()], i32(), true),
    );
    map.insert(
        "snprintf",
        IntrinsicSignature::new("snprintf", vec![ptr(), i64(), ptr()], i32(), true),
    );

    map.insert(
        "malloc",
        IntrinsicSignature::new("malloc", vec![i64()], ptr(), false),
    );
    map.insert(
        "free",
        IntrinsicSignature::new("free", vec![ptr()], void(), false),
    );
    map.insert(
        "realloc",
        IntrinsicSignature::new("realloc", vec![ptr(), i64()], ptr(), false),
    );
    map.insert(
        "calloc",
        IntrinsicSignature::new("calloc", vec![i64(), i64()], ptr(), false),
    );

    map.insert(
        "strlen",
        IntrinsicSignature::new("strlen", vec![ptr()], i64(), false),
    );
    map.insert(
        "strcmp",
        IntrinsicSignature::new("strcmp", vec![ptr(), ptr()], i32(), false),
    );
    map.insert(
        "strncmp",
        IntrinsicSignature::new("strncmp", vec![ptr(), ptr(), i64()], i32(), false),
    );
    map.insert(
        "strcpy",
        IntrinsicSignature::new("strcpy", vec![ptr(), ptr()], ptr(), false),
    );
    map.insert(
        "strncpy",
        IntrinsicSignature::new("strncpy", vec![ptr(), ptr(), i64()], ptr(), false),
    );
    map.insert(
        "strcat",
        IntrinsicSignature::new("strcat", vec![ptr(), ptr()], ptr(), false),
    );
    map.insert(
        "strncat",
        IntrinsicSignature::new("strncat", vec![ptr(), ptr(), i64()], ptr(), false),
    );
    map.insert(
        "strchr",
        IntrinsicSignature::new("strchr", vec![ptr(), i32()], ptr(), false),
    );
    map.insert(
        "strstr",
        IntrinsicSignature::new("strstr", vec![ptr(), ptr()], ptr(), false),
    );

    map.insert(
        "sin",
        IntrinsicSignature::new("sin", vec![f64()], f64(), false),
    );
    map.insert(
        "cos",
        IntrinsicSignature::new("cos", vec![f64()], f64(), false),
    );
    map.insert(
        "tan",
        IntrinsicSignature::new("tan", vec![f64()], f64(), false),
    );
    map.insert(
        "sqrt",
        IntrinsicSignature::new("sqrt", vec![f64()], f64(), false),
    );
    map.insert(
        "log",
        IntrinsicSignature::new("log", vec![f64()], f64(), false),
    );
    map.insert(
        "exp",
        IntrinsicSignature::new("exp", vec![f64()], f64(), false),
    );

    map.insert(
        "sinf",
        IntrinsicSignature::new("sinf", vec![f32()], f32(), false),
    );
    map.insert(
        "cosf",
        IntrinsicSignature::new("cosf", vec![f32()], f32(), false),
    );
    map.insert(
        "tanf",
        IntrinsicSignature::new("tanf", vec![f32()], f32(), false),
    );
    map.insert(
        "sqrtf",
        IntrinsicSignature::new("sqrtf", vec![f32()], f32(), false),
    );
    map.insert(
        "logf",
        IntrinsicSignature::new("logf", vec![f32()], f32(), false),
    );
    map.insert(
        "expf",
        IntrinsicSignature::new("expf", vec![f32()], f32(), false),
    );

    map.insert(
        "pow",
        IntrinsicSignature::new("pow", vec![f64(), f64()], f64(), false),
    );
    map.insert(
        "powf",
        IntrinsicSignature::new("powf", vec![f32(), f32()], f32(), false),
    );
    map.insert(
        "fabs",
        IntrinsicSignature::new("fabs", vec![f64()], f64(), false),
    );
    map.insert(
        "fabsf",
        IntrinsicSignature::new("fabsf", vec![f32()], f32(), false),
    );

    map.insert(
        "exit",
        IntrinsicSignature::new("exit", vec![i32()], void(), false),
    );
    map.insert(
        "abort",
        IntrinsicSignature::new("abort", vec![], void(), false),
    );
    map.insert(
        "atexit",
        IntrinsicSignature::new("atexit", vec![ptr()], i32(), false),
    );
    map.insert(
        "fp_panic",
        IntrinsicSignature::new("fp_panic", vec![ptr()], void(), false),
    );

    map.insert(
        "fopen",
        IntrinsicSignature::new("fopen", vec![ptr(), ptr()], ptr(), false),
    );
    map.insert(
        "fclose",
        IntrinsicSignature::new("fclose", vec![ptr()], i32(), false),
    );
    map.insert(
        "fread",
        IntrinsicSignature::new("fread", vec![ptr(), i64(), i64(), ptr()], i64(), false),
    );
    map.insert(
        "fwrite",
        IntrinsicSignature::new("fwrite", vec![ptr(), i64(), i64(), ptr()], i64(), false),
    );
    map.insert(
        "fseek",
        IntrinsicSignature::new("fseek", vec![ptr(), i64(), i32()], i32(), false),
    );
    map.insert(
        "ftell",
        IntrinsicSignature::new("ftell", vec![ptr()], i64(), false),
    );

    map
});

/// Manager for C runtime intrinsic declarations.
pub struct CRuntimeIntrinsics;

impl CRuntimeIntrinsics {
    /// Get the signature for a C runtime intrinsic.
    pub fn get_intrinsic_signature(fn_name: &str) -> Option<IntrinsicSignature> {
        RUNTIME_DECLS.get(fn_name).cloned()
    }

    /// Check if a function name is a known C runtime intrinsic.
    pub fn is_runtime_intrinsic(fn_name: &str) -> bool {
        RUNTIME_DECLS.contains_key(fn_name)
    }
}
