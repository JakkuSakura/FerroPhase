//! C Runtime Intrinsic Declarations for LLVM IR Generation
//!
//! This module provides declarations for the C runtime functions that
//! FerroPhase intrinsics map to at runtime.

use fp_core::intrinsics::{
    CallAbi, CallArgStrategy, IntrinsicArg, IntrinsicReturnKind, ResolvedCallSpec,
    ResolvedIntrinsic,
};
use llvm_ir::function::{CallingConvention, Function, Parameter};
use llvm_ir::module::{DLLStorageClass, Linkage, Visibility};
use llvm_ir::types::{FPType, Types};
use llvm_ir::*;
use std::collections::HashMap;
use std::sync::LazyLock;

/// Macro to declare C functions with less verbosity
macro_rules! declare_c_func {
    // Simple function with no parameters
    ($name:expr, $return_ty:expr) => {
        Function {
            name: $name.to_string(),
            parameters: vec![],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: $return_ty,
        }
    };

    // Function with one parameter
    ($name:expr, $param1_name:expr, $param1_ty:expr, $return_ty:expr) => {
        Function {
            name: $name.to_string(),
            parameters: vec![Parameter {
                name: Name::Name(Box::new($param1_name.to_string())),
                ty: $param1_ty,
                attributes: vec![],
            }],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: $return_ty,
        }
    };

    // Function with two parameters
    ($name:expr, $param1_name:expr, $param1_ty:expr, $param2_name:expr, $param2_ty:expr, $return_ty:expr) => {
        Function {
            name: $name.to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new($param1_name.to_string())),
                    ty: $param1_ty,
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new($param2_name.to_string())),
                    ty: $param2_ty,
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: $return_ty,
        }
    };

    // Variadic function with one required parameter
    (variadic $name:expr, $param1_name:expr, $param1_ty:expr, $return_ty:expr) => {
        Function {
            name: $name.to_string(),
            parameters: vec![Parameter {
                name: Name::Name(Box::new($param1_name.to_string())),
                ty: $param1_ty,
                attributes: vec![],
            }],
            is_var_arg: true,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: $return_ty,
        }
    };
}

type DeclFactory = fn(&Types) -> Function;

static RUNTIME_DECLS: LazyLock<HashMap<&'static str, DeclFactory>> = LazyLock::new(|| {
    let mut map: HashMap<&'static str, DeclFactory> = HashMap::new();
    map.insert("puts", CRuntimeIntrinsics::declare_puts as DeclFactory);
    map.insert("printf", CRuntimeIntrinsics::declare_printf as DeclFactory);
    map.insert(
        "fprintf",
        CRuntimeIntrinsics::declare_fprintf as DeclFactory,
    );
    map.insert(
        "sprintf",
        CRuntimeIntrinsics::declare_sprintf as DeclFactory,
    );
    map.insert(
        "snprintf",
        CRuntimeIntrinsics::declare_snprintf as DeclFactory,
    );
    map.insert("malloc", CRuntimeIntrinsics::declare_malloc as DeclFactory);
    map.insert("free", CRuntimeIntrinsics::declare_free as DeclFactory);
    map.insert(
        "realloc",
        CRuntimeIntrinsics::declare_realloc as DeclFactory,
    );
    map.insert("calloc", CRuntimeIntrinsics::declare_calloc as DeclFactory);
    map.insert("strlen", CRuntimeIntrinsics::declare_strlen as DeclFactory);
    map.insert("strcmp", CRuntimeIntrinsics::declare_strcmp as DeclFactory);
    map.insert(
        "strncmp",
        CRuntimeIntrinsics::declare_strncmp as DeclFactory,
    );
    map.insert("strcpy", CRuntimeIntrinsics::declare_strcpy as DeclFactory);
    map.insert(
        "strncpy",
        CRuntimeIntrinsics::declare_strncpy as DeclFactory,
    );
    map.insert("strcat", CRuntimeIntrinsics::declare_strcat as DeclFactory);
    map.insert(
        "strncat",
        CRuntimeIntrinsics::declare_strncat as DeclFactory,
    );
    map.insert("strchr", CRuntimeIntrinsics::declare_strchr as DeclFactory);
    map.insert("strstr", CRuntimeIntrinsics::declare_strstr as DeclFactory);
    map.insert("sin", declare_math_f64_sin as DeclFactory);
    map.insert("cos", declare_math_f64_cos as DeclFactory);
    map.insert("tan", declare_math_f64_tan as DeclFactory);
    map.insert("sqrt", declare_math_f64_sqrt as DeclFactory);
    map.insert("log", declare_math_f64_log as DeclFactory);
    map.insert("exp", declare_math_f64_exp as DeclFactory);
    map.insert("sinf", declare_math_f32_sin as DeclFactory);
    map.insert("cosf", declare_math_f32_cos as DeclFactory);
    map.insert("tanf", declare_math_f32_tan as DeclFactory);
    map.insert("sqrtf", declare_math_f32_sqrt as DeclFactory);
    map.insert("logf", declare_math_f32_log as DeclFactory);
    map.insert("expf", declare_math_f32_exp as DeclFactory);
    map.insert("pow", CRuntimeIntrinsics::declare_pow_f64 as DeclFactory);
    map.insert("powf", CRuntimeIntrinsics::declare_pow_f32 as DeclFactory);
    map.insert("fabs", CRuntimeIntrinsics::declare_fabs as DeclFactory);
    map.insert("fabsf", CRuntimeIntrinsics::declare_fabsf as DeclFactory);
    map.insert("exit", CRuntimeIntrinsics::declare_exit as DeclFactory);
    map.insert("abort", CRuntimeIntrinsics::declare_abort as DeclFactory);
    map.insert("atexit", CRuntimeIntrinsics::declare_atexit as DeclFactory);
    map.insert("fopen", CRuntimeIntrinsics::declare_fopen as DeclFactory);
    map.insert("fclose", CRuntimeIntrinsics::declare_fclose as DeclFactory);
    map.insert("fread", CRuntimeIntrinsics::declare_fread as DeclFactory);
    map.insert("fwrite", CRuntimeIntrinsics::declare_fwrite as DeclFactory);
    map.insert("fseek", CRuntimeIntrinsics::declare_fseek as DeclFactory);
    map.insert("ftell", CRuntimeIntrinsics::declare_ftell as DeclFactory);
    map
});

fn declare_math_f64_sin(types: &Types) -> Function {
    CRuntimeIntrinsics::declare_math_f64("sin", types)
}

fn declare_math_f64_cos(types: &Types) -> Function {
    CRuntimeIntrinsics::declare_math_f64("cos", types)
}

fn declare_math_f64_tan(types: &Types) -> Function {
    CRuntimeIntrinsics::declare_math_f64("tan", types)
}

fn declare_math_f64_sqrt(types: &Types) -> Function {
    CRuntimeIntrinsics::declare_math_f64("sqrt", types)
}

fn declare_math_f64_log(types: &Types) -> Function {
    CRuntimeIntrinsics::declare_math_f64("log", types)
}

fn declare_math_f64_exp(types: &Types) -> Function {
    CRuntimeIntrinsics::declare_math_f64("exp", types)
}

fn declare_math_f32_sin(types: &Types) -> Function {
    CRuntimeIntrinsics::declare_math_f32("sinf", types)
}

fn declare_math_f32_cos(types: &Types) -> Function {
    CRuntimeIntrinsics::declare_math_f32("cosf", types)
}

fn declare_math_f32_tan(types: &Types) -> Function {
    CRuntimeIntrinsics::declare_math_f32("tanf", types)
}

fn declare_math_f32_sqrt(types: &Types) -> Function {
    CRuntimeIntrinsics::declare_math_f32("sqrtf", types)
}

fn declare_math_f32_log(types: &Types) -> Function {
    CRuntimeIntrinsics::declare_math_f32("logf", types)
}

fn declare_math_f32_exp(types: &Types) -> Function {
    CRuntimeIntrinsics::declare_math_f32("expf", types)
}

const EMPTY_ARGS: &[IntrinsicArg] = &[];

const PRINTF_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "printf",
    abi: CallAbi::CVariadic,
    variadic: true,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const FPRINTF_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "fprintf",
    abi: CallAbi::CVariadic,
    variadic: true,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const MALLOC_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "malloc",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const FREE_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "free",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const REALLOC_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "realloc",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const SIN_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "sin",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const COS_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "cos",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const TAN_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "tan",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const SQRT_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "sqrt",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const POW_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "pow",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const POWF_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "powf",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const LOG_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "log",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const EXP_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "exp",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const SINF_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "sinf",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const COSF_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "cosf",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const TANF_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "tanf",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const SQRTF_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "sqrtf",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const LOGF_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "logf",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const EXPF_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "expf",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const STRLEN_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "strlen",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const STRCMP_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "strcmp",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const EXIT_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "exit",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

const ABORT_CALL_SPEC: ResolvedCallSpec = ResolvedCallSpec {
    callee: "abort",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: EMPTY_ARGS,
    return_kind: IntrinsicReturnKind::Void,
};

fn insert_aliases(
    map: &mut HashMap<&'static str, ResolvedCallSpec>,
    aliases: &[&'static str],
    spec: ResolvedCallSpec,
) {
    for alias in aliases {
        map.insert(*alias, spec);
    }
}

static FALLBACK_CALLS: LazyLock<HashMap<&'static str, ResolvedCallSpec>> = LazyLock::new(|| {
    let mut map: HashMap<&'static str, ResolvedCallSpec> = HashMap::new();
    insert_aliases(&mut map, &["printf"], PRINTF_CALL_SPEC);
    insert_aliases(
        &mut map,
        &["std::io::eprint", "eprint", "eprint!", "fprintf"],
        FPRINTF_CALL_SPEC,
    );
    insert_aliases(
        &mut map,
        &["std::io::eprintln", "eprintln", "eprintln!"],
        FPRINTF_CALL_SPEC,
    );
    insert_aliases(&mut map, &["std::alloc::alloc", "malloc"], MALLOC_CALL_SPEC);
    insert_aliases(&mut map, &["std::alloc::dealloc", "free"], FREE_CALL_SPEC);
    insert_aliases(
        &mut map,
        &["std::alloc::realloc", "realloc"],
        REALLOC_CALL_SPEC,
    );
    insert_aliases(&mut map, &["std::f64::sin", "sin"], SIN_CALL_SPEC);
    insert_aliases(&mut map, &["std::f64::cos", "cos"], COS_CALL_SPEC);
    insert_aliases(&mut map, &["std::f64::tan", "tan"], TAN_CALL_SPEC);
    insert_aliases(&mut map, &["std::f64::sqrt", "sqrt"], SQRT_CALL_SPEC);
    insert_aliases(&mut map, &["std::f64::pow", "pow"], POW_CALL_SPEC);
    insert_aliases(&mut map, &["std::f64::log", "log"], LOG_CALL_SPEC);
    insert_aliases(&mut map, &["std::f64::exp", "exp"], EXP_CALL_SPEC);
    insert_aliases(&mut map, &["std::f32::sin", "sinf"], SINF_CALL_SPEC);
    insert_aliases(&mut map, &["std::f32::cos", "cosf"], COSF_CALL_SPEC);
    insert_aliases(&mut map, &["std::f32::tan", "tanf"], TANF_CALL_SPEC);
    insert_aliases(&mut map, &["std::f32::sqrt", "sqrtf"], SQRTF_CALL_SPEC);
    insert_aliases(&mut map, &["std::f32::pow", "powf"], POWF_CALL_SPEC);
    insert_aliases(&mut map, &["std::f32::log", "logf"], LOGF_CALL_SPEC);
    insert_aliases(&mut map, &["std::f32::exp", "expf"], EXPF_CALL_SPEC);
    insert_aliases(&mut map, &["std::str::len", "strlen"], STRLEN_CALL_SPEC);
    insert_aliases(&mut map, &["std::str::cmp", "strcmp"], STRCMP_CALL_SPEC);
    insert_aliases(&mut map, &["std::process::exit", "exit"], EXIT_CALL_SPEC);
    insert_aliases(&mut map, &["std::process::abort", "abort"], ABORT_CALL_SPEC);
    map
});

pub fn runtime_call_spec(symbol: &str) -> Option<ResolvedCallSpec> {
    FALLBACK_CALLS.get(symbol).copied()
}

pub fn resolve_intrinsic(symbol: &str) -> Option<ResolvedIntrinsic> {
    runtime_call_spec(symbol).map(|spec| ResolvedIntrinsic::Call(spec.as_resolved_call()))
}

/// C runtime intrinsic declaration manager
pub struct CRuntimeIntrinsics;

impl CRuntimeIntrinsics {
    /// Get the declaration for a C runtime intrinsic
    pub fn get_intrinsic_decl(fn_name: &str, types: &Types) -> Option<Function> {
        RUNTIME_DECLS.get(fn_name).map(|factory| factory(types))
    }

    /// Check if a function name is a known C runtime intrinsic
    pub fn is_runtime_intrinsic(fn_name: &str) -> bool {
        RUNTIME_DECLS.contains_key(fn_name)
    }

    // I/O function declarations
    fn declare_puts(types: &Types) -> Function {
        declare_c_func!("puts", "str", types.pointer(), types.i32())
    }

    fn declare_printf(types: &Types) -> Function {
        declare_c_func!(variadic "printf", "format", types.pointer(), types.i32())
    }

    fn declare_fprintf(types: &Types) -> Function {
        let i32_ty = types.i32();
        let ptr_ty = types.pointer();
        Function {
            name: "fprintf".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("stream".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("format".to_string())),
                    ty: ptr_ty,
                    attributes: vec![],
                },
            ],
            is_var_arg: true,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: i32_ty,
        }
    }

    fn declare_sprintf(types: &Types) -> Function {
        let i32_ty = types.i32();
        let ptr_ty = types.pointer();
        Function {
            name: "sprintf".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("str".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("format".to_string())),
                    ty: ptr_ty,
                    attributes: vec![],
                },
            ],
            is_var_arg: true,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: i32_ty,
        }
    }

    fn declare_snprintf(types: &Types) -> Function {
        let i32_ty = types.i32();
        let ptr_ty = types.pointer();
        let size_ty = types.i64();
        Function {
            name: "snprintf".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("str".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("size".to_string())),
                    ty: size_ty,
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("format".to_string())),
                    ty: ptr_ty,
                    attributes: vec![],
                },
            ],
            is_var_arg: true,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: i32_ty,
        }
    }

    // Memory management functions
    fn declare_malloc(types: &Types) -> Function {
        declare_c_func!("malloc", "size", types.i64(), types.pointer())
    }

    fn declare_free(types: &Types) -> Function {
        declare_c_func!("free", "ptr", types.pointer(), types.void())
    }

    fn declare_realloc(types: &Types) -> Function {
        let ptr_ty = types.pointer();
        let size_ty = types.i64();
        Function {
            name: "realloc".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("ptr".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("size".to_string())),
                    ty: size_ty,
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: ptr_ty,
        }
    }

    fn declare_calloc(types: &Types) -> Function {
        let ptr_ty = types.pointer();
        let size_ty = types.i64();
        Function {
            name: "calloc".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("count".to_string())),
                    ty: size_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("size".to_string())),
                    ty: size_ty,
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: ptr_ty,
        }
    }

    // String functions
    fn declare_strlen(types: &Types) -> Function {
        let size_ty = types.i64();
        let ptr_ty = types.pointer();
        Function {
            name: "strlen".to_string(),
            parameters: vec![Parameter {
                name: Name::Name(Box::new("str".to_string())),
                ty: ptr_ty,
                attributes: vec![],
            }],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: size_ty,
        }
    }

    fn declare_strcmp(types: &Types) -> Function {
        let i32_ty = types.i32();
        let ptr_ty = types.pointer();
        Function {
            name: "strcmp".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("s1".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("s2".to_string())),
                    ty: ptr_ty,
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: i32_ty,
        }
    }

    fn declare_strncmp(types: &Types) -> Function {
        let i32_ty = types.i32();
        let ptr_ty = types.pointer();
        let size_ty = types.i64();
        Function {
            name: "strncmp".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("s1".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("s2".to_string())),
                    ty: ptr_ty,
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("n".to_string())),
                    ty: size_ty,
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: i32_ty,
        }
    }

    fn declare_strcpy(types: &Types) -> Function {
        let ptr_ty = types.pointer();
        Function {
            name: "strcpy".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("dest".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("src".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: ptr_ty,
        }
    }

    fn declare_strncpy(types: &Types) -> Function {
        let ptr_ty = types.pointer();
        let size_ty = types.i64();
        Function {
            name: "strncpy".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("dest".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("src".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("n".to_string())),
                    ty: size_ty,
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: ptr_ty,
        }
    }

    fn declare_strcat(types: &Types) -> Function {
        let ptr_ty = types.pointer();
        Function {
            name: "strcat".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("dest".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("src".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: ptr_ty,
        }
    }

    fn declare_strncat(types: &Types) -> Function {
        let ptr_ty = types.pointer();
        let size_ty = types.i64();
        Function {
            name: "strncat".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("dest".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("src".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("n".to_string())),
                    ty: size_ty,
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: ptr_ty,
        }
    }

    fn declare_strchr(types: &Types) -> Function {
        let ptr_ty = types.pointer();
        let i32_ty = types.i32();
        Function {
            name: "strchr".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("str".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("c".to_string())),
                    ty: i32_ty,
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: ptr_ty,
        }
    }

    fn declare_strstr(types: &Types) -> Function {
        let ptr_ty = types.pointer();
        Function {
            name: "strstr".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("haystack".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("needle".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: ptr_ty,
        }
    }

    // Math functions
    fn declare_math_f64(fn_name: &str, types: &Types) -> Function {
        let f64_ty = types.fp(FPType::Double);
        Function {
            name: fn_name.to_string(),
            parameters: vec![Parameter {
                name: Name::Name(Box::new("x".to_string())),
                ty: f64_ty.clone(),
                attributes: vec![],
            }],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: f64_ty,
        }
    }

    fn declare_math_f32(fn_name: &str, types: &Types) -> Function {
        let f32_ty = types.fp(FPType::Single);
        Function {
            name: fn_name.to_string(),
            parameters: vec![Parameter {
                name: Name::Name(Box::new("x".to_string())),
                ty: f32_ty.clone(),
                attributes: vec![],
            }],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: f32_ty,
        }
    }

    fn declare_pow_f64(types: &Types) -> Function {
        let f64_ty = types.fp(FPType::Double);
        Function {
            name: "pow".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("x".to_string())),
                    ty: f64_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("y".to_string())),
                    ty: f64_ty.clone(),
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: f64_ty,
        }
    }

    fn declare_pow_f32(types: &Types) -> Function {
        let f32_ty = types.fp(FPType::Single);
        Function {
            name: "powf".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("x".to_string())),
                    ty: f32_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("y".to_string())),
                    ty: f32_ty.clone(),
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: f32_ty,
        }
    }

    fn declare_fabs(types: &Types) -> Function {
        let f64_ty = types.fp(FPType::Double);
        Function {
            name: "fabs".to_string(),
            parameters: vec![Parameter {
                name: Name::Name(Box::new("x".to_string())),
                ty: f64_ty.clone(),
                attributes: vec![],
            }],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: f64_ty,
        }
    }

    fn declare_fabsf(types: &Types) -> Function {
        let f32_ty = types.fp(FPType::Single);
        Function {
            name: "fabsf".to_string(),
            parameters: vec![Parameter {
                name: Name::Name(Box::new("x".to_string())),
                ty: f32_ty.clone(),
                attributes: vec![],
            }],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: f32_ty,
        }
    }

    // Process functions
    fn declare_exit(types: &Types) -> Function {
        let void_ty = types.void();
        let i32_ty = types.i32();
        Function {
            name: "exit".to_string(),
            parameters: vec![Parameter {
                name: Name::Name(Box::new("status".to_string())),
                ty: i32_ty,
                attributes: vec![],
            }],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: void_ty,
        }
    }

    fn declare_abort(types: &Types) -> Function {
        let void_ty = types.void();
        Function {
            name: "abort".to_string(),
            parameters: vec![],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: void_ty,
        }
    }

    fn declare_atexit(types: &Types) -> Function {
        let i32_ty = types.i32();
        let ptr_ty = types.pointer();
        Function {
            name: "atexit".to_string(),
            parameters: vec![Parameter {
                name: Name::Name(Box::new("func".to_string())),
                ty: ptr_ty,
                attributes: vec![],
            }],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: i32_ty,
        }
    }

    // File I/O functions
    fn declare_fopen(types: &Types) -> Function {
        let ptr_ty = types.pointer();
        Function {
            name: "fopen".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("filename".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("mode".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: ptr_ty,
        }
    }

    fn declare_fclose(types: &Types) -> Function {
        let i32_ty = types.i32();
        let ptr_ty = types.pointer();
        Function {
            name: "fclose".to_string(),
            parameters: vec![Parameter {
                name: Name::Name(Box::new("stream".to_string())),
                ty: ptr_ty,
                attributes: vec![],
            }],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: i32_ty,
        }
    }

    fn declare_fread(types: &Types) -> Function {
        let size_ty = types.i64();
        let ptr_ty = types.pointer();
        Function {
            name: "fread".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("ptr".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("size".to_string())),
                    ty: size_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("count".to_string())),
                    ty: size_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("stream".to_string())),
                    ty: ptr_ty,
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: size_ty,
        }
    }

    fn declare_fwrite(types: &Types) -> Function {
        let size_ty = types.i64();
        let ptr_ty = types.pointer();
        Function {
            name: "fwrite".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("ptr".to_string())),
                    ty: ptr_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("size".to_string())),
                    ty: size_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("count".to_string())),
                    ty: size_ty.clone(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("stream".to_string())),
                    ty: ptr_ty,
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: size_ty,
        }
    }

    fn declare_fseek(types: &Types) -> Function {
        let i32_ty = types.i32();
        let ptr_ty = types.pointer();
        let long_ty = types.i64();
        Function {
            name: "fseek".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("stream".to_string())),
                    ty: ptr_ty,
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("offset".to_string())),
                    ty: long_ty,
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("whence".to_string())),
                    ty: i32_ty.clone(),
                    attributes: vec![],
                },
            ],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: i32_ty,
        }
    }

    fn declare_ftell(types: &Types) -> Function {
        let long_ty = types.i64();
        let ptr_ty = types.pointer();
        Function {
            name: "ftell".to_string(),
            parameters: vec![Parameter {
                name: Name::Name(Box::new("stream".to_string())),
                ty: ptr_ty,
                attributes: vec![],
            }],
            is_var_arg: false,
            visibility: Visibility::Default,
            linkage: Linkage::External,
            calling_convention: CallingConvention::C,
            section: None,
            comdat: None,
            alignment: 0,
            garbage_collector_name: None,
            personality_function: None,
            basic_blocks: vec![],
            function_attributes: vec![],
            return_attributes: vec![],
            dll_storage_class: DLLStorageClass::Default,
            debugloc: None,
            return_type: long_ty,
        }
    }
}
