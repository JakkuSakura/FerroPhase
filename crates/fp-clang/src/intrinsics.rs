//! Shared C runtime intrinsic declarations.
//!
//! The clang frontend and LLVM backend both need to refer to the same set of
//! C runtime helper declarations (e.g. `printf`, `puts`, allocation helpers).
//! Keeping the definitions here avoids duplicating the signature catalogue
//! across crates.

use llvm_ir::function::{CallingConvention, Function, Parameter};
use llvm_ir::module::{DLLStorageClass, Linkage, Visibility};
use llvm_ir::types::{FPType, Types};
use llvm_ir::*;
use std::collections::HashMap;
use std::sync::LazyLock;

/// Macro to declare C functions with less verbosity.
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

/// Manager for C runtime intrinsic declarations.
pub struct CRuntimeIntrinsics;

impl CRuntimeIntrinsics {
    /// Get the declaration for a C runtime intrinsic.
    pub fn get_intrinsic_decl(fn_name: &str, types: &Types) -> Option<Function> {
        RUNTIME_DECLS.get(fn_name).map(|factory| factory(types))
    }

    /// Check if a function name is a known C runtime intrinsic.
    pub fn is_runtime_intrinsic(fn_name: &str) -> bool {
        RUNTIME_DECLS.contains_key(fn_name)
    }

    // I/O function declarations
    pub fn declare_puts(types: &Types) -> Function {
        declare_c_func!("puts", "str", types.pointer(), types.i32())
    }

    pub fn declare_printf(types: &Types) -> Function {
        Function {
            name: "printf".to_string(),
            parameters: vec![Parameter {
                name: Name::Name(Box::new("fmt".to_string())),
                ty: types.pointer(),
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
            return_type: types.i32(),
        }
    }

    pub fn declare_fprintf(types: &Types) -> Function {
        Function {
            name: "fprintf".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("stream".to_string())),
                    ty: types.pointer(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("fmt".to_string())),
                    ty: types.pointer(),
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
            return_type: types.i32(),
        }
    }

    pub fn declare_sprintf(types: &Types) -> Function {
        Function {
            name: "sprintf".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("buffer".to_string())),
                    ty: types.pointer(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("fmt".to_string())),
                    ty: types.pointer(),
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
            return_type: types.i32(),
        }
    }

    pub fn declare_snprintf(types: &Types) -> Function {
        Function {
            name: "snprintf".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("buffer".to_string())),
                    ty: types.pointer(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("size".to_string())),
                    ty: types.i64(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("fmt".to_string())),
                    ty: types.pointer(),
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
            return_type: types.i32(),
        }
    }

    // Memory allocation functions
    pub fn declare_malloc(types: &Types) -> Function {
        declare_c_func!("malloc", "size", types.i64(), types.pointer())
    }

    pub fn declare_free(types: &Types) -> Function {
        declare_c_func!("free", "ptr", types.pointer(), types.void())
    }

    pub fn declare_realloc(types: &Types) -> Function {
        Function {
            name: "realloc".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("ptr".to_string())),
                    ty: types.pointer(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("size".to_string())),
                    ty: types.i64(),
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
            return_type: types.pointer(),
        }
    }

    pub fn declare_calloc(types: &Types) -> Function {
        Function {
            name: "calloc".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("count".to_string())),
                    ty: types.i64(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("size".to_string())),
                    ty: types.i64(),
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
            return_type: types.pointer(),
        }
    }

    // String functions
    pub fn declare_strlen(types: &Types) -> Function {
        declare_c_func!("strlen", "s", types.pointer(), types.i64())
    }

    pub fn declare_strcmp(types: &Types) -> Function {
        declare_c_func!(
            "strcmp",
            "a",
            types.pointer(),
            "b",
            types.pointer(),
            types.i32()
        )
    }

    pub fn declare_strncmp(types: &Types) -> Function {
        Function {
            name: "strncmp".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("a".to_string())),
                    ty: types.pointer(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("b".to_string())),
                    ty: types.pointer(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("n".to_string())),
                    ty: types.i64(),
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
            return_type: types.i32(),
        }
    }

    pub fn declare_strcpy(types: &Types) -> Function {
        declare_c_func!(
            "strcpy",
            "dest",
            types.pointer(),
            "src",
            types.pointer(),
            types.pointer()
        )
    }

    pub fn declare_strncpy(types: &Types) -> Function {
        Function {
            name: "strncpy".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("dest".to_string())),
                    ty: types.pointer(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("src".to_string())),
                    ty: types.pointer(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("n".to_string())),
                    ty: types.i64(),
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
            return_type: types.pointer(),
        }
    }

    pub fn declare_strcat(types: &Types) -> Function {
        declare_c_func!(
            "strcat",
            "dest",
            types.pointer(),
            "src",
            types.pointer(),
            types.pointer()
        )
    }

    pub fn declare_strncat(types: &Types) -> Function {
        Function {
            name: "strncat".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("dest".to_string())),
                    ty: types.pointer(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("src".to_string())),
                    ty: types.pointer(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("n".to_string())),
                    ty: types.i64(),
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
            return_type: types.pointer(),
        }
    }

    pub fn declare_strchr(types: &Types) -> Function {
        declare_c_func!(
            "strchr",
            "s",
            types.pointer(),
            "c",
            types.i32(),
            types.pointer()
        )
    }

    pub fn declare_strstr(types: &Types) -> Function {
        declare_c_func!(
            "strstr",
            "haystack",
            types.pointer(),
            "needle",
            types.pointer(),
            types.pointer()
        )
    }

    // Math declarations shared with libm
    pub fn declare_math_f64(name: &str, types: &Types) -> Function {
        let f64_ty = types.fp(FPType::Double);
        Function {
            name: name.to_string(),
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

    pub fn declare_math_f32(name: &str, types: &Types) -> Function {
        let f32_ty = types.fp(FPType::Single);
        Function {
            name: name.to_string(),
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

    pub fn declare_pow_f64(types: &Types) -> Function {
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

    pub fn declare_pow_f32(types: &Types) -> Function {
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

    pub fn declare_fabs(types: &Types) -> Function {
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

    pub fn declare_fabsf(types: &Types) -> Function {
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

    // Process control
    pub fn declare_exit(types: &Types) -> Function {
        declare_c_func!("exit", "code", types.i32(), types.void())
    }

    pub fn declare_abort(types: &Types) -> Function {
        declare_c_func!("abort", types.void())
    }

    pub fn declare_atexit(types: &Types) -> Function {
        declare_c_func!("atexit", "handler", types.pointer(), types.i32())
    }

    // File I/O
    pub fn declare_fopen(types: &Types) -> Function {
        declare_c_func!(
            "fopen",
            "path",
            types.pointer(),
            "mode",
            types.pointer(),
            types.pointer()
        )
    }

    pub fn declare_fclose(types: &Types) -> Function {
        declare_c_func!("fclose", "file", types.pointer(), types.i32())
    }

    pub fn declare_fread(types: &Types) -> Function {
        Function {
            name: "fread".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("ptr".to_string())),
                    ty: types.pointer(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("size".to_string())),
                    ty: types.i64(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("nmemb".to_string())),
                    ty: types.i64(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("stream".to_string())),
                    ty: types.pointer(),
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
            return_type: types.i64(),
        }
    }

    pub fn declare_fwrite(types: &Types) -> Function {
        Function {
            name: "fwrite".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("ptr".to_string())),
                    ty: types.pointer(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("size".to_string())),
                    ty: types.i64(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("nmemb".to_string())),
                    ty: types.i64(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("stream".to_string())),
                    ty: types.pointer(),
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
            return_type: types.i64(),
        }
    }

    pub fn declare_fseek(types: &Types) -> Function {
        Function {
            name: "fseek".to_string(),
            parameters: vec![
                Parameter {
                    name: Name::Name(Box::new("stream".to_string())),
                    ty: types.pointer(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("offset".to_string())),
                    ty: types.i64(),
                    attributes: vec![],
                },
                Parameter {
                    name: Name::Name(Box::new("whence".to_string())),
                    ty: types.i32(),
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
            return_type: types.i32(),
        }
    }

    pub fn declare_ftell(types: &Types) -> Function {
        declare_c_func!("ftell", "stream", types.pointer(), types.i64())
    }
}
