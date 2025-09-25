//! C Standard Library Function Declarations for LLVM IR Generation
//! 
//! This module provides declarations for C standard library functions that 
//! FerroPhase std library functions map to at runtime.

use llvm_ir::*;
use llvm_ir::types::{FPType, Types};
use llvm_ir::function::{Function, Parameter, CallingConvention};
use llvm_ir::module::{Linkage, Visibility, DLLStorageClass};

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
                }
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

/// C Standard Library function declarations manager
pub struct CStdLib;

impl CStdLib {
    /// Get function declaration for a C standard library function
    pub fn get_function_decl(fn_name: &str, types: &Types) -> Option<Function> {
        match fn_name {
            // I/O functions
            "puts" => Some(Self::declare_puts(types)),
            "printf" => Some(Self::declare_printf(types)),
            "fprintf" => Some(Self::declare_fprintf(types)),
            "sprintf" => Some(Self::declare_sprintf(types)),
            "snprintf" => Some(Self::declare_snprintf(types)),
            
            // Memory management functions
            "malloc" => Some(Self::declare_malloc(types)),
            "free" => Some(Self::declare_free(types)),
            "realloc" => Some(Self::declare_realloc(types)),
            "calloc" => Some(Self::declare_calloc(types)),
            
            // String functions
            "strlen" => Some(Self::declare_strlen(types)),
            "strcmp" => Some(Self::declare_strcmp(types)),
            "strncmp" => Some(Self::declare_strncmp(types)),
            "strcpy" => Some(Self::declare_strcpy(types)),
            "strncpy" => Some(Self::declare_strncpy(types)),
            "strcat" => Some(Self::declare_strcat(types)),
            "strncat" => Some(Self::declare_strncat(types)),
            "strchr" => Some(Self::declare_strchr(types)),
            "strstr" => Some(Self::declare_strstr(types)),
            
            // Math functions (libm)
            "sin" | "cos" | "tan" | "sqrt" | "log" | "exp" => Some(Self::declare_math_f64(fn_name, types)),
            "sinf" | "cosf" | "tanf" | "sqrtf" | "logf" | "expf" => Some(Self::declare_math_f32(fn_name, types)),
            "pow" => Some(Self::declare_pow_f64(types)),
            "powf" => Some(Self::declare_pow_f32(types)),
            "fabs" => Some(Self::declare_fabs(types)),
            "fabsf" => Some(Self::declare_fabsf(types)),
            
            // Process functions
            "exit" => Some(Self::declare_exit(types)),
            "abort" => Some(Self::declare_abort(types)),
            "atexit" => Some(Self::declare_atexit(types)),
            
            // File I/O functions
            "fopen" => Some(Self::declare_fopen(types)),
            "fclose" => Some(Self::declare_fclose(types)),
            "fread" => Some(Self::declare_fread(types)),
            "fwrite" => Some(Self::declare_fwrite(types)),
            "fseek" => Some(Self::declare_fseek(types)),
            "ftell" => Some(Self::declare_ftell(types)),
            
            _ => None,
        }
    }
    
    /// Check if a function name is a known C stdlib function
    pub fn is_stdlib_function(fn_name: &str) -> bool {
        matches!(fn_name,
            // I/O functions
            "puts" | "printf" | "fprintf" | "sprintf" | "snprintf" |
            // Memory management
            "malloc" | "free" | "realloc" | "calloc" |
            // String functions
            "strlen" | "strcmp" | "strncmp" | "strcpy" | "strncpy" | 
            "strcat" | "strncat" | "strchr" | "strstr" |
            // Math functions
            "sin" | "cos" | "tan" | "sqrt" | "log" | "exp" | "pow" | "fabs" |
            "sinf" | "cosf" | "tanf" | "sqrtf" | "logf" | "expf" | "powf" | "fabsf" |
            // Process functions
            "exit" | "abort" | "atexit" |
            // File I/O
            "fopen" | "fclose" | "fread" | "fwrite" | "fseek" | "ftell"
        )
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
                }
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
                }
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
                }
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
                }
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
                }
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
                }
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
                }
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
                }
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
                }
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
                }
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
                }
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
                }
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
                }
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
                }
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
                }
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
                }
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
                }
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
                }
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
                }
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