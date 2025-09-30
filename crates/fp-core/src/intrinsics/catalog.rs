use super::{
    BackendFlavor, CallAbi, CallArgStrategy, IntrinsicArg, IntrinsicKind, IntrinsicReturnKind,
    ResolvedCall,
};
use std::collections::HashMap;
use std::sync::LazyLock;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ResolvedCallSpec {
    pub callee: &'static str,
    pub abi: CallAbi,
    pub variadic: bool,
    pub append_newline_to_first_string: bool,
    pub arg_strategy: CallArgStrategy,
    pub args: &'static [IntrinsicArg],
    pub return_kind: IntrinsicReturnKind,
}

impl ResolvedCallSpec {
    pub fn as_resolved_call(&self) -> ResolvedCall {
        ResolvedCall {
            callee: self.callee,
            abi: self.abi,
            variadic: self.variadic,
            append_newline_to_first_string: self.append_newline_to_first_string,
            arg_strategy: self.arg_strategy,
            args: self.args.to_vec(),
            return_kind: self.return_kind,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntrinsicBackendBehaviour {
    Call(&'static ResolvedCallSpec),
    Unsupported(&'static str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntrinsicBackendSpec {
    pub backend: BackendFlavor,
    pub behaviour: IntrinsicBackendBehaviour,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntrinsicSpec {
    pub kind: IntrinsicKind,
    pub canonical_symbol: &'static str,
    pub aliases: &'static [&'static str],
    pub backends: &'static [IntrinsicBackendSpec],
}

// I/O intrinsics
const PRINTLN_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "printf",
    abi: CallAbi::CVariadic,
    variadic: true,
    append_newline_to_first_string: true,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::Void,
};

const PRINT_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "printf",
    abi: CallAbi::CVariadic,
    variadic: true,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::Void,
};

const FPRINTF_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "fprintf",
    abi: CallAbi::CVariadic,
    variadic: true,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::Void,
};

// Memory allocation intrinsics
const MALLOC_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "malloc",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::Void,
};

const FREE_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "free",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::Void,
};

const REALLOC_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "realloc",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::Void,
};

// Math intrinsics - f64
const SIN_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "sin",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::SameAsArgument(0),
};

const COS_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "cos",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::SameAsArgument(0),
};

const TAN_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "tan",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::SameAsArgument(0),
};

const SQRT_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "sqrt",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::SameAsArgument(0),
};

const POW_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "pow",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::SameAsArgument(0),
};

const LOG_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "log",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::SameAsArgument(0),
};

const EXP_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "exp",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::SameAsArgument(0),
};

// Math intrinsics - f32
const SINF_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "sinf",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::SameAsArgument(0),
};

const COSF_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "cosf",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::SameAsArgument(0),
};

const TANF_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "tanf",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::SameAsArgument(0),
};

const SQRTF_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "sqrtf",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::SameAsArgument(0),
};

const POWF_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "powf",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::SameAsArgument(0),
};

const LOGF_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "logf",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::SameAsArgument(0),
};

const EXPF_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "expf",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::SameAsArgument(0),
};

// String operations
const STRLEN_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "strlen",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::Void,
};

const STRCMP_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "strcmp",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::Void,
};

// Process control
const EXIT_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "exit",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::Void,
};

const ABORT_CALL: ResolvedCallSpec = ResolvedCallSpec {
    callee: "abort",
    abi: CallAbi::Default,
    variadic: false,
    append_newline_to_first_string: false,
    arg_strategy: CallArgStrategy::Passthrough,
    args: &[],
    return_kind: IntrinsicReturnKind::Void,
};

// Backend specifications
const PRINTLN_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&PRINTLN_CALL),
}];

const PRINT_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&PRINT_CALL),
}];

const EPRINT_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&FPRINTF_CALL),
}];

const EPRINTLN_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&FPRINTF_CALL),
}];

const MALLOC_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&MALLOC_CALL),
}];

const FREE_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&FREE_CALL),
}];

const REALLOC_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&REALLOC_CALL),
}];

const SIN_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&SIN_CALL),
}];

const COS_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&COS_CALL),
}];

const TAN_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&TAN_CALL),
}];

const SQRT_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&SQRT_CALL),
}];

const POW_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&POW_CALL),
}];

const LOG_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&LOG_CALL),
}];

const EXP_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&EXP_CALL),
}];

const SINF_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&SINF_CALL),
}];

const COSF_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&COSF_CALL),
}];

const TANF_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&TANF_CALL),
}];

const SQRTF_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&SQRTF_CALL),
}];

const POWF_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&POWF_CALL),
}];

const LOGF_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&LOGF_CALL),
}];

const EXPF_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&EXPF_CALL),
}];

const STRLEN_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&STRLEN_CALL),
}];

const STRCMP_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&STRCMP_CALL),
}];

const EXIT_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&EXIT_CALL),
}];

const ABORT_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&ABORT_CALL),
}];

const SIZE_OF_BACKENDS: &[IntrinsicBackendSpec] = &[];

// Aliases
const PRINTLN_ALIASES: &[&str] = &["println", "println!", "std::io::println"];
const PRINT_ALIASES: &[&str] = &["print", "print!", "std::io::print"];
const EPRINT_ALIASES: &[&str] = &["eprint", "eprint!", "std::io::eprint", "fprintf"];
const EPRINTLN_ALIASES: &[&str] = &["eprintln", "eprintln!", "std::io::eprintln"];
const MALLOC_ALIASES: &[&str] = &["std::alloc::alloc", "malloc"];
const FREE_ALIASES: &[&str] = &["std::alloc::dealloc", "free"];
const REALLOC_ALIASES: &[&str] = &["std::alloc::realloc", "realloc"];
const SIN_ALIASES: &[&str] = &["std::f64::sin", "sin"];
const COS_ALIASES: &[&str] = &["std::f64::cos", "cos"];
const TAN_ALIASES: &[&str] = &["std::f64::tan", "tan"];
const SQRT_ALIASES: &[&str] = &["std::f64::sqrt", "sqrt"];
const POW_ALIASES: &[&str] = &["std::f64::pow", "pow"];
const LOG_ALIASES: &[&str] = &["std::f64::log", "log"];
const EXP_ALIASES: &[&str] = &["std::f64::exp", "exp"];
const SINF_ALIASES: &[&str] = &["std::f32::sin", "sinf"];
const COSF_ALIASES: &[&str] = &["std::f32::cos", "cosf"];
const TANF_ALIASES: &[&str] = &["std::f32::tan", "tanf"];
const SQRTF_ALIASES: &[&str] = &["std::f32::sqrt", "sqrtf"];
const POWF_ALIASES: &[&str] = &["std::f32::pow", "powf"];
const LOGF_ALIASES: &[&str] = &["std::f32::log", "logf"];
const EXPF_ALIASES: &[&str] = &["std::f32::exp", "expf"];
const STRLEN_ALIASES: &[&str] = &["std::str::len", "strlen"];
const STRCMP_ALIASES: &[&str] = &["std::str::cmp", "strcmp"];
const EXIT_ALIASES: &[&str] = &["std::process::exit", "exit"];
const ABORT_ALIASES: &[&str] = &["std::process::abort", "abort"];
const SIZE_OF_ALIASES: &[&str] = &["std::builtins::size_of", "size_of"];

const INTRINSIC_SPECS: &[IntrinsicSpec] = &[
    // I/O
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::IoPrintln),
        canonical_symbol: "std::io::println",
        aliases: PRINTLN_ALIASES,
        backends: PRINTLN_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::IoPrint),
        canonical_symbol: "std::io::print",
        aliases: PRINT_ALIASES,
        backends: PRINT_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::IoEprint),
        canonical_symbol: "std::io::eprint",
        aliases: EPRINT_ALIASES,
        backends: EPRINT_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::IoEprintln),
        canonical_symbol: "std::io::eprintln",
        aliases: EPRINTLN_ALIASES,
        backends: EPRINTLN_BACKENDS,
    },
    // Memory allocation
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::AllocAlloc),
        canonical_symbol: "std::alloc::alloc",
        aliases: MALLOC_ALIASES,
        backends: MALLOC_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::AllocDealloc),
        canonical_symbol: "std::alloc::dealloc",
        aliases: FREE_ALIASES,
        backends: FREE_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::AllocRealloc),
        canonical_symbol: "std::alloc::realloc",
        aliases: REALLOC_ALIASES,
        backends: REALLOC_BACKENDS,
    },
    // Math - f64
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::F64Sin),
        canonical_symbol: "std::f64::sin",
        aliases: SIN_ALIASES,
        backends: SIN_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::F64Cos),
        canonical_symbol: "std::f64::cos",
        aliases: COS_ALIASES,
        backends: COS_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::F64Tan),
        canonical_symbol: "std::f64::tan",
        aliases: TAN_ALIASES,
        backends: TAN_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::F64Sqrt),
        canonical_symbol: "std::f64::sqrt",
        aliases: SQRT_ALIASES,
        backends: SQRT_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::F64Pow),
        canonical_symbol: "std::f64::pow",
        aliases: POW_ALIASES,
        backends: POW_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::F64Log),
        canonical_symbol: "std::f64::log",
        aliases: LOG_ALIASES,
        backends: LOG_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::F64Exp),
        canonical_symbol: "std::f64::exp",
        aliases: EXP_ALIASES,
        backends: EXP_BACKENDS,
    },
    // Math - f32
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::F32Sin),
        canonical_symbol: "std::f32::sin",
        aliases: SINF_ALIASES,
        backends: SINF_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::F32Cos),
        canonical_symbol: "std::f32::cos",
        aliases: COSF_ALIASES,
        backends: COSF_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::F32Tan),
        canonical_symbol: "std::f32::tan",
        aliases: TANF_ALIASES,
        backends: TANF_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::F32Sqrt),
        canonical_symbol: "std::f32::sqrt",
        aliases: SQRTF_ALIASES,
        backends: SQRTF_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::F32Pow),
        canonical_symbol: "std::f32::pow",
        aliases: POWF_ALIASES,
        backends: POWF_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::F32Log),
        canonical_symbol: "std::f32::log",
        aliases: LOGF_ALIASES,
        backends: LOGF_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::F32Exp),
        canonical_symbol: "std::f32::exp",
        aliases: EXPF_ALIASES,
        backends: EXPF_BACKENDS,
    },
    // String operations
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::StrLen),
        canonical_symbol: "std::str::len",
        aliases: STRLEN_ALIASES,
        backends: STRLEN_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::StrCmp),
        canonical_symbol: "std::str::cmp",
        aliases: STRCMP_ALIASES,
        backends: STRCMP_BACKENDS,
    },
    // Process control
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::ProcessExit),
        canonical_symbol: "std::process::exit",
        aliases: EXIT_ALIASES,
        backends: EXIT_BACKENDS,
    },
    IntrinsicSpec {
        kind: IntrinsicKind::Std(super::StdIntrinsic::ProcessAbort),
        canonical_symbol: "std::process::abort",
        aliases: ABORT_ALIASES,
        backends: ABORT_BACKENDS,
    },
    // Builtins
    IntrinsicSpec {
        kind: IntrinsicKind::Builtin(super::BuiltinIntrinsic::SizeOf),
        canonical_symbol: "std::builtins::size_of",
        aliases: SIZE_OF_ALIASES,
        backends: SIZE_OF_BACKENDS,
    },
];

static LOOKUP_BY_SYMBOL: LazyLock<HashMap<&'static str, &'static IntrinsicSpec>> =
    LazyLock::new(|| {
        let mut map = HashMap::new();
        for spec in INTRINSIC_SPECS {
            map.insert(spec.canonical_symbol, spec);
            for alias in spec.aliases {
                map.insert(*alias, spec);
            }
        }
        map
    });

pub fn all_specs() -> &'static [IntrinsicSpec] {
    INTRINSIC_SPECS
}

pub fn lookup_spec(symbol: &str) -> Option<&'static IntrinsicSpec> {
    LOOKUP_BY_SYMBOL.get(symbol).copied()
}

pub fn lookup_call(symbol: &str, backend: BackendFlavor) -> Option<&'static ResolvedCallSpec> {
    let spec = lookup_spec(symbol)?;
    spec.backends
        .iter()
        .find(|entry| entry.backend == backend)
        .and_then(|entry| match entry.behaviour {
            IntrinsicBackendBehaviour::Call(call) => Some(call),
            IntrinsicBackendBehaviour::Unsupported(_) => None,
        })
}

pub fn lookup_backend_behaviour(
    symbol: &str,
    backend: BackendFlavor,
) -> Option<IntrinsicBackendBehaviour> {
    let spec = lookup_spec(symbol)?;
    spec.backends
        .iter()
        .find(|entry| entry.backend == backend)
        .map(|entry| entry.behaviour)
}
