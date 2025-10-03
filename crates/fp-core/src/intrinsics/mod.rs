//! Shared intrinsic and standard-library descriptors.
//!
//! Front-ends normalise language-specific helpers into symbolic intrinsics while
//! back-ends materialise those symbols into concrete runtime calls.  The goal of
//! this module is to host the shared vocabulary so every consumer speaks the same
//! language before we introduce the backend-specific resolver.

mod catalog;
pub mod runtime;

use std::collections::HashMap;
use std::sync::LazyLock;

pub use catalog::{
    all_specs as intrinsic_specs, lookup_backend_behaviour, lookup_call as lookup_runtime_call,
    lookup_spec as lookup_intrinsic_spec, IntrinsicBackendBehaviour, IntrinsicBackendSpec,
    IntrinsicSpec, ResolvedCallSpec,
};
pub use runtime::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StdIntrinsic {
    // I/O
    IoPrintln,
    IoPrint,
    IoEprint,
    IoEprintln,

    // Memory allocation
    AllocAlloc,
    AllocDealloc,
    AllocRealloc,

    // Math - f64
    F64Sin,
    F64Cos,
    F64Tan,
    F64Sqrt,
    F64Pow,
    F64Log,
    F64Exp,

    // Math - f32
    F32Sin,
    F32Cos,
    F32Tan,
    F32Sqrt,
    F32Pow,
    F32Log,
    F32Exp,

    // String operations
    StrLen,
    StrCmp,

    // Process control
    ProcessExit,
    ProcessAbort,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinIntrinsic {
    SizeOf,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompilerIntrinsic {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicKind {
    Std(StdIntrinsic),
    Builtin(BuiltinIntrinsic),
    Intrinsic(CompilerIntrinsic),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BackendFlavor {
    Llvm,
    Interpreter,
    TranspileRust,
    TranspileJavascript,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntrinsicArgKind {
    /// Pass the value unchanged.
    Value,
    /// Expect a string literal and normalise it for the backend.
    FormatString,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntrinsicArg {
    pub kind: IntrinsicArgKind,
}

impl IntrinsicArg {
    pub const VALUE: Self = Self {
        kind: IntrinsicArgKind::Value,
    };

    pub const FORMAT_STRING: Self = Self {
        kind: IntrinsicArgKind::FormatString,
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallAbi {
    Default,
    CVariadic,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallArgStrategy {
    Passthrough,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntrinsicReturnKind {
    Void,
    SameAsArgument(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedCall {
    pub callee: &'static str,
    pub abi: CallAbi,
    pub variadic: bool,
    pub append_newline_to_first_string: bool,
    pub arg_strategy: CallArgStrategy,
    pub args: Vec<IntrinsicArg>,
    pub return_kind: IntrinsicReturnKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedIntrinsic {
    Call(ResolvedCall),
    InlineEmitter,
    Unsupported(String),
}

pub type ResolverKey = (IntrinsicKind, BackendFlavor);

#[derive(Default)]
pub struct IntrinsicResolver {
    entries: HashMap<ResolverKey, ResolvedIntrinsic>,
}

pub mod calls;

pub use calls::{IntrinsicCall, IntrinsicCallKind, IntrinsicCallPayload};

impl IntrinsicResolver {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    pub fn register(&mut self, key: ResolverKey, value: ResolvedIntrinsic) {
        self.entries.insert(key, value);
    }

    pub fn with_entry(mut self, key: ResolverKey, value: ResolvedIntrinsic) -> Self {
        self.register(key, value);
        self
    }

    pub fn resolve(&self, key: &ResolverKey) -> Option<&ResolvedIntrinsic> {
        self.entries.get(key)
    }
}

pub static DEFAULT_RESOLVER: LazyLock<IntrinsicResolver> = LazyLock::new(|| {
    let mut resolver = IntrinsicResolver::new();

    for spec in catalog::all_specs() {
        for backend in spec.backends {
            let value = match backend.behaviour {
                catalog::IntrinsicBackendBehaviour::Call(call) => {
                    ResolvedIntrinsic::Call(call.as_resolved_call())
                }
                catalog::IntrinsicBackendBehaviour::Unsupported(message) => {
                    ResolvedIntrinsic::Unsupported(message.to_string())
                }
            };

            resolver.register((spec.kind, backend.backend), value);
        }
    }

    resolver
});

pub fn default_resolver() -> &'static IntrinsicResolver {
    &DEFAULT_RESOLVER
}

pub fn identify_symbol(symbol: &str) -> Option<IntrinsicKind> {
    catalog::lookup_spec(symbol).map(|spec| spec.kind)
}

/// Result of materializing a print intrinsic
pub struct MaterializedPrint {
    pub format_literal: String,
    pub printf_function_name: String,
}

/// Trait for backend-specific THIR intrinsic materialization.
///
/// Each backend implements this trait to define how intrinsic calls are transformed
/// at the THIR level into backend-specific representations.
pub trait IntrinsicMaterializer: Send + Sync {
    /// Prepare materialization data for a print/println intrinsic call.
    ///
    /// Returns `Some(data)` if this backend handles print/println at THIR level,
    /// or `None` to keep the intrinsic call for later backend-specific handling.
    fn prepare_print(
        &self,
        kind: calls::IntrinsicCallKind,
        template: &crate::hir::typed::FormatString,
    ) -> Option<MaterializedPrint>;

    /// Check if this materializer handles the given intrinsic kind at THIR level.
    fn can_materialize(&self, kind: calls::IntrinsicCallKind) -> bool;
}
