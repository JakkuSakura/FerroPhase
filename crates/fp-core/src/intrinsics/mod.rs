//! Shared intrinsic and standard-library descriptors.
//!
//! Front-ends normalise language-specific helpers into symbolic intrinsics while
//! back-ends materialise those symbols into concrete runtime calls.  The goal of
//! this module is to host the shared vocabulary so every consumer speaks the same
//! language before we introduce the backend-specific resolver.

use std::collections::HashMap;
use std::sync::LazyLock;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StdIntrinsic {
    IoPrintln,
    IoPrint,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntrinsicArgKind {
    /// Pass the value unchanged.
    Value,
    /// Expect a string literal and normalise it for the backend.
    FormatString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CallAbi {
    Default,
    CVariadic,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallArgStrategy {
    Passthrough,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    resolver.register(
        (
            IntrinsicKind::Std(StdIntrinsic::IoPrintln),
            BackendFlavor::Llvm,
        ),
        ResolvedIntrinsic::Call(ResolvedCall {
            callee: "printf",
            abi: CallAbi::CVariadic,
            variadic: true,
            append_newline_to_first_string: true,
            arg_strategy: CallArgStrategy::Passthrough,
            args: Vec::new(),
            return_kind: IntrinsicReturnKind::Void,
        }),
    );
    resolver
});

pub fn default_resolver() -> &'static IntrinsicResolver {
    &DEFAULT_RESOLVER
}

pub fn identify_symbol(symbol: &str) -> Option<IntrinsicKind> {
    match symbol {
        "std::io::println" => Some(IntrinsicKind::Std(StdIntrinsic::IoPrintln)),
        "std::io::print" => Some(IntrinsicKind::Std(StdIntrinsic::IoPrint)),
        "std::builtins::size_of" => Some(IntrinsicKind::Builtin(BuiltinIntrinsic::SizeOf)),
        _ => None,
    }
}
