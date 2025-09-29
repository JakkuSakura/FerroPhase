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

const PRINTLN_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&PRINTLN_CALL),
}];

const PRINT_BACKENDS: &[IntrinsicBackendSpec] = &[IntrinsicBackendSpec {
    backend: BackendFlavor::Llvm,
    behaviour: IntrinsicBackendBehaviour::Call(&PRINT_CALL),
}];

const SIZE_OF_BACKENDS: &[IntrinsicBackendSpec] = &[];

const PRINTLN_ALIASES: &[&str] = &["println", "println!", "std::io::println"];

const PRINT_ALIASES: &[&str] = &["print", "print!", "std::io::print"];

const SIZE_OF_ALIASES: &[&str] = &["std::builtins::size_of", "size_of"];

const INTRINSIC_SPECS: &[IntrinsicSpec] = &[
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
