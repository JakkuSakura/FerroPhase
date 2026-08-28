use crate::lir::LirFunctionSignature;

/// The host-side contract for a function declared with `extern "host"`.
#[derive(Debug, Clone, PartialEq)]
pub struct HostFunctionDescriptor {
    pub name: String,
    pub signature: LirFunctionSignature,
}

impl HostFunctionDescriptor {
    pub fn new(name: impl Into<String>, signature: LirFunctionSignature) -> Self {
        Self {
            name: name.into(),
            signature,
        }
    }
}
