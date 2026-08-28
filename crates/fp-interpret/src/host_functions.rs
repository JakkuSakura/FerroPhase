use std::collections::HashMap;
use std::ffi::c_void;

use fp_core::HostFunctionDescriptor;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HostFunctionError {
    EmptyName,
    Duplicate(String),
    InvalidAddress { name: String },
}

impl std::fmt::Display for HostFunctionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyName => write!(f, "host function name must not be empty"),
            Self::Duplicate(name) => write!(f, "host function {name} is already registered"),
            Self::InvalidAddress { name } => {
                write!(f, "host function {name} has an invalid address")
            }
        }
    }
}

impl std::error::Error for HostFunctionError {}

#[derive(Clone)]
pub struct HostFunction {
    pub descriptor: HostFunctionDescriptor,
    address: usize,
}

impl HostFunction {
    pub fn address(&self) -> *const c_void {
        self.address as *const c_void
    }
}

#[derive(Default, Clone)]
pub struct HostFunctionRegistry {
    functions: HashMap<String, HostFunction>,
}

impl HostFunctionRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a host function pointer using the exact LIR signature emitted
    /// for the matching `extern "host" fn` declaration.
    pub fn register(
        &mut self,
        descriptor: HostFunctionDescriptor,
        address: *const c_void,
    ) -> Result<(), HostFunctionError> {
        let name = descriptor.name.clone();
        if name.is_empty() {
            return Err(HostFunctionError::EmptyName);
        }
        if self.functions.contains_key(&name) {
            return Err(HostFunctionError::Duplicate(name));
        }
        if address.is_null() {
            return Err(HostFunctionError::InvalidAddress { name });
        }
        self.functions.insert(
            name,
            HostFunction {
                descriptor,
                address: address as usize,
            },
        );
        Ok(())
    }

    pub fn get(&self, name: &str) -> Option<&HostFunction> {
        self.functions.get(name)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str, &HostFunction)> {
        self.functions
            .iter()
            .map(|(name, function)| (name.as_str(), function))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::lir::{LirFunctionSignature, LirType};

    fn descriptor(name: &str) -> HostFunctionDescriptor {
        HostFunctionDescriptor::new(
            name,
            LirFunctionSignature {
                params: vec![LirType::I64],
                return_type: LirType::I64,
                is_variadic: false,
            },
        )
    }

    #[test]
    fn validates_host_function_registration() {
        let mut registry = HostFunctionRegistry::new();
        assert_eq!(
            registry.register(descriptor(""), 1usize as *const c_void),
            Err(HostFunctionError::EmptyName)
        );
        assert_eq!(
            registry.register(descriptor("add"), std::ptr::null()),
            Err(HostFunctionError::InvalidAddress { name: "add".into() })
        );
        registry
            .register(descriptor("add"), 1usize as *const c_void)
            .unwrap();
        assert_eq!(
            registry.register(descriptor("add"), 1usize as *const c_void),
            Err(HostFunctionError::Duplicate("add".into()))
        );
    }
}
