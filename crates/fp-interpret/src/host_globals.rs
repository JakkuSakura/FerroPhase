use std::collections::HashMap;
use fp_core::lir::LirType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HostGlobalDescriptor {
    pub name: String,
    pub ty: LirType,
    pub mutable: bool,
}

#[derive(Clone)]
pub struct HostGlobal {
    pub descriptor: HostGlobalDescriptor,
    address: usize,
}

impl HostGlobal {
    pub fn address(&self) -> *mut u8 { self.address as *mut u8 }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HostGlobalError {
    EmptyName,
    Duplicate(String),
    InvalidAddress { name: String },
}

impl std::fmt::Display for HostGlobalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyName => write!(f, "host global name must not be empty"),
            Self::Duplicate(name) => write!(f, "host global {name} is already registered"),
            Self::InvalidAddress { name } => write!(f, "host global {name} has an invalid address"),
        }
    }
}

impl std::error::Error for HostGlobalError {}

#[derive(Default, Clone)]
pub struct HostGlobalRegistry {
    globals: HashMap<String, HostGlobal>,
}

impl HostGlobalRegistry {
    pub fn new() -> Self { Self::default() }

    pub fn register(
        &mut self,
        name: impl Into<String>,
        ty: LirType,
        address: *mut u8,
        mutable: bool,
    ) -> Result<(), HostGlobalError> {
        let name = name.into();
        if name.is_empty() { return Err(HostGlobalError::EmptyName); }
        if self.globals.contains_key(&name) { return Err(HostGlobalError::Duplicate(name)); }
        if address.is_null() { return Err(HostGlobalError::InvalidAddress { name }); }
        self.globals.insert(name.clone(), HostGlobal { descriptor: HostGlobalDescriptor { name, ty, mutable }, address: address as usize });
        Ok(())
    }

    pub fn get(&self, name: &str) -> Option<&HostGlobal> { self.globals.get(name) }
    pub fn iter(&self) -> impl Iterator<Item = (&str, &HostGlobal)> { self.globals.iter().map(|(name, global)| (name.as_str(), global)) }
}
