use std::collections::HashMap;

use crate::lir::{LirDataLayout, LirDataLayoutError, LirType, layout::StructLayout};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HostFieldDescriptor {
    pub name: &'static str,
    pub ty: LirType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HostLayoutDescriptor {
    pub name: &'static str,
    pub fields: &'static [HostFieldDescriptor],
    pub packed: bool,
}

impl HostLayoutDescriptor {
    pub const fn new(
        name: &'static str,
        fields: &'static [HostFieldDescriptor],
        packed: bool,
    ) -> Self {
        Self {
            name,
            fields,
            packed,
        }
    }

    pub fn lir_type(&self) -> LirType {
        LirType::Struct {
            fields: self.fields.iter().map(|field| field.ty.clone()).collect(),
            packed: self.packed,
            name: Some(self.name.to_owned()),
        }
    }

    pub fn layout(&self, data_layout: &LirDataLayout) -> Result<StructLayout, LirDataLayoutError> {
        data_layout
            .struct_layout(&self.lir_type())?
            .ok_or_else(|| LirDataLayoutError::ExpectedStruct(self.lir_type()))
    }
}

pub trait HostLayout {
    const DESCRIPTOR: HostLayoutDescriptor;
}

#[derive(Debug, Default)]
pub struct HostLayoutRegistry {
    descriptors: HashMap<&'static str, HostLayoutDescriptor>,
}

impl HostLayoutRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register<T: HostLayout>(&mut self) -> &HostLayoutDescriptor {
        let descriptor = T::DESCRIPTOR.clone();
        self.descriptors
            .entry(descriptor.name)
            .or_insert(descriptor)
    }

    pub fn get(&self, name: &str) -> Option<&HostLayoutDescriptor> {
        self.descriptors.get(name)
    }
}
