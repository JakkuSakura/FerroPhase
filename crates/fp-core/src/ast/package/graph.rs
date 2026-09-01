use crate::ast::package::{PackageDescriptor, PackageId};

#[derive(Clone, Debug)]
pub struct PackageGraph {
    pub package: PackageDescriptor,
}

impl PackageGraph {
    pub fn new(package: PackageDescriptor) -> Self {
        Self { package }
    }

    pub fn package(&self, id: &PackageId) -> Option<&PackageDescriptor> {
        (self.package.id == *id).then_some(&self.package)
    }

    pub fn packages(&self) -> impl Iterator<Item = &PackageDescriptor> {
        std::iter::once(&self.package)
    }
}
