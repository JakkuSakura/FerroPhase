use std::collections::HashMap;

use crate::ast::package::{PackageDescriptor, PackageId};

#[derive(Clone, Debug, Default)]
pub struct PackageGraph {
    packages: HashMap<PackageId, PackageDescriptor>,
}

impl PackageGraph {
    pub fn new(packages: Vec<PackageDescriptor>) -> Self {
        let mut graph = Self {
            packages: HashMap::new(),
        };
        for package in packages {
            graph.insert_package(package);
        }
        graph
    }

    pub fn insert_package(&mut self, package: PackageDescriptor) {
        let package_id = package.id.clone();
        self.packages.insert(package_id, package);
    }

    pub fn package(&self, id: &PackageId) -> Option<&PackageDescriptor> {
        self.packages.get(id)
    }

    pub fn packages(&self) -> impl Iterator<Item = &PackageDescriptor> {
        self.packages.values()
    }

}
