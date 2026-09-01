use std::collections::HashMap;

use crate::ast::module::ModuleDescriptor;
use crate::ast::module::ModuleId;
use crate::ast::package::{PackageDescriptor, PackageId};

#[derive(Clone, Debug, Default)]
pub struct PackageGraph {
    packages: HashMap<PackageId, PackageDescriptor>,
    modules: HashMap<ModuleId, ModuleDescriptor>,
}

impl PackageGraph {
    pub fn new(packages: Vec<PackageDescriptor>) -> Self {
        let mut graph = Self {
            packages: HashMap::new(),
            modules: HashMap::new(),
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

    pub fn insert_module(&mut self, module: ModuleDescriptor) {
        self.modules.insert(module.id.clone(), module);
    }

    pub fn package(&self, id: &PackageId) -> Option<&PackageDescriptor> {
        self.packages.get(id)
    }

    pub fn packages(&self) -> impl Iterator<Item = &PackageDescriptor> {
        self.packages.values()
    }

}
