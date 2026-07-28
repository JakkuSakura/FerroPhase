use std::collections::HashMap;

use crate::module::ModuleDescriptor;
use crate::module::ModuleId;
use crate::package::{PackageDescriptor, PackageId};

#[derive(Clone, Debug)]
pub struct PackageGraph {
    packages: HashMap<PackageId, PackageDescriptor>,
    modules: HashMap<ModuleId, ModuleDescriptor>,
    package_by_name: HashMap<String, PackageId>,
    modules_by_package: HashMap<PackageId, Vec<ModuleId>>,
}

impl PackageGraph {
    pub fn new(packages: Vec<PackageDescriptor>) -> Self {
        let mut graph = Self {
            packages: HashMap::new(),
            modules: HashMap::new(),
            package_by_name: HashMap::new(),
            modules_by_package: HashMap::new(),
        };
        for package in packages {
            graph.insert_package(package);
        }
        graph
    }

    pub fn insert_package(&mut self, package: PackageDescriptor) {
        let package_id = package.id.clone();
        self.package_by_name
            .insert(package.name.clone(), package_id.clone());
        let module_ids: Vec<ModuleId> = package.modules.clone();
        self.modules_by_package
            .insert(package_id.clone(), module_ids);
        self.packages.insert(package_id, package);
    }

    pub fn insert_module(&mut self, module: ModuleDescriptor) {
        self.modules.insert(module.id.clone(), module);
    }

    pub fn package(&self, id: &PackageId) -> Option<&PackageDescriptor> {
        self.packages.get(id)
    }

    pub fn package_by_name(&self, name: &str) -> Option<&PackageId> {
        self.package_by_name.get(name)
    }

    pub fn module(&self, id: &ModuleId) -> Option<&ModuleDescriptor> {
        self.modules.get(id)
    }

    pub fn modules_for_package(&self, id: &PackageId) -> Option<&[ModuleId]> {
        self.modules_by_package.get(id).map(|mods| mods.as_slice())
    }

    pub fn packages(&self) -> impl Iterator<Item = &PackageDescriptor> {
        self.packages.values()
    }

    pub fn modules(&self) -> impl Iterator<Item = &ModuleDescriptor> {
        self.modules.values()
    }
}
