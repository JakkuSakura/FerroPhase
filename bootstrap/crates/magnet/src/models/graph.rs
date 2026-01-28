use std::collections::BTreeMap;

use fp_core::package::graph::PackageGraph;

#[derive(Debug, Clone)]
pub struct PackageGraphOptions {
    pub include_dependencies: bool,
    pub include_dev_dependencies: bool,
    pub include_build_dependencies: bool,
}

impl Default for PackageGraphOptions {
    fn default() -> Self {
        Self {
            include_dependencies: true,
            include_dev_dependencies: false,
            include_build_dependencies: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PackageGraphSummary {
    pub package_count: usize,
    pub dependency_count: usize,
    pub packages: BTreeMap<String, Vec<String>>,
}

impl PackageGraphSummary {
    pub fn from_graph(graph: &PackageGraph) -> Self {
        let mut packages = BTreeMap::new();
        let mut dep_count = 0;
        for pkg in graph.packages() {
            let deps = pkg
                .metadata
                .dependencies
                .iter()
                .map(|dep| dep.package.clone())
                .collect::<Vec<_>>();
            dep_count += deps.len();
            packages.insert(pkg.name.clone(), deps);
        }
        Self {
            package_count: packages.len(),
            dependency_count: dep_count,
            packages,
        }
    }
}
