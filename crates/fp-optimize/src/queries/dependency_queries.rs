// Dependency queries - stateless operations for dependency analysis

use crate::utils::evaluation_context::ConstBlock;
use fp_core::diagnostics::report_error;
use fp_core::error::Result;
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, HashSet};

/// Stateless dependency analysis queries
pub struct DependencyQueries;

impl DependencyQueries {
    pub fn new() -> Self {
        Self
    }

    /// Analyze dependencies between const blocks
    pub fn analyze_dependencies(
        &self,
        const_blocks: &HashMap<u64, ConstBlock>,
    ) -> Result<HashMap<u64, HashSet<u64>>> {
        let mut graph = HashMap::new();

        for (id, block) in const_blocks {
            let dependencies = block
                .dependencies
                .iter()
                .copied()
                .filter(|dep| const_blocks.contains_key(dep))
                .collect();
            graph.insert(*id, dependencies);
        }

        Ok(graph)
    }

    /// Compute topological order for evaluation
    pub fn compute_topological_order(
        &self,
        dependencies: &HashMap<u64, HashSet<u64>>,
    ) -> Result<Vec<u64>> {
        if dependencies.is_empty() {
            return Ok(Vec::new());
        }

        let mut in_degree: HashMap<u64, usize> = HashMap::new();
        let mut dependants: HashMap<u64, HashSet<u64>> = HashMap::new();

        for (node, deps) in dependencies {
            in_degree.entry(*node).or_insert(0);
            for dep in deps {
                if !dependencies.contains_key(dep) {
                    return Err(report_error(format!(
                        "Const-eval reference to unknown dependency {} from {}",
                        dep, node
                    )));
                }
                *in_degree.entry(*node).or_insert(0) += 1;
                in_degree.entry(*dep).or_insert(0);
                dependants.entry(*dep).or_default().insert(*node);
            }
        }

        let mut queue: BinaryHeap<Reverse<u64>> = in_degree
            .iter()
            .filter_map(|(node, degree)| {
                if *degree == 0 {
                    Some(Reverse(*node))
                } else {
                    None
                }
            })
            .collect();

        let mut ordered = Vec::with_capacity(in_degree.len());
        let mut remaining = in_degree.clone();

        while let Some(Reverse(node)) = queue.pop() {
            ordered.push(node);

            if let Some(children) = dependants.get(&node) {
                for child in children {
                    if let Some(entry) = remaining.get_mut(child) {
                        if *entry > 0 {
                            *entry -= 1;
                            if *entry == 0 {
                                queue.push(Reverse(*child));
                            }
                        }
                    }
                }
            }
        }

        if ordered.len() != remaining.len() {
            return Err(report_error("Const-eval circular dependency detected"));
        }

        Ok(ordered)
    }
}
