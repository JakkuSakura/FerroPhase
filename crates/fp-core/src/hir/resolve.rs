//! Central module-tree resolution structure, replacing the flat
//! `module_defs`/`global_type_defs`/`global_value_defs`/`prelude_*`/
//! `crate_roots` tables `HirGenerator` used to keep independently (see
//! `docs/Resolution.md`). One `ModuleTree` lives per `hir::Package`.
//!
//! Every operation is O(depth) or O(1) — never a scan over every
//! definition in the program, unlike the flat-map design it replaces
//! (`expand_glob_import`'s old three-map scan-and-filter being the
//! clearest example of what this avoids).

use super::Res;
use crate::ast::path::QualifiedPath;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Namespace {
    Type,
    Value,
}

#[derive(Debug, Clone, PartialEq)]
struct ModuleNode {
    #[allow(dead_code)]
    parent: Option<ModuleId>,
    #[allow(dead_code)]
    name: String,
    children: HashMap<String, ModuleId>,
    bindings: [HashMap<String, Res>; 2],
}

impl ModuleNode {
    fn root() -> Self {
        Self {
            parent: None,
            name: String::new(),
            children: HashMap::new(),
            bindings: [HashMap::new(), HashMap::new()],
        }
    }
}

/// A module tree, rooted at `ModuleTree::root()`. Every real module path
/// this package ever mentions (whether via a literal `mod X { .. }` item
/// or a file-based provider assigning every source file its own module
/// path) becomes a node, reachable both by descending `child` from an
/// ancestor and by direct `QualifiedPath` lookup.
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleTree {
    nodes: Vec<ModuleNode>,
    by_path: HashMap<QualifiedPath, ModuleId>,
}

impl Default for ModuleTree {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleTree {
    pub fn new() -> Self {
        let mut by_path = HashMap::new();
        by_path.insert(QualifiedPath::new(Vec::new()), ModuleId(0));
        Self {
            nodes: vec![ModuleNode::root()],
            by_path,
        }
    }

    pub fn root(&self) -> ModuleId {
        ModuleId(0)
    }

    /// Ensures every segment of `path` exists as a node, creating any
    /// missing ones, and returns the id of the final segment's node.
    pub fn ensure_module(&mut self, path: &QualifiedPath) -> ModuleId {
        let mut current = self.root();
        let mut prefix = Vec::with_capacity(path.segments.len());
        for segment in &path.segments {
            prefix.push(segment.clone());
            current = self.ensure_child(current, segment, &prefix);
        }
        current
    }

    fn ensure_child(&mut self, parent: ModuleId, name: &str, prefix: &[String]) -> ModuleId {
        if let Some(existing) = self.nodes[parent.0 as usize].children.get(name) {
            return *existing;
        }
        let id = ModuleId(self.nodes.len() as u32);
        self.nodes.push(ModuleNode {
            parent: Some(parent),
            name: name.to_string(),
            children: HashMap::new(),
            bindings: [HashMap::new(), HashMap::new()],
        });
        self.nodes[parent.0 as usize]
            .children
            .insert(name.to_string(), id);
        self.by_path
            .insert(QualifiedPath::new(prefix.to_vec()), id);
        id
    }

    pub fn module_exists(&self, path: &QualifiedPath) -> bool {
        self.by_path.contains_key(path)
    }

    pub fn module_id(&self, path: &QualifiedPath) -> Option<ModuleId> {
        self.by_path.get(path).copied()
    }

    /// Every registered module path, at any depth — for callers that
    /// genuinely need the full set (e.g. an external helper's own
    /// `&HashSet<QualifiedPath>` parameter) rather than a single
    /// existence check or one level of children.
    pub fn all_paths(&self) -> impl Iterator<Item = &QualifiedPath> {
        self.by_path.keys()
    }

    /// Direct child lookup — replaces a flat-map `module_defs.contains(&candidate)`
    /// per-segment check with a single `HashMap` lookup on the parent node.
    pub fn child(&self, module: ModuleId, name: &str) -> Option<ModuleId> {
        self.nodes[module.0 as usize].children.get(name).copied()
    }

    pub fn bind(&mut self, module: ModuleId, ns: Namespace, name: &str, res: Res) {
        self.nodes[module.0 as usize].bindings[ns as usize].insert(name.to_string(), res);
    }

    pub fn lookup(&self, module: ModuleId, ns: Namespace, name: &str) -> Option<&Res> {
        self.nodes[module.0 as usize].bindings[ns as usize].get(name)
    }

    /// Every direct child of `module`, by name — replaces `expand_glob_import`'s
    /// old full scan across every global definition in the program with one
    /// `HashMap` iteration over just this node's own children.
    pub fn children(&self, module: ModuleId) -> impl Iterator<Item = (&str, ModuleId)> {
        self.nodes[module.0 as usize]
            .children
            .iter()
            .map(|(name, id)| (name.as_str(), *id))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn path(segments: &[&str]) -> QualifiedPath {
        QualifiedPath::new(segments.iter().map(|s| s.to_string()).collect())
    }

    #[test]
    fn ensure_module_creates_and_reuses_nodes() {
        let mut tree = ModuleTree::new();
        let a = tree.ensure_module(&path(&["std", "core"]));
        let b = tree.ensure_module(&path(&["std", "core"]));
        assert_eq!(a, b);
        assert!(tree.module_exists(&path(&["std", "core"])));
        assert!(tree.module_exists(&path(&["std"])));
        assert!(!tree.module_exists(&path(&["std", "alloc"])));
    }

    #[test]
    fn child_descends_one_segment_at_a_time() {
        let mut tree = ModuleTree::new();
        tree.ensure_module(&path(&["std", "core", "option"]));
        let std_id = tree.child(tree.root(), "std").expect("std child");
        let core_id = tree.child(std_id, "core").expect("core child");
        assert!(tree.child(core_id, "option").is_some());
        assert!(tree.child(core_id, "nonexistent").is_none());
    }

    #[test]
    fn bind_and_lookup_round_trip_per_namespace() {
        let mut tree = ModuleTree::new();
        let module = tree.ensure_module(&path(&["std", "core", "option"]));
        tree.bind(module, Namespace::Type, "Option", Res::SelfTy);
        assert!(matches!(
            tree.lookup(module, Namespace::Type, "Option"),
            Some(Res::SelfTy)
        ));
        assert!(tree.lookup(module, Namespace::Value, "Option").is_none());
    }

    #[test]
    fn children_lists_direct_children_only() {
        let mut tree = ModuleTree::new();
        tree.ensure_module(&path(&["std", "core"]));
        tree.ensure_module(&path(&["std", "alloc"]));
        tree.ensure_module(&path(&["std", "core", "option"]));
        let std_id = tree.child(tree.root(), "std").unwrap();
        let mut names: Vec<&str> = tree.children(std_id).map(|(name, _)| name).collect();
        names.sort();
        assert_eq!(names, vec!["alloc", "core"]);
    }
}
