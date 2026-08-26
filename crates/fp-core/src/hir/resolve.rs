//! Central module-tree resolution structure, replacing the flat
//! `module_defs`/`global_type_defs`/`global_value_defs`/`prelude_*`/
//! `crate_roots` tables `AstToHirLowerer` used to keep independently (see
//! `docs/Resolution.md`). One `ModuleTree` lives per `hir::HirPackage`.
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

/// A name binding's resolved target plus the visibility/canonical-path
/// metadata a bare `Res` doesn't carry. Moved here (from `fp-backend`'s
/// `ast_to_hir` module) so `ModuleTree` bindings can hold it directly
/// instead of `AstToHirLowerer` keeping a second, parallel flat-map lookup
/// table just to carry this extra metadata (see `docs/Resolution.md`).
#[derive(Debug, Clone, PartialEq)]
pub struct SymbolEntry {
    pub res: Res,
    pub export: SymbolExport,
    pub path: Option<QualifiedPath>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolExport {
    Public,
    Scoped(Vec<String>),
}

impl SymbolExport {
    pub fn can_access(&self, current_module: &[String]) -> bool {
        match self {
            SymbolExport::Public => true,
            SymbolExport::Scoped(scope) => current_module.starts_with(scope.as_slice()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct ModuleNode {
    #[allow(dead_code)]
    parent: Option<ModuleId>,
    #[allow(dead_code)]
    name: String,
    children: HashMap<String, ModuleId>,
    bindings: [HashMap<String, SymbolEntry>; 2],
    /// True only for a node reached via `ensure_module` (a genuine `mod
    /// X { .. }`/file-based module) — false for a node that exists purely
    /// because `bind_symbol` needed *somewhere* to hang a struct/enum's own
    /// associated items (impl methods, variants) under its name, via
    /// `ensure_namespace`. Without this distinction, a struct/enum with any
    /// associated item at all (i.e. almost every real one) makes its own
    /// qualified path satisfy `module_exists`, so `register_import_binding`'s
    /// "is this name a module?" check wrongly wins over an ordinary item
    /// import — e.g. `use core::marker::PhantomData;` resolving `PhantomData`
    /// to `Res::Module(["core","marker","PhantomData"])` instead of the
    /// struct's own `Res::Def`, because `PhantomData::new()` (or any other
    /// associated item) had already caused `ensure_namespace` to create a
    /// node at that exact path.
    is_module: bool,
}

impl ModuleNode {
    fn root() -> Self {
        Self {
            parent: None,
            name: String::new(),
            children: HashMap::new(),
            bindings: [HashMap::new(), HashMap::new()],
            is_module: true,
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
            // Node 1 is the reserved prelude node (see `prelude()`) — not
            // reachable via `by_path`/`ensure_module`/`child`, since it
            // isn't a real module, just a place to hang bare-name fallback
            // bindings that `load_default_prelude_defs` populates.
            nodes: vec![ModuleNode::root(), ModuleNode::root()],
            by_path,
        }
    }

    pub fn root(&self) -> ModuleId {
        ModuleId(0)
    }

    /// Reserved node for package-scoped, unqualified prelude fallback
    /// bindings (replaces the old `prelude_type_defs`/`prelude_value_defs`
    /// flat maps) — not a real module, never reachable via `child`/
    /// `module_exists`/`ensure_module`.
    pub fn prelude(&self) -> ModuleId {
        ModuleId(1)
    }

    /// Ensures every segment of `path` exists as a node, creating any
    /// missing ones, and returns the id of the final segment's node —
    /// for a **genuine** module (a literal `mod X { .. }` item, or a
    /// file-based provider's own per-file module path): every node
    /// walked is marked `is_module = true` (a nested real module implies
    /// every ancestor segment is a real module too), so `module_exists`
    /// answers correctly. Callers that need a node purely to hang a
    /// struct/enum's own associated items under (not a real module) must
    /// use `ensure_namespace` instead, or `module_exists` would wrongly
    /// start reporting every struct/enum with any associated item as if
    /// it were also a module.
    pub fn ensure_module(&mut self, path: &QualifiedPath) -> ModuleId {
        let id = self.ensure_namespace(path);
        let mut current = self.root();
        self.nodes[current.0 as usize].is_module = true;
        for segment in &path.segments {
            current = self.nodes[current.0 as usize].children[segment];
            self.nodes[current.0 as usize].is_module = true;
        }
        id
    }

    /// Same node-creation walk as `ensure_module`, but never marks any
    /// node `is_module` — for a caller that just needs *somewhere* to
    /// bind a definition's own child namespace (an impl method, an enum
    /// variant) and doesn't know or care whether `path` also happens to
    /// be a real module's path (see `ModuleNode::is_module`'s doc
    /// comment). Reuses whatever's already there either way, so calling
    /// this after `ensure_module` already marked the same path real is a
    /// harmless no-op, not a downgrade.
    pub fn ensure_namespace(&mut self, path: &QualifiedPath) -> ModuleId {
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
            is_module: false,
        });
        self.nodes[parent.0 as usize]
            .children
            .insert(name.to_string(), id);
        self.by_path.insert(QualifiedPath::new(prefix.to_vec()), id);
        id
    }

    /// True only for a **genuine** module at `path` (see `ModuleNode::
    /// is_module`'s doc comment) — a struct/enum's own associated-item
    /// namespace at the same path (from `ensure_namespace`) doesn't count,
    /// even though a tree node exists there too.
    pub fn module_exists(&self, path: &QualifiedPath) -> bool {
        self.by_path
            .get(path)
            .is_some_and(|id| self.nodes[id.0 as usize].is_module)
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

    pub fn bind(&mut self, module: ModuleId, ns: Namespace, name: &str, entry: SymbolEntry) {
        self.nodes[module.0 as usize].bindings[ns as usize].insert(name.to_string(), entry);
    }

    pub fn lookup(&self, module: ModuleId, ns: Namespace, name: &str) -> Option<&SymbolEntry> {
        self.nodes[module.0 as usize].bindings[ns as usize].get(name)
    }

    /// Convenience for callers that only need the resolved target, not the
    /// visibility/canonical-path metadata (e.g. module-alias detection).
    pub fn lookup_res(&self, module: ModuleId, ns: Namespace, name: &str) -> Option<&Res> {
        self.lookup(module, ns, name).map(|entry| &entry.res)
    }

    /// Every binding in the tree in the given namespace, with its full
    /// qualified path — a tree walk replacing what used to be a flat
    /// `HashMap` iteration over `global_type_defs`/`global_value_defs`
    /// (used by `exported_symbols` and `load_default_prelude_defs`).
    /// Does not visit the reserved `prelude()` node — it holds no real
    /// qualified path of its own.
    pub fn all_bindings(
        &self,
        ns: Namespace,
    ) -> impl Iterator<Item = (QualifiedPath, &SymbolEntry)> {
        self.by_path.iter().flat_map(move |(path, id)| {
            self.nodes[id.0 as usize].bindings[ns as usize]
                .iter()
                .map(move |(name, entry)| (path.with_segment(name.clone()), entry))
        })
    }

    /// Every binding directly at `module` (not descendants) in namespace
    /// `ns` — lets a glob-import (`use some::module::*;`) expansion list a
    /// module's own value/type members with one `HashMap` iteration,
    /// instead of a flat scan over every global definition in the package
    /// filtered by qualified-path prefix.
    pub fn bindings(
        &self,
        module: ModuleId,
        ns: Namespace,
    ) -> impl Iterator<Item = (&str, &SymbolEntry)> {
        self.nodes[module.0 as usize].bindings[ns as usize]
            .iter()
            .map(|(name, entry)| (name.as_str(), entry))
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
        tree.bind(
            module,
            Namespace::Type,
            "Option",
            SymbolEntry {
                res: Res::SelfTy,
                export: SymbolExport::Public,
                path: None,
            },
        );
        assert!(matches!(
            tree.lookup(module, Namespace::Type, "Option"),
            Some(SymbolEntry {
                res: Res::SelfTy,
                ..
            })
        ));
        assert!(tree.lookup(module, Namespace::Value, "Option").is_none());
    }

    #[test]
    fn all_bindings_walks_every_module_with_full_path() {
        let mut tree = ModuleTree::new();
        let module = tree.ensure_module(&path(&["std", "core", "option"]));
        tree.bind(
            module,
            Namespace::Type,
            "Option",
            SymbolEntry {
                res: Res::SelfTy,
                export: SymbolExport::Public,
                path: None,
            },
        );
        let found: Vec<_> = tree.all_bindings(Namespace::Type).collect();
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].0, path(&["std", "core", "option", "Option"]));
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
