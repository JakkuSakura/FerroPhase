use crate::ast::{Ident, Visibility};
use crate::{common_enum, common_struct};
use std::fmt::{Display, Formatter};

common_struct! {
    pub struct ItemImport {
        pub visibility: Visibility,
        pub tree: ItemImportTree,
    }
}

common_enum! {
    pub enum ItemImportTree {
        /// :: prefix in rust
        Root,
        SelfMod,
        SuperMod,
        Crate,

        /// a single identifier
        Ident(Ident),
        Rename(ItemImportRename),
        Path(ItemImportPath),
        Group(ItemImportGroup),
        /// Wildcard import
        Glob,
    }
}
common_struct! {
    pub struct ItemImportRename {
        pub from: Ident,
        pub to: Ident,
    }
}
impl Display for ItemImportRename {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} as {}", self.from, self.to)
    }
}
common_struct! {
    pub struct ItemImportPath {
        pub segments: Vec<ItemImportTree>,
    }
}
impl Display for ItemImportPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for seg in &self.segments {
            if first {
                first = false;
            } else if matches!(seg, ItemImportTree::Root) {
                first = false;
                continue;
            } else {
                f.write_str("::")?;
            }
            seg.fmt(f)?;
        }
        Ok(())
    }
}
impl ItemImportPath {
    pub fn new() -> Self {
        ItemImportPath {
            segments: Vec::new(),
        }
    }
    pub fn push(&mut self, seg: ItemImportTree) {
        self.segments.push(seg);
    }
    pub fn extend(&mut self, other: ItemImportPath) {
        self.segments.extend(other.segments);
    }

    pub fn validate(&self, depth: usize) -> bool {
        if self.segments.is_empty() {
            return false;
        }
        let mut has_glob = false;
        let mut has_group = false;
        for (i, node) in self.segments.iter().enumerate() {
            if !node.validate(depth + i) {
                return false;
            }
            match node {
                ItemImportTree::Glob => {
                    if has_glob || has_group {
                        return false;
                    }
                    has_glob = true;
                }
                ItemImportTree::Group(_) => {
                    if has_group || has_glob {
                        return false;
                    }
                    has_group = true;
                }
                _ => {}
            }
        }
        true
    }
}
common_struct! {
    pub struct ItemImportGroup {
        pub items: Vec<ItemImportTree>,
    }
}
impl Display for ItemImportGroup {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("{")?;
        let mut first = true;
        for seg in &self.items {
            if first {
                first = false;
            } else {
                f.write_str(", ")?;
            }
            seg.fmt(f)?;
        }
        f.write_str("}")
    }
}
impl ItemImportGroup {
    pub fn new() -> Self {
        ItemImportGroup { items: Vec::new() }
    }
    pub fn push(&mut self, seg: ItemImportTree) {
        self.items.push(seg);
    }
    pub fn validate(&self, depth: usize) -> bool {
        if self.items.is_empty() {
            return false;
        }
        let mut has_glob = false;
        for (i, node) in self.items.iter().enumerate() {
            if !node.validate(depth + i) {
                return false;
            }
            match node {
                ItemImportTree::Glob => {
                    if has_glob {
                        return false;
                    }
                    has_glob = true;
                }
                _ => {}
            }
        }
        true
    }
}
impl Display for ItemImportTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ItemImportTree::Root => f.write_str("::"),
            ItemImportTree::SelfMod => f.write_str("self"),
            ItemImportTree::SuperMod => f.write_str("super"),
            ItemImportTree::Crate => f.write_str("crate"),
            ItemImportTree::Ident(ident) => ident.fmt(f),
            ItemImportTree::Rename(rename) => rename.fmt(f),
            ItemImportTree::Path(path) => path.fmt(f),
            ItemImportTree::Group(group) => group.fmt(f),
            ItemImportTree::Glob => f.write_str("*"),
        }
    }
}
impl ItemImportTree {
    pub fn new_path() -> Self {
        ItemImportTree::Path(ItemImportPath {
            segments: Vec::new(),
        })
    }
    pub fn into_path(self) -> ItemImportPath {
        match self {
            ItemImportTree::Path(path) => path,
            node => ItemImportPath {
                segments: vec![node],
            },
        }
    }
    /// verify that Root, self, super, crate only appear once at first place
    ///
    /// wildcard and group only appear once at last place
    pub fn validate(&self, depth: usize) -> bool {
        match self {
            Self::Root | Self::SelfMod | Self::SuperMod | Self::Crate => depth == 0,
            Self::Ident(_) | Self::Rename(_) | Self::Glob => true,
            Self::Path(nodes) => nodes.validate(depth),
            Self::Group(nodes) => nodes.validate(depth),
        }
    }
}
