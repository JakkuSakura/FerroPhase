use std::collections::HashMap;

use super::{Body, BodyId, Item};

/// One package's flat, lowered MIR content — every item and body produced
/// for it, with no further structure. This is what `HirToMirLowerer`'s whole-
/// package entry points build, and what every MIR-consuming backend
/// (`MirToLirLowerer`, `fp-bytecode`, `fp-jvm`, `fp-cil`, ...) actually reads;
/// `mir::MirProgram` is the higher-level, multi-package/`DefId`-partitioned
/// structure `CompilerState` accumulates lowering results into, of which a
/// `MirModule` is one flattened view (see `MirPackage::flatten`).
#[derive(Debug, Clone, PartialEq)]
pub struct MirModule {
    pub items: Vec<Item>,
    pub bodies: HashMap<BodyId, Body>,
}

impl MirModule {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            bodies: HashMap::new(),
        }
    }

    pub fn span(&self) -> super::Span {
        super::Span::union(self.items.iter().map(Item::span))
    }
}

impl Default for MirModule {
    fn default() -> Self {
        Self::new()
    }
}
