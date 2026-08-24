use std::collections::HashMap;

use super::{Body, BodyId, Item};

/// One `hir::DefId`'s worth of lowered MIR content — usually one item plus
/// its one body, occasionally more when lowering that item pulled in
/// something it directly references (e.g. a synthetic comptime probe's
/// item, or a nested item discovered along the way). Distinct from
/// a whole `MirPackage`'s combined content: a `MirCodeUnit` is
/// deliberately partial and keyed by the `DefId` that produced it (see
/// `MirPackage::units`), so re-lowering one item after a comptime value
/// resolves means replacing its one unit, not rebuilding the whole
/// package's content.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct MirCodeUnit {
    pub items: Vec<Item>,
    pub bodies: HashMap<BodyId, Body>,
}

impl MirCodeUnit {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            bodies: HashMap::new(),
        }
    }
}
