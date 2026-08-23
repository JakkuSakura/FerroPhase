use std::collections::HashMap;

use super::{Body, BodyId, Item};

#[derive(Debug, Clone, PartialEq)]
pub struct MirProgram {
    pub items: Vec<Item>,
    pub bodies: HashMap<BodyId, Body>,
}

impl MirProgram {
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
