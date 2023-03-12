use crate::ast::*;
use common::*;
use std::collections::HashMap;

/// a preloader is used to parse rust files and extract
/// runtime relfection information like struct definition, function exportation, and var types
#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct Preloader {
    mappings: HashMap<String, Def>,
}
impl Preloader {
    pub fn new() -> Self {
        Self {
            mappings: Default::default(),
        }
    }
    pub fn lookup_def(&self, ident: Ident) -> Option<&Def> {
        self.mappings.get(ident.as_str())
    }
    pub fn preload_file(&mut self, m: &Module) -> Result<()> {
        for b in &m.stmts {
            if let Some(def) = b.as_ast::<Def>() {
                self.mappings.insert(def.name.name.clone(), def.clone());
            }
        }
        Ok(())
    }
}
