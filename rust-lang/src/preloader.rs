use crate::RustSerde;
use common::*;
use common_lang::ast::{Def, Ident};
use std::collections::HashMap;

/// a preloader is used to parse rust files and extract
/// runtime relfection information like struct definition, function exportation, and var types
pub struct RustPreloader {
    parser: RustSerde,
    mappings: HashMap<Ident, Def>,
}
impl RustPreloader {
    pub fn preload_file(&mut self, content: syn::File) -> Result<()> {
        let m = self.parser.deserialize_file(content)?;
        for b in m.stmts {
            if let Some(def) = b.as_ast::<Def>() {
                self.mappings.insert(def.name.clone(), def);
            }
        }
        Ok(())
    }
}
