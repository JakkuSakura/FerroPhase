use std::collections::HashMap;
use std::sync::Arc;

use super::LanguageFrontend;

/// Registry that maps language identifiers and extensions to frontend
/// implementations.
#[derive(Default)]
pub struct FrontendRegistry {
    lookup: HashMap<String, Arc<dyn LanguageFrontend>>, // normalised key -> frontend
}

impl FrontendRegistry {
    pub fn new() -> Self {
        Self {
            lookup: HashMap::new(),
        }
    }

    pub fn register(&mut self, frontend: Arc<dyn LanguageFrontend>) {
        let key = frontend.language().to_lowercase();
        self.lookup.insert(key, frontend.clone());
        for ext in frontend.extensions() {
            self.lookup.insert(ext.to_lowercase(), frontend.clone());
        }
    }

    pub fn get(&self, key: &str) -> Option<Arc<dyn LanguageFrontend>> {
        self.lookup.get(&key.to_lowercase()).cloned()
    }

    pub fn merge(&mut self, other: &FrontendRegistry) {
        for (key, frontend) in &other.lookup {
            self.lookup.insert(key.clone(), frontend.clone());
        }
    }
}
