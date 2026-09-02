use std::collections::VecDeque;

use crate::package::ImportDirective;

#[derive(Debug, Default)]
pub struct ResolutionWorklist {
    pub(crate) queue: VecDeque<ImportDirective>,
}

impl ResolutionWorklist {
    pub fn push(&mut self, directive: ImportDirective) {
        self.queue.push_back(directive);
    }

    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }

    pub fn len(&self) -> usize {
        self.queue.len()
    }
}
