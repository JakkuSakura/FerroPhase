use fp_core::hir;
use std::collections::HashSet;

pub(super) struct ExprRecursionGuard {
    set: *mut HashSet<hir::HirId>,
    id: hir::HirId,
}

impl ExprRecursionGuard {
    pub(super) fn new(set: &mut HashSet<hir::HirId>, id: hir::HirId) -> Self {
        Self {
            set: set as *mut HashSet<hir::HirId>,
            id,
        }
    }
}

impl Drop for ExprRecursionGuard {
    fn drop(&mut self) {
        unsafe {
            (*self.set).remove(&self.id);
        }
    }
}
