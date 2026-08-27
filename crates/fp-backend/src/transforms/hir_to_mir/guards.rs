use fp_core::hir;
use std::collections::HashSet;

pub(super) struct ExprRecursionGuard {
    set: *mut HashSet<hir::HirId>,
    id: hir::HirId,
}

pub(super) struct PlaceDepthGuard {
    depth: *mut usize,
}

impl PlaceDepthGuard {
    pub(super) fn new(depth: &mut usize) -> Self {
        *depth += 1;
        Self { depth }
    }
}

impl Drop for PlaceDepthGuard {
    fn drop(&mut self) {
        unsafe {
            *self.depth -= 1;
        }
    }
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
