use super::*;

impl<'ctx> AstInterpreter<'ctx> {
    #[inline]
    pub(super) fn enter_const_region(&mut self) {
        self.in_const_region += 1;
    }

    #[inline]
    pub(super) fn exit_const_region(&mut self) {
        self.in_const_region = self.in_const_region.saturating_sub(1);
    }

    #[inline]
    pub(super) fn in_const_region(&self) -> bool {
        self.in_const_region > 0
    }
}

