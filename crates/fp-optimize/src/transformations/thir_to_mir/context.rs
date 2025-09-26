use super::*;

impl MirGenerator {
    pub(super) fn reset_for_new_function(&mut self) {
        self.next_local_id = 0;
        self.next_bb_id = 0;
        self.current_locals.clear();
        self.current_blocks.clear();
        self.local_decls.clear();
        self.loop_stack.clear();
        self.current_return_local = None;
    }

    pub(super) fn create_local(&mut self, ty: Ty) -> mir::LocalId {
        let local_id = self.next_local_id;
        self.next_local_id += 1;
        self.local_decls.push(mir::LocalDecl {
            mutability: mir::Mutability::Not,
            local_info: mir::LocalInfo::Other,
            internal: false,
            is_block_tail: None,
            ty,
            user_ty: None,
            source_info: Span::new(0, 0, 0),
        });
        local_id
    }

    pub(super) fn create_local_from_thir(&mut self, ty: &hir_types::Ty) -> mir::LocalId {
        let mir_ty = self.transform_type(ty);
        self.create_local(mir_ty)
    }

    pub(super) fn get_or_create_local(&mut self, thir_id: thir::ThirId, ty: Ty) -> mir::LocalId {
        if let Some(&local) = self.current_locals.get(&thir_id) {
            local
        } else {
            let local = self.create_local(ty);
            self.current_locals.insert(thir_id, local);
            local
        }
    }

    pub(super) fn get_or_create_local_from_thir(
        &mut self,
        thir_id: thir::ThirId,
        ty: &hir_types::Ty,
    ) -> mir::LocalId {
        if let Some(&local) = self.current_locals.get(&thir_id) {
            local
        } else {
            let local = self.create_local_from_thir(ty);
            self.current_locals.insert(thir_id, local);
            local
        }
    }

    pub(super) fn create_basic_block(&mut self) -> mir::BasicBlockId {
        let bb_id = self.next_bb_id;
        self.next_bb_id += 1;

        self.current_blocks.push(mir::BasicBlockData {
            statements: Vec::new(),
            terminator: None,
            is_cleanup: false,
        });

        bb_id
    }

    pub(super) fn add_statement_to_block(
        &mut self,
        bb_id: mir::BasicBlockId,
        stmt: mir::Statement,
    ) {
        if let Some(block) = self.current_blocks.get_mut(bb_id as usize) {
            block.statements.push(stmt);
        }
    }

    pub(super) fn set_block_terminator(
        &mut self,
        bb_id: mir::BasicBlockId,
        terminator: mir::Terminator,
    ) {
        if let Some(block) = self.current_blocks.get_mut(bb_id as usize) {
            block.terminator = Some(terminator);
        }
    }

    pub(super) fn block_has_terminator(&self, bb_id: mir::BasicBlockId) -> bool {
        self.current_blocks
            .get(bb_id as usize)
            .and_then(|block| block.terminator.as_ref())
            .is_some()
    }

    pub(super) fn ensure_goto(
        &mut self,
        bb_id: mir::BasicBlockId,
        target: mir::BasicBlockId,
        span: Span,
    ) {
        if !self.block_has_terminator(bb_id) {
            self.set_block_terminator(
                bb_id,
                mir::Terminator {
                    kind: mir::TerminatorKind::Goto { target },
                    source_info: span,
                },
            );
        }
    }

    pub(super) fn next_id(&mut self) -> mir::MirId {
        let id = self.next_mir_id;
        self.next_mir_id += 1;
        id
    }
}
