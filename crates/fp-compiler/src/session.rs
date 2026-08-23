use std::rc::Rc;

use fp_core::lir::LirDataLayout;
use fp_core::ast::program::AstProgram;

use crate::{CompilerDriver, CompilerExecutor};

/// Owns the executor, driver, and workspace (with its one required
/// provider, set at construction) for one compilation session. Package
/// workspaces created by the driver share this session's workspace but
/// keep package state isolated.
pub struct CompilerSession {
    workspace: Rc<AstProgram>,
    driver: CompilerDriver,
}

impl CompilerSession {
    pub fn new(
        data_layout: LirDataLayout,
        executor: &CompilerExecutor,
        workspace: Rc<AstProgram>,
    ) -> Self {
        let driver =
            CompilerDriver::with_workspace(data_layout, executor.handle(), workspace.clone());
        Self { workspace, driver }
    }

    pub fn workspace(&self) -> Rc<AstProgram> {
        self.workspace.clone()
    }

    pub fn driver(&mut self) -> &mut CompilerDriver {
        &mut self.driver
    }

    pub fn into_driver(self) -> CompilerDriver {
        self.driver
    }
}
