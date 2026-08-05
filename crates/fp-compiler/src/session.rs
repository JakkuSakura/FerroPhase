use std::rc::Rc;
use std::sync::Arc;

use fp_core::lir::LirDataLayout;
use fp_core::package::provider::PackageProvider;
use fp_core::workspace::WorkspaceContext;

use crate::{CompilerDriver, CompilerExecutor};

/// Owns the executor, provider registry, and driver for one compilation
/// session. Package workspaces created by the driver share this session's
/// provider registry but keep package state isolated.
pub struct CompilerSession {
    workspace: Rc<WorkspaceContext>,
    driver: CompilerDriver,
}

impl CompilerSession {
    pub fn new(
        data_layout: LirDataLayout,
        executor: &CompilerExecutor,
        workspace: Rc<WorkspaceContext>,
    ) -> Self {
        let driver =
            CompilerDriver::with_workspace(data_layout, executor.handle(), workspace.clone());
        Self { workspace, driver }
    }

    pub fn register_provider(&self, provider: Arc<dyn PackageProvider>) {
        self.workspace.register_provider(provider);
    }

    pub fn workspace(&self) -> Rc<WorkspaceContext> {
        self.workspace.clone()
    }

    pub fn driver(&mut self) -> &mut CompilerDriver {
        &mut self.driver
    }

    pub fn into_driver(self) -> CompilerDriver {
        self.driver
    }
}
