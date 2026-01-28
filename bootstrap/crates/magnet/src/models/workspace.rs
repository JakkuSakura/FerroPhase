use std::path::PathBuf;

use crate::configs::WorkspaceConfig;
use crate::models::PackageModel;

#[derive(Debug, Clone)]
pub struct WorkspaceModel {
    pub root_path: PathBuf,
    pub manifest_path: PathBuf,
    pub workspace: WorkspaceConfig,
    pub packages: Vec<PackageModel>,
}
