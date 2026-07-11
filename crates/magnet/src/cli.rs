use std::path::Path;

use eyre::Result;

use crate::commands::{
    self, BenchOptions, BuildOptions, LockOptions, RunOptions, TestOptions, UpdateOptions,
    export::ExportOptions, generate::GenerateOptions,
};
use crate::models::ManifestModel;
use crate::resolver::project::load_manifest;

#[derive(Debug, Default, Clone, Copy)]
pub struct MagnetCli;

impl MagnetCli {
    pub fn new() -> Self {
        Self
    }

    pub fn load_manifest(&self, path: &Path) -> Result<ManifestModel> {
        load_manifest(path)
    }

    pub fn package_name(&self, path: &Path) -> Result<String> {
        Ok(self.load_manifest(path)?.name())
    }

    pub fn init(&self, path: &Path, from_cargo: bool) -> Result<()> {
        commands::init(path, from_cargo)
    }

    pub fn generate(&self, options: &GenerateOptions) -> Result<()> {
        commands::generate(options)
    }

    pub fn check(&self, config_path: &Path) -> Result<()> {
        commands::check(config_path)
    }

    pub fn tree(&self, config_path: &Path) -> Result<()> {
        commands::tree(config_path)
    }

    pub fn graph(&self, config_path: &Path, output_path: Option<&Path>) -> Result<()> {
        commands::graph(config_path, output_path)
    }

    pub fn lock(&self, options: &LockOptions) -> Result<()> {
        commands::lock(options)
    }

    pub fn update(&self, options: &UpdateOptions) -> Result<()> {
        commands::update(options)
    }

    pub fn export(&self, options: &ExportOptions) -> Result<()> {
        commands::export(options)
    }

    pub fn run(&self, options: &RunOptions) -> Result<()> {
        commands::run(options)
    }

    pub fn build(&self, options: &BuildOptions) -> Result<()> {
        commands::build(options)
    }

    pub fn test(&self, options: &TestOptions) -> Result<()> {
        commands::test(options)
    }

    pub fn bench(&self, options: &BenchOptions) -> Result<()> {
        commands::bench(options)
    }

    pub fn submodule_init(&self, path: &Path) -> Result<()> {
        commands::submodule_init(path)
    }

    pub fn submodule_update(&self, path: &Path, remote: bool) -> Result<()> {
        commands::submodule_update(path, remote)
    }

    pub fn submodule_deinit(&self, path: &Path, submodule_path: &Path) -> Result<()> {
        commands::submodule_deinit(path, submodule_path)
    }

    pub fn submodule_list(&self, path: &Path) -> Result<()> {
        commands::submodule_list(path)
    }

    pub fn submodule_switch(&self, path: &Path, rev: &str) -> Result<()> {
        commands::submodule_switch(path, rev)
    }
}
