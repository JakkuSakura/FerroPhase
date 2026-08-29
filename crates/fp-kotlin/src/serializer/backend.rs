use super::*;

struct KotlinScan {
    ctx: KotlinWorkspaceContext,
    workspace_packages: HashSet<String>,
    /// Every package name in this workspace compile, sorted — used only
    /// by `write_workspace_files` for `settings.gradle.kts`'s
    /// `include(...)` lines.
    package_names: Vec<String>,
}

/// `TargetBackend` wrapper around [`KotlinSerializer`]. Kotlin needs
/// workspace-wide context beyond what `BackendConfig` carries — the
/// workspace-wide `KotlinScan` is read lazily from `&AstProgram` on
/// first `emit_package_artifact`/`write_workspace_files` call, same as every
/// other backend gets its input. `config.root_name` (the *source* project
/// directory's name, not `config.workspace_root`, the output directory)
/// is read straight off `self.config` — `AstProgram` has no way to
/// reconstruct it, it isn't package data at all.
pub struct KotlinBackend {
    serializer: KotlinSerializer,
    config: BackendConfig,
    scan: std::sync::OnceLock<KotlinScan>,
}

impl KotlinBackend {
    pub fn new(config: BackendConfig) -> Self {
        Self {
            serializer: KotlinSerializer,
            config,
            scan: std::sync::OnceLock::new(),
        }
    }

    /// Builds and caches the workspace-wide scan from `&AstProgram`
    /// on first call. Safe to call from any package's `emit_package_artifact` —
    /// including the very first — since `run_named_target`'s typecheck
    /// phase already ran for every package in the workspace before any
    /// `emit_package_artifact` call happens.
    fn ensure_scan(
        &self,
        workspace: &fp_core::ast::program::AstProgram,
    ) -> fp_core::error::Result<&KotlinScan> {
        if let Some(scan) = self.scan.get() {
            return Ok(scan);
        }
        // The provider may describe the entire Cargo workspace, while this
        // compile can intentionally select one member. Scan only packages
        // that the driver actually loaded and type-checked; asking for every
        // provider member would make a focused `--package` compile fail on
        // unrelated packages.
        let workspace_packages: HashSet<String> = workspace
            .crates()
            .keys()
            .map(|package_id| package_id.as_str().to_owned())
            .collect();
        let sources: Vec<AstPackage> = workspace_packages
            .iter()
            .map(|name| {
                workspace.package_source(&fp_core::ast::package::PackageId::new(name.clone()))
            })
            .collect::<fp_core::error::Result<_>>()?;
        let ctx = KotlinWorkspaceContext::collect(sources.iter());
        let mut package_names: Vec<String> = sources.iter().map(|s| s.name.clone()).collect();
        package_names.sort();
        let _ = self.scan.set(KotlinScan {
            ctx,
            workspace_packages,
            package_names,
        });
        Ok(self.scan.get().expect("just set above"))
    }
}

impl TargetBackend for KotlinBackend {
    fn capabilities(&self) -> fp_core::capabilities::LanguageCapabilities {
        crate::CAPABILITIES
    }

    fn emit_package_artifact(
        &self,
        workspace: &fp_core::ast::program::AstProgram,
        package_id: &fp_core::ast::package::PackageId,
        mir: &fp_core::mir::MirCodeUnit,
        lir: Option<&fp_core::lir::LirBlob>,
    ) -> fp_core::error::Result<()> {
        let scan = self.ensure_scan(workspace)?;
        // Materialize the central portable-operation representation into
        // Kotlin constructs before serialization. `package_source` derives
        // from this compiled package, so the materialized AST is what the
        // serializer consumes.
        {
            let compiled = workspace.compiled_package(package_id).ok_or_else(|| {
                fp_core::error::Error::from(format!(
                    "package `{package_id}` is unavailable for materialization"
                ))
            })?;
            let mut compiled = compiled.borrow_mut();
            for pkg_item in &mut compiled.items {
                pkg_item.item = fp_core::intrinsics::materialize_item(
                    pkg_item.item.clone(),
                    &crate::KotlinMaterializer,
                )?;
            }
        }
        let package = workspace.package_source(package_id)?;
        let package = &package;
        let files =
            self.serializer
                .serialize_package(package, &scan.workspace_packages, &scan.ctx)?;
        let writer = PackageWriter::new(self.config.workspace_root.join(&package.name));
        for (mod_path, code) in files {
            let rel = if mod_path.contains('.') {
                mod_path
            } else {
                format!("{}.kt", mod_path)
            };
            writer.write_file(&rel, code)?;
        }
        Ok(())
    }

    fn write_workspace_files(
        &self,
        workspace: &fp_core::ast::program::AstProgram,
    ) -> fp_core::error::Result<()> {
        let scan = self.ensure_scan(workspace)?;
        let root_name = self.config.root_name.replace('-', "_");
        let settings = format!(
            "rootProject.name = \"{root_name}\"\n\n{}\n",
            scan.package_names
                .iter()
                .map(|n| format!("include(\":{}\")", n))
                .collect::<Vec<_>>()
                .join("\n")
        );
        let writer = PackageWriter::new(self.config.workspace_root.clone());
        writer.write_file("settings.gradle.kts", settings)?;
        writer.write_file(
            "build.gradle.kts",
            "plugins {\n    kotlin(\"jvm\") version \"2.1.0\" apply false\n}\n\n\
             allprojects {\n    repositories { mavenCentral() }\n}\n",
        )?;
        Ok(())
    }
}
