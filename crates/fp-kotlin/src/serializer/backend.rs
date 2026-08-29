use super::*;

struct KotlinScan {
    ctx: KotlinWorkspaceContext,
    workspace_packages: HashSet<String>,
    /// Every package selected for Kotlin emission, sorted — used by
    /// `write_workspace_files` for `settings.gradle.kts`'s `include(...)`
    /// lines.
    package_names: Vec<String>,
    kotlin_packages: HashMap<String, String>,
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
    output_initialized: std::sync::OnceLock<()>,
}

impl KotlinBackend {
    pub fn new(config: BackendConfig) -> Self {
        Self {
            serializer: KotlinSerializer,
            config,
            scan: std::sync::OnceLock::new(),
            output_initialized: std::sync::OnceLock::new(),
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
        // The compiler loads dependencies for resolution, but only selected
        // roots become Kotlin projects. Keeping this set separate prevents
        // Rust sysroot crates such as `core` and `libc` from becoming Gradle
        // project dependencies without emitted Kotlin artifacts.
        let workspace_packages: HashSet<String> = self
            .config
            .emitted_packages
            .iter()
            .map(|package_id| package_id.as_str().to_owned())
            .collect();
        let kotlin_packages = workspace_packages
            .iter()
            .flat_map(|name| {
                let package =
                    kotlin_package_name(self.config.kotlin_package_prefix.as_deref(), name);
                [
                    (name.clone(), package.clone()),
                    (name.replace('-', "_"), package),
                ]
            })
            .collect::<HashMap<_, _>>();
        let sources: Vec<AstPackage> = workspace_packages
            .iter()
            .map(|name| workspace.package_source(&fp_core::ast::package::PackageId::new(name)))
            .collect::<fp_core::error::Result<_>>()?;
        let ctx = KotlinWorkspaceContext::collect(sources.iter());
        let mut package_names: Vec<String> = sources.iter().map(|s| s.name.clone()).collect();
        package_names.sort();
        let _ = self.scan.set(KotlinScan {
            ctx,
            workspace_packages,
            package_names,
            kotlin_packages,
        });
        Ok(self.scan.get().expect("just set above"))
    }

    fn initialize_output(&self) -> fp_core::error::Result<()> {
        if self.output_initialized.get().is_none() {
            if self.config.workspace_root.exists() {
                std::fs::remove_dir_all(&self.config.workspace_root)?;
            }
            self.output_initialized
                .set(())
                .map_err(|_| fp_core::error::Error::from("Kotlin output initialization raced"))?;
        }
        Ok(())
    }
}

fn kotlin_package_name(prefix: Option<&str>, package: &str) -> String {
    let crate_name = package.replace('-', "_");
    match prefix
        .map(|prefix| prefix.trim_end_matches('.'))
        .filter(|prefix| !prefix.is_empty())
    {
        Some(prefix) => format!("{prefix}.{crate_name}"),
        None => crate_name,
    }
}

#[cfg(test)]
mod tests {
    use super::kotlin_package_name;

    #[test]
    fn kotlin_package_name_normalizes_cargo_names_under_prefix() {
        assert_eq!(
            kotlin_package_name(Some("com.example.generated"), "skln-git"),
            "com.example.generated.skln_git"
        );
    }

    #[test]
    fn kotlin_package_name_has_no_leading_dot_without_prefix() {
        assert_eq!(kotlin_package_name(None, "skln-git"), "skln_git");
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
        self.initialize_output()?;
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
        let files = self.serializer.serialize_package(
            package,
            &scan.workspace_packages,
            &scan.kotlin_packages,
            &scan.ctx,
        )?;
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
