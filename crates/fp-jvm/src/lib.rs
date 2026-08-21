mod classfile;
mod error;
mod jar;
mod jir;
mod lower;
mod parse;

pub use classfile::{EmittedClass, emit_class_files};
pub use error::JvmError;
pub use jar::{emit_executable_jar, extract_class_files_from_jar};
pub use jir::{JvmClass, JvmCode, JvmInstr, JvmMethod, JvmProgram};
pub use lower::{JvmBackendOptions, derive_class_name, lower_program};
pub use parse::parse_class_to_lir;

/// `TargetBackend` for the `--target jvm-bytecode` target — reads a
/// package's already-compiled MIR straight off the shared workspace's
/// `CompiledPackage`, same source `CompilerDriver::compile_bytecode`-style
/// helpers used, just without re-driving a second compile.
pub struct JvmBackend {
    pub output: std::path::PathBuf,
    pub save_intermediates: bool,
}

impl fp_core::backend::TargetBackend for JvmBackend {
    fn compile_package(
        &self,
        workspace: &fp_core::workspace::WorkspaceContext,
        package_id: &fp_core::package::PackageId,
    ) -> fp_core::error::Result<()> {
        let package = workspace.compiled_package(package_id).ok_or_else(|| {
            fp_core::error::Error::from(format!("package `{package_id}` is unavailable"))
        })?;
        let mir = package.borrow().mir_program.clone().ok_or_else(|| {
            fp_core::error::Error::from(format!("package `{package_id}` has no MIR program"))
        })?;

        let class_stem = package_id.as_str();
        let jvm_options = JvmBackendOptions {
            class_name: derive_class_name(class_stem),
            emit_java_entrypoint: true,
        };
        let program = lower_program(&mir, &jvm_options)
            .map_err(|e| fp_core::error::Error::from(format!("MIR→JVM lowering failed: {e}")))?;
        let mut classes = emit_class_files(&program)
            .map_err(|e| fp_core::error::Error::from(format!("JVM class emission failed: {e}")))?;
        if classes.len() != 1 {
            return Err(fp_core::error::Error::from(
                "JVM backend currently expects exactly one emitted class",
            ));
        }
        let class = classes.remove(0);

        let wants_jar = self.output.extension().and_then(|ext| ext.to_str()) == Some("jar");
        let output_path = if wants_jar {
            self.output.clone()
        } else {
            self.output.with_extension("class")
        };
        if let Some(parent) = output_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let class_bytes = class.bytes.clone();
        let class_path = if wants_jar {
            output_path.with_extension("class")
        } else {
            output_path.clone()
        };

        if wants_jar {
            if self.save_intermediates {
                std::fs::write(&class_path, &class_bytes)?;
            }
            let jar = emit_executable_jar(&[class], &program.class.name)
                .map_err(|e| fp_core::error::Error::from(format!("JAR packaging failed: {e}")))?;
            std::fs::write(&output_path, jar)?;
        } else {
            std::fs::write(&output_path, class_bytes)?;
        }

        Ok(())
    }
}
