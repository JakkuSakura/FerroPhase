use super::super::*;

impl Pipeline {
    pub(crate) fn stage_emit_ebpf(
        &self,
        lir_program: &fp_core::lir::LirProgram,
        base_path: &Path,
        options: &PipelineOptions,
    ) -> Result<String, CliError> {
        let wants_object = base_path.extension().and_then(|ext| ext.to_str()) == Some("o");

        if wants_object {
            fp_ebpf::write_object(base_path, lir_program)
                .map_err(|err| CliError::Compilation(format!("eBPF object emit failed: {}", err)))?;
            if options.save_intermediates {
                let asm_path = base_path.with_extension("ebpf");
                let asm = fp_ebpf::emit_assembly(lir_program).map_err(|err| {
                    CliError::Compilation(format!("eBPF text emit failed: {}", err))
                })?;
                std::fs::write(&asm_path, asm).map_err(CliError::Io)?;
            }
            Ok(String::new())
        } else {
            fp_ebpf::emit_assembly(lir_program)
                .map_err(|err| CliError::Compilation(format!("eBPF emit failed: {}", err)))
        }
    }
}
