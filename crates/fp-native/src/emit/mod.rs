mod codegen;

use crate::link;
use fp_core::error::{Error, Result};
use fp_core::lir::{
    CallingConvention, LirBasicBlock, LirFunction, LirFunctionSignature, LirProgram, LirTerminator,
    LirType, Linkage, Name,
};
use std::path::Path;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetFormat {
    MachO,
    Elf,
    Coff,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetArch {
    X86_64,
    Aarch64,
}

pub fn detect_target(triple: Option<&str>) -> Result<(TargetFormat, TargetArch)> {
    let triple = triple.map(|t| t.to_ascii_lowercase());

    let format = match triple.as_deref() {
        Some(triple)
            if triple.contains("apple")
                || triple.contains("darwin")
                || triple.contains("macos")
                || triple.contains("ios") =>
        {
            TargetFormat::MachO
        }
        Some(triple) if triple.contains("windows") || triple.contains("msvc") || triple.contains("mingw") => {
            TargetFormat::Coff
        }
        _ => TargetFormat::Elf,
    };

    let arch = match triple.as_deref() {
        Some(triple) if triple.contains("x86_64") || triple.contains("amd64") => TargetArch::X86_64,
        Some(triple) if triple.contains("aarch64") || triple.contains("arm64") => TargetArch::Aarch64,
        _ => {
            if cfg!(target_arch = "x86_64") {
                TargetArch::X86_64
            } else if cfg!(target_arch = "aarch64") {
                TargetArch::Aarch64
            } else {
                return Err(Error::from("unsupported target architecture"));
            }
        }
    };

    Ok((format, arch))
}

pub fn emit_object(path: &Path, format: TargetFormat, arch: TargetArch) -> Result<()> {
    let plan = emit_plan(&default_lir_program(), format, arch)?;
    write_object(path, &plan)
}

pub fn emit_executable(path: &Path, format: TargetFormat, arch: TargetArch) -> Result<()> {
    let plan = emit_plan(&default_lir_program(), format, arch)?;
    write_executable(path, &plan)
}

pub struct EmitPlan {
    pub format: TargetFormat,
    pub arch: TargetArch,
    pub text: Vec<u8>,
}

pub fn emit_plan(
    lir_program: &LirProgram,
    format: TargetFormat,
    arch: TargetArch,
) -> Result<EmitPlan> {
    let text = codegen::emit_text_from_lir(lir_program, format, arch)?;
    Ok(EmitPlan { format, arch, text })
}

pub fn write_object(path: &Path, plan: &EmitPlan) -> Result<()> {
    match plan.format {
        TargetFormat::MachO => link::macho::emit_object_macho(path, plan.arch, &plan.text),
        TargetFormat::Elf => link::elf::emit_object_elf64(path, plan.arch, &plan.text),
        TargetFormat::Coff => link::coff::emit_object_coff(path, plan.arch, &plan.text),
    }
}

pub fn write_executable(path: &Path, plan: &EmitPlan) -> Result<()> {
    link::link_executable(path, plan.format, plan.arch, &plan.text)
}

fn default_lir_program() -> LirProgram {
    let func = LirFunction {
        name: Name::new("main"),
        signature: LirFunctionSignature {
            params: Vec::new(),
            return_type: LirType::I32,
            is_variadic: false,
        },
        basic_blocks: vec![LirBasicBlock {
            id: 0,
            label: Some(Name::new("entry")),
            instructions: Vec::new(),
            terminator: LirTerminator::Return(None),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }],
        locals: Vec::new(),
        stack_slots: Vec::new(),
        calling_convention: CallingConvention::C,
        linkage: Linkage::External,
    };

    LirProgram {
        functions: vec![func],
        globals: Vec::new(),
        type_definitions: Vec::new(),
    }
}
