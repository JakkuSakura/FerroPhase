mod arch;
pub(crate) mod coff;
pub(crate) mod elf;
pub(crate) mod macho;

use crate::link;
use fp_core::error::{Error, Result};
use fp_core::lir::{
    CallingConvention, LirBasicBlock, LirConstant, LirFunction, LirFunctionSignature, LirProgram,
    LirTerminator, LirType, LirValue, Linkage, Name,
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

pub fn emit_object_minimal(path: &Path, format: TargetFormat, arch: TargetArch) -> Result<()> {
    let plan = emit_plan_minimal(&default_lir_program(), format, arch)?;
    write_object_minimal(path, &plan)
}

pub fn emit_executable_minimal(path: &Path, format: TargetFormat, arch: TargetArch) -> Result<()> {
    let plan = emit_plan_minimal(&default_lir_program(), format, arch)?;
    write_executable_minimal(path, &plan)
}

pub struct EmitPlan {
    pub format: TargetFormat,
    pub arch: TargetArch,
    pub text: Vec<u8>,
}

pub fn emit_plan_minimal(
    lir_program: &LirProgram,
    format: TargetFormat,
    arch: TargetArch,
) -> Result<EmitPlan> {
    let exit_code = derive_exit_code(lir_program)?;
    let text = arch::emit_entry_text(arch, format, exit_code)?;
    Ok(EmitPlan { format, arch, text })
}

pub fn write_object_minimal(path: &Path, plan: &EmitPlan) -> Result<()> {
    match plan.format {
        TargetFormat::MachO => macho::emit_object_macho_minimal(path, plan.arch, &plan.text),
        TargetFormat::Elf => elf::emit_object_elf64_minimal(path, plan.arch, &plan.text),
        TargetFormat::Coff => coff::emit_object_coff_minimal(path, plan.arch, &plan.text),
    }
}

pub fn write_executable_minimal(path: &Path, plan: &EmitPlan) -> Result<()> {
    link::link_executable_minimal(path, plan.format, plan.arch, &plan.text)
}

fn derive_exit_code(lir_program: &LirProgram) -> Result<u32> {
    let func = lir_program
        .functions
        .first()
        .ok_or_else(|| Error::from("no functions in LIR program"))?;

    let block = func
        .basic_blocks
        .first()
        .ok_or_else(|| Error::from("function has no basic blocks"))?;

    if !block.instructions.is_empty() {
        return Err(Error::from(
            "native emitter only supports empty basic blocks in this stage",
        ));
    }

    match &block.terminator {
        LirTerminator::Return(None) => Ok(0),
        LirTerminator::Return(Some(value)) => match value {
            LirValue::Constant(LirConstant::Int(value, _)) => {
                Ok(u32::try_from(*value).unwrap_or(0))
            }
            LirValue::Constant(LirConstant::UInt(value, _)) => Ok(u32::try_from(*value).unwrap_or(0)),
            LirValue::Constant(LirConstant::Bool(value)) => Ok(if *value { 1 } else { 0 }),
            _ => Err(Error::from(
                "native emitter only supports integer/bool return constants",
            )),
        },
        other => Err(Error::from(format!(
            "native emitter only supports Return terminators, got {other:?}"
        ))),
    }
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
