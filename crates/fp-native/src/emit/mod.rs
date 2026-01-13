mod aarch64;
mod codegen;
mod x86_64;

use crate::link;
use fp_core::error::{Error, Result};
use fp_core::lir::{
    CallingConvention, LirBasicBlock, LirFunction, LirFunctionSignature, LirProgram, LirTerminator,
    LirType, Linkage, Name,
};
use std::fmt::Write;
use std::fs;
use std::path::Path;
use std::collections::HashMap;

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
        Some(_) => TargetFormat::Elf,
        None => {
            if cfg!(target_os = "windows") {
                TargetFormat::Coff
            } else if cfg!(any(target_os = "macos", target_os = "ios")) {
                TargetFormat::MachO
            } else {
                TargetFormat::Elf
            }
        }
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
    pub rodata: Vec<u8>,
    pub relocs: Vec<Relocation>,
    pub symbols: HashMap<String, u64>,
    pub entry_offset: u64,
}

pub struct CodegenOutput {
    pub text: Vec<u8>,
    pub rodata: Vec<u8>,
    pub relocs: Vec<Relocation>,
    pub symbols: HashMap<String, u64>,
    pub entry_offset: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RelocKind {
    CallRel32,
    Abs64,
    Aarch64AdrpAdd,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RelocSection {
    Text,
    Rdata,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Relocation {
    pub offset: u64,
    pub kind: RelocKind,
    pub section: RelocSection,
    pub symbol: String,
    pub addend: i64,
}

pub fn emit_plan(
    lir_program: &LirProgram,
    format: TargetFormat,
    arch: TargetArch,
) -> Result<EmitPlan> {
    let output = codegen::emit_text_from_lir(lir_program, format, arch)?;
    Ok(EmitPlan {
        format,
        arch,
        text: output.text,
        rodata: output.rodata,
        relocs: output.relocs,
        symbols: output.symbols,
        entry_offset: output.entry_offset,
    })
}

pub fn write_object(path: &Path, plan: &EmitPlan) -> Result<()> {
    match plan.format {
        TargetFormat::MachO => link::macho::emit_object_macho(path, plan.arch, plan),
        TargetFormat::Elf => link::elf::emit_object_elf64(path, plan.arch, plan),
        TargetFormat::Coff => link::coff::emit_object_coff(path, plan.arch, plan),
    }
}

pub fn write_executable(path: &Path, plan: &EmitPlan) -> Result<()> {
    link::link_executable(path, plan.format, plan.arch, plan)?;
    set_executable_permissions(path)?;
    Ok(())
}

pub fn dump_asm(path: &Path, plan: &EmitPlan) -> Result<()> {
    let mut out = String::new();
    writeln!(
        &mut out,
        "fp-native dump: format={:?} arch={:?} entry=0x{:x}",
        plan.format, plan.arch, plan.entry_offset
    )
    .ok();

    if !plan.symbols.is_empty() {
        let mut symbols: Vec<(&String, &u64)> = plan.symbols.iter().collect();
        symbols.sort_by_key(|(_, offset)| *offset);
        writeln!(&mut out, "\nSymbols:").ok();
        for (name, offset) in symbols {
            writeln!(&mut out, "  {:<32} 0x{:08x}", name, offset).ok();
        }
    }

    if !plan.relocs.is_empty() {
        writeln!(&mut out, "\nRelocations:").ok();
        for reloc in &plan.relocs {
            writeln!(
                &mut out,
                "  offset=0x{:08x} kind={:?} symbol={} addend={}",
                reloc.offset, reloc.kind, reloc.symbol, reloc.addend
            )
            .ok();
        }
    }

    writeln!(&mut out, "\n.text ({} bytes):", plan.text.len()).ok();
    dump_bytes(&mut out, &plan.text);
    writeln!(&mut out, "\n.rodata ({} bytes):", plan.rodata.len()).ok();
    dump_bytes(&mut out, &plan.rodata);

    fs::write(path, out).map_err(|e| Error::from(e.to_string()))?;
    Ok(())
}

fn dump_bytes(out: &mut String, bytes: &[u8]) {
    let mut offset = 0usize;
    while offset < bytes.len() {
        let end = (offset + 16).min(bytes.len());
        let chunk = &bytes[offset..end];
        write!(out, "  {:08x}  ", offset).ok();
        for (idx, byte) in chunk.iter().enumerate() {
            if idx == 8 {
                out.push(' ');
            }
            write!(out, "{:02x} ", byte).ok();
        }
        out.push('\n');
        offset = end;
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

fn set_executable_permissions(path: &Path) -> Result<()> {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(path)
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?
            .permissions();
        perms.set_mode(0o755);
        fs::set_permissions(path, perms)
            .map_err(|e| fp_core::error::Error::from(e.to_string()))?;
    }
    #[cfg(not(unix))]
    {
        let _ = path;
    }
    Ok(())
}
