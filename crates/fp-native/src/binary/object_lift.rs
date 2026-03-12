use crate::binary::{TextRelocation, aarch64, x86_64};
use fp_core::asmir::{
    AsmArchitecture, AsmEndianness, AsmFunction, AsmFunctionSignature, AsmObjectFormat, AsmProgram,
    AsmSection, AsmSectionFlag, AsmSectionKind, AsmStackFrame, AsmSyscallConvention, AsmTarget,
    AsmTerminator, AsmType,
};
use fp_core::error::{Error, Result};
use fp_core::lir::{CallingConvention, Linkage, Name, Visibility};
use object::{Object, ObjectSection, ObjectSymbol, RelocationTarget, SymbolKind};

pub(super) fn lift_object_to_asmir(bytes: &[u8]) -> Result<AsmProgram> {
    let file = object::File::parse(bytes).map_err(|err| Error::from(err.to_string()))?;
    let architecture = match file.architecture() {
        object::Architecture::Aarch64 => AsmArchitecture::Aarch64,
        object::Architecture::X86_64 => AsmArchitecture::X86_64,
        other => {
            return Err(Error::from(format!(
                "object lift currently supports only x86_64 and aarch64; got {other:?}"
            )));
        }
    };

    let object_format = match file.format() {
        object::BinaryFormat::Elf => AsmObjectFormat::Elf,
        object::BinaryFormat::MachO => AsmObjectFormat::MachO,
        object::BinaryFormat::Coff => AsmObjectFormat::Coff,
        _ => AsmObjectFormat::Raw,
    };

    let text_section = file
        .sections()
        .find(|section| section.kind() == object::SectionKind::Text)
        .or_else(|| file.section_by_name(".text"))
        .or_else(|| file.section_by_name("__text"))
        .ok_or_else(|| Error::from("object file has no .text section"))?;
    let text_index = text_section.index();
    let text_bytes = text_section
        .data()
        .map_err(|err| Error::from(err.to_string()))?;

    let mut relocs = Vec::new();
    for (offset, relocation) in text_section.relocations() {
        let RelocationTarget::Symbol(symbol_index) = relocation.target() else {
            continue;
        };
        let symbol = file
            .symbol_by_index(symbol_index)
            .map_err(|err| Error::from(err.to_string()))?;
        let name = match symbol.name() {
            Ok(name) => name.to_string(),
            Err(_) => continue,
        };
        relocs.push(TextRelocation {
            offset,
            symbol: name,
            addend: relocation.addend(),
            kind: relocation.kind(),
            encoding: relocation.encoding(),
            size: relocation.size(),
            flags: relocation.flags(),
        });
    }

    let mut program = AsmProgram::new(AsmTarget {
        architecture: architecture.clone(),
        object_format: object_format.clone(),
        endianness: AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: None,
    });
    program.sections.push(AsmSection {
        name: ".text".to_string(),
        kind: AsmSectionKind::Text,
        flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
        alignment: Some(16),
    });

    let mut text_symbols = file
        .symbols()
        .filter(|symbol| symbol.section_index() == Some(text_index))
        .filter(|symbol| symbol.kind() == SymbolKind::Text)
        .filter(|symbol| symbol.size() > 0)
        .filter_map(|symbol| {
            let name = symbol
                .name()
                .ok()
                .map(Name::new)
                .unwrap_or_else(|| Name::new("lifted"));
            let section_addr = text_section.address();
            let symbol_offset = symbol.address().saturating_sub(section_addr) as usize;
            let symbol_size = symbol.size() as usize;
            Some((name, symbol_offset, symbol_size))
        })
        .collect::<Vec<_>>();
    text_symbols.sort_by_key(|(_, symbol_offset, _)| *symbol_offset);

    if text_symbols.is_empty() {
        if text_bytes.is_empty() {
            return Err(Error::from("object lift requires non-empty .text section"));
        }
        text_symbols.push((Name::new("lifted"), 0, text_bytes.len()));
    }

    for (name, symbol_offset, symbol_size) in text_symbols {
        if symbol_offset >= text_bytes.len() || symbol_offset + symbol_size > text_bytes.len() {
            return Err(Error::from("text symbol range is out of bounds"));
        }

        let code = &text_bytes[symbol_offset..symbol_offset + symbol_size];
        let symbol_relocs = relocs
            .iter()
            .filter(|reloc| {
                let offset = reloc.offset as usize;
                offset >= symbol_offset && offset < symbol_offset + symbol_size
            })
            .map(|reloc| TextRelocation {
                offset: reloc.offset - symbol_offset as u64,
                symbol: reloc.symbol.clone(),
                addend: reloc.addend,
                kind: reloc.kind,
                encoding: reloc.encoding,
                size: reloc.size,
                flags: reloc.flags,
            })
            .collect::<Vec<_>>();

        let syscall_convention = match (&architecture, &object_format) {
            (AsmArchitecture::X86_64, AsmObjectFormat::Elf) => AsmSyscallConvention::LinuxX86_64,
            (AsmArchitecture::X86_64, AsmObjectFormat::MachO) => AsmSyscallConvention::DarwinX86_64,
            (AsmArchitecture::Aarch64, AsmObjectFormat::Elf) => AsmSyscallConvention::LinuxAarch64,
            (AsmArchitecture::Aarch64, AsmObjectFormat::MachO) => {
                AsmSyscallConvention::DarwinAarch64
            }
            _ => {
                return Err(Error::from(
                    "object lift currently supports syscalls only for ELF/Mach-O x86_64/aarch64",
                ));
            }
        };

        let mut lifted = match &architecture {
            AsmArchitecture::Aarch64 => {
                aarch64::lift_function_bytes(code, symbol_relocs.as_slice(), syscall_convention)?
            }
            AsmArchitecture::X86_64 => {
                x86_64::lift_function_bytes(code, symbol_relocs.as_slice(), syscall_convention)?
            }
            _ => {
                return Err(Error::from(
                    "object lift internal error: unsupported architecture",
                ));
            }
        };

        let calling_convention = match (&architecture, &object_format) {
            (AsmArchitecture::X86_64, AsmObjectFormat::Coff | AsmObjectFormat::Pe) => {
                CallingConvention::Win64
            }
            (AsmArchitecture::X86_64, _) => CallingConvention::X86_64SysV,
            (AsmArchitecture::Aarch64, _) => CallingConvention::AAPCS,
            _ => CallingConvention::C,
        };
        for block in &mut lifted.basic_blocks {
            for inst in &mut block.instructions {
                if let fp_core::asmir::AsmInstructionKind::Call {
                    calling_convention: cc,
                    ..
                } = &mut inst.kind
                {
                    *cc = calling_convention.clone();
                }
            }
        }

        let return_type = lifted
            .basic_blocks
            .iter()
            .find_map(|block| match &block.terminator {
                AsmTerminator::Return(Some(_)) => Some(AsmType::I64),
                _ => None,
            })
            .unwrap_or(AsmType::Void);

        let frame = infer_stack_frame(&architecture, &lifted);

        program.functions.push(AsmFunction {
            name,
            signature: AsmFunctionSignature {
                params: Vec::new(),
                return_type,
                is_variadic: false,
            },
            basic_blocks: lifted.basic_blocks,
            locals: lifted.locals,
            stack_slots: Vec::new(),
            frame,
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: None,
            section: Some(".text".to_string()),
            is_declaration: false,
        });
    }

    Ok(program)
}

fn infer_stack_frame(
    arch: &AsmArchitecture,
    lifted: &crate::binary::LiftedFunction,
) -> Option<AsmStackFrame> {
    let sp_name = match arch {
        AsmArchitecture::X86_64 => "rsp",
        AsmArchitecture::Aarch64 => "sp",
        _ => return None,
    };
    let sp_local = lifted
        .locals
        .iter()
        .find(|local| local.name.as_deref() == Some(sp_name))?
        .id;

    let entry = lifted.basic_blocks.first()?;
    for inst in &entry.instructions {
        let fp_core::asmir::AsmInstructionKind::Sub(lhs, rhs) = &inst.kind else {
            continue;
        };
        if !matches!(lhs, fp_core::asmir::AsmValue::Local(id) if *id == sp_local) {
            continue;
        }
        let fp_core::asmir::AsmValue::Constant(fp_core::asmir::AsmConstant::Int(size, _)) = rhs
        else {
            continue;
        };
        if *size <= 0 {
            continue;
        }
        return Some(AsmStackFrame {
            stack_size: (*size).min(u32::MAX as i64) as u32,
            stack_alignment: 16,
            callee_saved: Vec::new(),
        });
    }

    None
}
