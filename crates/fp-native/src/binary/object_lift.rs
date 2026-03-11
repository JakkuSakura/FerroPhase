use crate::binary::{TextRelocation, aarch64, x86_64};
use fp_core::asmir::{
    AsmArchitecture, AsmEndianness, AsmFunction, AsmFunctionSignature, AsmObjectFormat, AsmProgram,
    AsmSection, AsmSectionFlag, AsmSectionKind, AsmTarget, AsmTerminator, AsmType,
};
use fp_core::error::{Error, Result};
use fp_core::lir::{Linkage, Name, Visibility};
use object::{Object, ObjectSection, ObjectSymbol, RelocationTarget};

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
        });
    }

    let symbol = file
        .symbols()
        .filter(|symbol| symbol.section_index() == Some(text_index))
        .find(|symbol| symbol.size() > 0)
        .or_else(|| {
            file.symbols().find(|symbol| {
                symbol
                    .name()
                    .ok()
                    .map(|name| name == "main")
                    .unwrap_or(false)
            })
        });

    let (name, symbol_offset, symbol_size) = match symbol {
        Some(symbol) => {
            let name = symbol
                .name()
                .ok()
                .map(Name::new)
                .unwrap_or_else(|| Name::new("lifted"));
            let section_addr = text_section.address();
            let symbol_offset = symbol.address().saturating_sub(section_addr) as usize;
            let symbol_size = symbol.size() as usize;
            (name, symbol_offset, symbol_size)
        }
        None => {
            if text_bytes.is_empty() {
                return Err(Error::from("object lift requires non-empty .text section"));
            }
            (Name::new("lifted"), 0, text_bytes.len())
        }
    };
    if symbol_offset >= text_bytes.len() || symbol_offset + symbol_size > text_bytes.len() {
        return Err(Error::from("text symbol range is out of bounds"));
    }
    let code = &text_bytes[symbol_offset..symbol_offset + symbol_size];
    let relocs = relocs
        .into_iter()
        .filter(|reloc| {
            let offset = reloc.offset as usize;
            offset >= symbol_offset && offset < symbol_offset + symbol_size
        })
        .map(|mut reloc| {
            reloc.offset = reloc.offset - symbol_offset as u64;
            reloc
        })
        .collect::<Vec<_>>();

    let lifted = match architecture {
        AsmArchitecture::Aarch64 => aarch64::lift_function_bytes(code, relocs.as_slice())?,
        AsmArchitecture::X86_64 => x86_64::lift_function_bytes(code, relocs.as_slice())?,
        _ => {
            return Err(Error::from(
                "object lift internal error: unsupported architecture",
            ));
        }
    };

    let mut program = AsmProgram::new(AsmTarget {
        architecture,
        object_format: AsmObjectFormat::Raw,
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

    let return_type = match lifted.terminator {
        Some(AsmTerminator::Return(Some(_))) => AsmType::I64,
        _ => AsmType::Void,
    };

    program.functions.push(AsmFunction {
        name,
        signature: AsmFunctionSignature {
            params: Vec::new(),
            return_type,
            is_variadic: false,
        },
        basic_blocks: vec![fp_core::asmir::AsmBlock {
            id: 0,
            label: Some(Name::new("entry")),
            instructions: lifted.instructions,
            terminator: lifted.terminator.unwrap_or(AsmTerminator::Return(None)),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }],
        locals: lifted.locals,
        stack_slots: Vec::new(),
        frame: None,
        linkage: Linkage::External,
        visibility: Visibility::Default,
        calling_convention: None,
        section: Some(".text".to_string()),
        is_declaration: false,
    });

    Ok(program)
}
