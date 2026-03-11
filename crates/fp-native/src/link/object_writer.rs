use crate::emit::{EmitPlan, RelocKind, RelocSection, TargetArch, TargetFormat};
use fp_core::container::{
    ContainerArchitecture, ContainerEndianness, ContainerFile, ContainerFormat, ContainerKind,
    ContainerRelocation, ContainerRelocationEncoding, ContainerRelocationKind,
    ContainerRelocationSpec, ContainerRelocationTarget, ContainerSection, ContainerSectionKind,
    ContainerSymbol, ContainerSymbolKind, ContainerSymbolScope,
};
use fp_core::error::{Error, Result};
use object::write::{Mangling, Object, Relocation, StandardSection, Symbol, SymbolSection};
use object::{
    Architecture, BinaryFormat, Endianness, RelocationEncoding, RelocationFlags, RelocationKind,
    SectionKind, SymbolFlags, SymbolKind, SymbolScope,
};
use std::collections::HashMap;

fn binary_format_for_container(format: &ContainerFormat) -> Result<BinaryFormat> {
    Ok(match format {
        ContainerFormat::Elf => BinaryFormat::Elf,
        ContainerFormat::MachO => BinaryFormat::MachO,
        ContainerFormat::Coff => BinaryFormat::Coff,
        other => {
            return Err(Error::from(format!(
                "unsupported container format for object emission: {other:?}"
            )));
        }
    })
}

fn architecture_for_container(arch: &ContainerArchitecture) -> Result<Architecture> {
    Ok(match arch {
        ContainerArchitecture::X86_64 => Architecture::X86_64,
        ContainerArchitecture::Aarch64 => Architecture::Aarch64,
        other => {
            return Err(Error::from(format!(
                "unsupported container architecture for object emission: {other:?}"
            )));
        }
    })
}

fn endianness_for_container(endian: ContainerEndianness) -> Endianness {
    match endian {
        ContainerEndianness::Little => Endianness::Little,
        ContainerEndianness::Big => Endianness::Big,
    }
}

fn relocation_flags(kind: RelocKind, arch: TargetArch) -> Result<RelocationFlags> {
    Ok(RelocationFlags::Generic {
        kind: match kind {
            RelocKind::Abs64 => RelocationKind::Absolute,
            RelocKind::CallRel32 => RelocationKind::Relative,
            RelocKind::Aarch64AdrpAdd => {
                return Err(Error::from(
                    "unsupported Aarch64AdrpAdd relocation for object emission",
                ));
            }
        },
        encoding: match kind {
            RelocKind::Abs64 => RelocationEncoding::Generic,
            RelocKind::CallRel32 => match arch {
                TargetArch::X86_64 => RelocationEncoding::X86Branch,
                TargetArch::Aarch64 => RelocationEncoding::AArch64Call,
            },
            RelocKind::Aarch64AdrpAdd => RelocationEncoding::Unknown,
        },
        size: match kind {
            RelocKind::Abs64 => 64,
            RelocKind::CallRel32 => 32,
            RelocKind::Aarch64AdrpAdd => 0,
        },
    })
}

fn relocation_flags_for_container(spec: ContainerRelocationSpec) -> Result<RelocationFlags> {
    Ok(RelocationFlags::Generic {
        kind: match spec.kind {
            ContainerRelocationKind::Absolute => RelocationKind::Absolute,
            ContainerRelocationKind::Relative => RelocationKind::Relative,
            ContainerRelocationKind::Other => RelocationKind::Unknown,
        },
        encoding: match spec.encoding {
            ContainerRelocationEncoding::Generic => RelocationEncoding::Generic,
            ContainerRelocationEncoding::X86Branch => RelocationEncoding::X86Branch,
            ContainerRelocationEncoding::Aarch64Call => RelocationEncoding::AArch64Call,
            ContainerRelocationEncoding::Other => RelocationEncoding::Unknown,
        },
        size: spec.size,
    })
}

fn intern_symbol(
    obj: &mut Object,
    symbol_ids: &mut HashMap<String, object::write::SymbolId>,
    name: &str,
    kind: SymbolKind,
    section: SymbolSection,
    value: u64,
) -> object::write::SymbolId {
    if let Some(id) = symbol_ids.get(name) {
        return *id;
    }
    let id = obj.add_symbol(Symbol {
        name: name.as_bytes().to_vec(),
        value,
        size: 0,
        kind,
        scope: SymbolScope::Linkage,
        weak: false,
        section,
        flags: SymbolFlags::None,
    });
    symbol_ids.insert(name.to_string(), id);
    id
}

pub fn emit_object(
    path: &std::path::Path,
    format: TargetFormat,
    arch: TargetArch,
    plan: &EmitPlan,
) -> Result<()> {
    let container = container_from_emit_plan(format, arch, plan)?;
    let bytes = write_object_container(&container)?;
    std::fs::write(path, bytes).map_err(|err| Error::from(err.to_string()))?;
    Ok(())
}

pub fn container_from_emit_plan(
    format: TargetFormat,
    arch: TargetArch,
    plan: &EmitPlan,
) -> Result<ContainerFile> {
    let format = match format {
        TargetFormat::Elf => ContainerFormat::Elf,
        TargetFormat::MachO => ContainerFormat::MachO,
        TargetFormat::Coff => ContainerFormat::Coff,
    };
    let architecture = match arch {
        TargetArch::X86_64 => ContainerArchitecture::X86_64,
        TargetArch::Aarch64 => ContainerArchitecture::Aarch64,
    };

    let mut container = ContainerFile::new(
        ContainerKind::Object,
        format,
        architecture,
        ContainerEndianness::Little,
    );

    let text_section = container.add_section(ContainerSection {
        name: ".text".to_string(),
        kind: ContainerSectionKind::Text,
        align: 16,
        data: plan.text.clone(),
    });
    let rodata_section = if plan.rodata.is_empty() {
        None
    } else {
        Some(container.add_section(ContainerSection {
            name: ".rodata".to_string(),
            kind: ContainerSectionKind::ReadOnlyData,
            align: 16,
            data: plan.rodata.clone(),
        }))
    };

    for (name, offset) in &plan.symbols {
        container.add_symbol(ContainerSymbol {
            name: name.clone(),
            kind: ContainerSymbolKind::Text,
            scope: ContainerSymbolScope::Global,
            section: Some(text_section),
            value: *offset,
            size: 0,
        });
    }
    if let Some(rodata_section) = rodata_section {
        for (name, offset) in &plan.rodata_symbols {
            container.add_symbol(ContainerSymbol {
                name: name.clone(),
                kind: ContainerSymbolKind::Data,
                scope: ContainerSymbolScope::Global,
                section: Some(rodata_section),
                value: *offset,
                size: 0,
            });
        }
    }

    for reloc in &plan.relocs {
        let section = match reloc.section {
            RelocSection::Text => text_section,
            RelocSection::Rdata => {
                rodata_section.ok_or_else(|| Error::from("relocation refers to missing .rodata"))?
            }
        };
        let target = if reloc.symbol == ".rodata" {
            ContainerRelocationTarget::Section(
                rodata_section.ok_or_else(|| Error::from("relocation refers to .rodata"))?,
            )
        } else {
            ContainerRelocationTarget::Symbol(reloc.symbol.clone())
        };
        let flags = relocation_flags(reloc.kind, arch)?;
        let spec = match flags {
            RelocationFlags::Generic {
                kind,
                encoding,
                size,
            } => ContainerRelocationSpec {
                kind: match kind {
                    RelocationKind::Absolute => ContainerRelocationKind::Absolute,
                    RelocationKind::Relative => ContainerRelocationKind::Relative,
                    _ => ContainerRelocationKind::Other,
                },
                encoding: match encoding {
                    RelocationEncoding::Generic => ContainerRelocationEncoding::Generic,
                    RelocationEncoding::X86Branch => ContainerRelocationEncoding::X86Branch,
                    RelocationEncoding::AArch64Call => ContainerRelocationEncoding::Aarch64Call,
                    _ => ContainerRelocationEncoding::Other,
                },
                size: size as u8,
            },
            _ => {
                return Err(Error::from(
                    "unsupported non-generic relocation flags in emit plan",
                ));
            }
        };

        container.add_relocation(ContainerRelocation {
            section,
            offset: reloc.offset,
            target,
            addend: reloc.addend,
            spec,
        });
    }

    Ok(container)
}

pub fn write_object_container(container: &ContainerFile) -> Result<Vec<u8>> {
    if container.kind != ContainerKind::Object {
        return Err(Error::from(
            "write_object_container expects object container",
        ));
    }

    let format = binary_format_for_container(&container.format)?;
    let architecture = architecture_for_container(&container.architecture)?;
    let mut obj = Object::new(
        format,
        architecture,
        endianness_for_container(container.endianness),
    );
    obj.set_mangling(Mangling::default(format, architecture));

    let mut section_ids = Vec::with_capacity(container.sections.len());
    for section in &container.sections {
        let section_id = match section.kind {
            ContainerSectionKind::Text => obj.section_id(StandardSection::Text),
            ContainerSectionKind::ReadOnlyData => obj.section_id(StandardSection::ReadOnlyData),
            ContainerSectionKind::Data => obj.section_id(StandardSection::Data),
            ContainerSectionKind::Bss => obj.section_id(StandardSection::UninitializedData),
            ContainerSectionKind::Debug | ContainerSectionKind::Other => obj.add_section(
                Vec::new(),
                section.name.as_bytes().to_vec(),
                SectionKind::Unknown,
            ),
        };

        if !section.data.is_empty() {
            obj.append_section_data(section_id, &section.data, section.align);
        }
        section_ids.push(section_id);
    }

    let mut symbol_ids: HashMap<String, object::write::SymbolId> = HashMap::new();
    for symbol in &container.symbols {
        let section = match symbol.section {
            Some(section) => {
                let section_id = *section_ids.get(section).ok_or_else(|| {
                    Error::from(format!("symbol refers to missing section id {section}"))
                })?;
                SymbolSection::Section(section_id)
            }
            None => SymbolSection::Undefined,
        };

        if symbol_ids.contains_key(&symbol.name) {
            continue;
        }

        let weak = matches!(symbol.scope, ContainerSymbolScope::Weak);
        let scope = match symbol.scope {
            ContainerSymbolScope::Local => SymbolScope::Compilation,
            ContainerSymbolScope::Global | ContainerSymbolScope::Weak => SymbolScope::Linkage,
        };
        let kind = match symbol.kind {
            ContainerSymbolKind::Text => SymbolKind::Text,
            ContainerSymbolKind::Data => SymbolKind::Data,
            ContainerSymbolKind::Section => SymbolKind::Section,
            ContainerSymbolKind::Other => SymbolKind::Unknown,
        };

        let id = obj.add_symbol(Symbol {
            name: symbol.name.as_bytes().to_vec(),
            value: symbol.value,
            size: symbol.size,
            kind,
            scope,
            weak,
            section,
            flags: SymbolFlags::None,
        });
        symbol_ids.insert(symbol.name.clone(), id);
    }

    for reloc in &container.relocations {
        let section_id = *section_ids
            .get(reloc.section)
            .ok_or_else(|| Error::from("relocation refers to missing section"))?;

        let symbol_id = match &reloc.target {
            ContainerRelocationTarget::Section(section) => {
                let section_id = *section_ids
                    .get(*section)
                    .ok_or_else(|| Error::from("relocation refers to missing section"))?;
                obj.section_symbol(section_id)
            }
            ContainerRelocationTarget::Symbol(name) => {
                if let Some(id) = symbol_ids.get(name).copied() {
                    id
                } else {
                    let kind = match reloc.spec.encoding {
                        ContainerRelocationEncoding::X86Branch
                        | ContainerRelocationEncoding::Aarch64Call => SymbolKind::Text,
                        _ => SymbolKind::Data,
                    };
                    intern_symbol(
                        &mut obj,
                        &mut symbol_ids,
                        name,
                        kind,
                        SymbolSection::Undefined,
                        0,
                    )
                }
            }
        };

        obj.add_relocation(
            section_id,
            Relocation {
                offset: reloc.offset,
                symbol: symbol_id,
                addend: reloc.addend,
                flags: relocation_flags_for_container(reloc.spec)?,
            },
        )
        .map_err(|err| Error::from(err.to_string()))?;
    }

    obj.write().map_err(|err| Error::from(err.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::asmir::{AsmArchitecture, AsmEndianness, AsmObjectFormat, AsmTarget};
    use fp_core::lir::CallingConvention;
    use object::Object as _;

    #[test]
    fn container_writer_emits_parseable_elf_object() {
        let asmir = fp_core::asmir::AsmProgram::new(AsmTarget {
            architecture: AsmArchitecture::X86_64,
            object_format: AsmObjectFormat::Elf,
            endianness: AsmEndianness::Little,
            pointer_width: 64,
            default_calling_convention: Some(CallingConvention::X86_64SysV),
        });
        let mut symbols = HashMap::new();
        symbols.insert("main".to_string(), 0);
        let plan = EmitPlan {
            format: TargetFormat::Elf,
            arch: TargetArch::X86_64,
            asmir,
            text: vec![0xC3],
            rodata: Vec::new(),
            relocs: Vec::new(),
            symbols,
            rodata_symbols: HashMap::new(),
            entry_offset: 0,
        };

        let container = container_from_emit_plan(TargetFormat::Elf, TargetArch::X86_64, &plan)
            .expect("container conversion should succeed");
        let bytes = write_object_container(&container).expect("object write should succeed");
        assert!(bytes.starts_with(b"\x7fELF"));

        let parsed = object::File::parse(bytes.as_slice()).expect("object parse should succeed");
        assert_eq!(parsed.format(), object::BinaryFormat::Elf);
        assert_eq!(parsed.architecture(), object::Architecture::X86_64);
        assert!(parsed.section_by_name(".text").is_some());
    }
}
