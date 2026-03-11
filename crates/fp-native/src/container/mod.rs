use fp_core::container::{
    ContainerArchitecture, ContainerEndianness, ContainerFile, ContainerFormat, ContainerKind,
    ContainerReader, ContainerRelocation, ContainerRelocationEncoding, ContainerRelocationKind,
    ContainerRelocationSpec, ContainerRelocationTarget, ContainerSection, ContainerSectionKind,
    ContainerSymbol, ContainerSymbolKind, ContainerSymbolScope,
};
use fp_core::error::{Error, Result};
use object::read::FileKind;
use object::{Object as _, ObjectSection as _, ObjectSymbol as _};

pub struct ObjectContainerReader;

impl ObjectContainerReader {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ObjectContainerReader {
    fn default() -> Self {
        Self::new()
    }
}

impl ContainerReader for ObjectContainerReader {
    fn can_read(&self, bytes: &[u8]) -> bool {
        matches!(
            FileKind::parse(bytes),
            Ok(FileKind::Elf32
                | FileKind::Elf64
                | FileKind::MachO32
                | FileKind::MachO64
                | FileKind::MachOFat32
                | FileKind::MachOFat64
                | FileKind::Coff
                | FileKind::CoffBig
                | FileKind::Pe32
                | FileKind::Pe64
                | FileKind::Xcoff32
                | FileKind::Xcoff64)
        )
    }

    fn read(&self, bytes: &[u8]) -> Result<ContainerFile> {
        let file = object::File::parse(bytes).map_err(|err| Error::from(err.to_string()))?;

        let format = match file.format() {
            object::BinaryFormat::Elf => ContainerFormat::Elf,
            object::BinaryFormat::MachO => ContainerFormat::MachO,
            object::BinaryFormat::Coff => ContainerFormat::Coff,
            object::BinaryFormat::Pe => ContainerFormat::Pe,
            other => ContainerFormat::Other(format!("{other:?}")),
        };
        let architecture = match file.architecture() {
            object::Architecture::X86_64 => ContainerArchitecture::X86_64,
            object::Architecture::Aarch64 => ContainerArchitecture::Aarch64,
            other => ContainerArchitecture::Other(format!("{other:?}")),
        };
        let endianness = match file.endianness() {
            object::Endianness::Little => ContainerEndianness::Little,
            object::Endianness::Big => ContainerEndianness::Big,
        };
        let kind = match file.kind() {
            object::ObjectKind::Relocatable => ContainerKind::Object,
            object::ObjectKind::Executable => ContainerKind::Executable,
            object::ObjectKind::Dynamic => ContainerKind::Other,
            object::ObjectKind::Core => ContainerKind::Other,
            _ => ContainerKind::Other,
        };

        let mut container = ContainerFile::new(kind, format, architecture, endianness);

        let mut section_indices = Vec::new();
        for section in file.sections() {
            let name = section.name().unwrap_or("<unknown>").to_string();
            let kind = match section.kind() {
                object::SectionKind::Text => ContainerSectionKind::Text,
                object::SectionKind::ReadOnlyData => ContainerSectionKind::ReadOnlyData,
                object::SectionKind::Data => ContainerSectionKind::Data,
                object::SectionKind::UninitializedData => ContainerSectionKind::Bss,
                object::SectionKind::Debug => ContainerSectionKind::Debug,
                _ => ContainerSectionKind::Other,
            };
            let align = section.align();
            let data = section
                .data()
                .map(|bytes| bytes.to_vec())
                .unwrap_or_default();

            let id = container.add_section(ContainerSection {
                name,
                kind,
                align,
                data,
            });
            section_indices.push((section.index(), id));
        }

        let mut section_id_by_index = std::collections::HashMap::new();
        for (index, id) in section_indices {
            section_id_by_index.insert(index, id);
        }

        for symbol in file.symbols() {
            let name = match symbol.name() {
                Ok(name) => name.to_string(),
                Err(_) => continue,
            };
            let kind = match symbol.kind() {
                object::SymbolKind::Text => ContainerSymbolKind::Text,
                object::SymbolKind::Data => ContainerSymbolKind::Data,
                object::SymbolKind::Section => ContainerSymbolKind::Section,
                _ => ContainerSymbolKind::Other,
            };
            let scope = if symbol.is_weak() {
                ContainerSymbolScope::Weak
            } else if symbol.is_global() {
                ContainerSymbolScope::Global
            } else {
                ContainerSymbolScope::Local
            };
            let (section, value) = match symbol.section_index() {
                Some(section_index) => (
                    section_id_by_index.get(&section_index).copied(),
                    symbol.address(),
                ),
                None => (None, symbol.address()),
            };

            container.add_symbol(ContainerSymbol {
                name,
                kind,
                scope,
                section,
                value,
                size: symbol.size(),
            });
        }

        for section in file.sections() {
            let Some(container_section_id) = section_id_by_index.get(&section.index()).copied()
            else {
                continue;
            };
            for (offset, reloc) in section.relocations() {
                let target = match reloc.target() {
                    object::RelocationTarget::Symbol(symbol_index) => {
                        let symbol = file
                            .symbol_by_index(symbol_index)
                            .map_err(|err| Error::from(err.to_string()))?;
                        let name = symbol
                            .name()
                            .map_err(|_| Error::from("relocation refers to unnamed symbol"))?;
                        ContainerRelocationTarget::Symbol(name.to_string())
                    }
                    object::RelocationTarget::Section(section_index) => {
                        let section_id = section_id_by_index
                            .get(&section_index)
                            .copied()
                            .ok_or_else(|| Error::from("relocation refers to missing section"))?;
                        ContainerRelocationTarget::Section(section_id)
                    }
                    _ => {
                        continue;
                    }
                };

                let spec = match reloc.flags() {
                    object::RelocationFlags::Generic {
                        kind,
                        encoding,
                        size,
                    } => ContainerRelocationSpec {
                        kind: match kind {
                            object::RelocationKind::Absolute => ContainerRelocationKind::Absolute,
                            object::RelocationKind::Relative => ContainerRelocationKind::Relative,
                            _ => ContainerRelocationKind::Other,
                        },
                        encoding: match encoding {
                            object::RelocationEncoding::Generic => {
                                ContainerRelocationEncoding::Generic
                            }
                            object::RelocationEncoding::X86Branch => {
                                ContainerRelocationEncoding::X86Branch
                            }
                            object::RelocationEncoding::AArch64Call => {
                                ContainerRelocationEncoding::Aarch64Call
                            }
                            _ => ContainerRelocationEncoding::Other,
                        },
                        size: size as u8,
                    },
                    _ => continue,
                };

                container.add_relocation(ContainerRelocation {
                    section: container_section_id,
                    offset,
                    target,
                    addend: reloc.addend(),
                    spec,
                });
            }
        }

        Ok(container)
    }
}

pub struct ObjectContainerWriter;

impl ObjectContainerWriter {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ObjectContainerWriter {
    fn default() -> Self {
        Self::new()
    }
}

impl fp_core::container::ContainerWriter for ObjectContainerWriter {
    fn can_write(&self, format: &ContainerFormat) -> bool {
        matches!(
            format,
            ContainerFormat::Elf | ContainerFormat::MachO | ContainerFormat::Coff
        )
    }

    fn write(&self, container: &ContainerFile) -> Result<Vec<u8>> {
        crate::link::object_writer::write_object_container(container)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::emit::{EmitPlan, TargetArch, TargetFormat};
    use fp_core::asmir::{AsmArchitecture, AsmEndianness, AsmObjectFormat, AsmProgram, AsmTarget};
    use fp_core::container::ContainerWriter as _;
    use fp_core::lir::CallingConvention;
    use std::collections::HashMap;

    #[test]
    fn object_reader_roundtrips_object_writer_output() {
        let asmir = AsmProgram::new(AsmTarget {
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
        let container = crate::link::object_writer::container_from_emit_plan(
            TargetFormat::Elf,
            TargetArch::X86_64,
            &plan,
        )
        .unwrap();
        let bytes = ObjectContainerWriter::new().write(&container).unwrap();

        let roundtripped = ObjectContainerReader::new().read(&bytes).unwrap();
        assert_eq!(roundtripped.format, ContainerFormat::Elf);
        assert_eq!(roundtripped.architecture, ContainerArchitecture::X86_64);
        assert!(roundtripped.sections.iter().any(|s| s.name == ".text"));
    }
}
