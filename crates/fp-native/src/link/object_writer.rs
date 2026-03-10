use crate::emit::{EmitPlan, RelocKind, RelocSection, TargetArch, TargetFormat};
use fp_core::error::{Error, Result};
use object::write::{Mangling, Object, Relocation, StandardSection, Symbol, SymbolSection};
use object::{
    Architecture, BinaryFormat, Endianness, RelocationEncoding, RelocationFlags, RelocationKind,
    SymbolFlags, SymbolKind, SymbolScope,
};
use std::collections::HashMap;

fn architecture(arch: TargetArch) -> Architecture {
    match arch {
        TargetArch::X86_64 => Architecture::X86_64,
        TargetArch::Aarch64 => Architecture::Aarch64,
    }
}

fn binary_format(format: TargetFormat) -> BinaryFormat {
    match format {
        TargetFormat::Elf => BinaryFormat::Elf,
        TargetFormat::MachO => BinaryFormat::MachO,
        TargetFormat::Coff => BinaryFormat::Coff,
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
                ))
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

pub fn emit_object(path: &std::path::Path, format: TargetFormat, arch: TargetArch, plan: &EmitPlan) -> Result<()> {
    let format = binary_format(format);
    let architecture = architecture(arch);
    let mut obj = Object::new(format, architecture, Endianness::Little);
    obj.set_mangling(Mangling::default(format, architecture));

    let text_id = obj.section_id(StandardSection::Text);
    obj.append_section_data(text_id, &plan.text, 16);

    let rodata_id = if plan.rodata.is_empty() {
        None
    } else {
        let id = obj.section_id(StandardSection::ReadOnlyData);
        obj.append_section_data(id, &plan.rodata, 16);
        Some(id)
    };

    let mut symbol_ids: HashMap<String, object::write::SymbolId> = HashMap::new();

    for (name, offset) in &plan.symbols {
        intern_symbol(
            &mut obj,
            &mut symbol_ids,
            name,
            SymbolKind::Text,
            SymbolSection::Section(text_id),
            *offset,
        );
    }

    if let Some(rodata_id) = rodata_id {
        for (name, offset) in &plan.rodata_symbols {
            intern_symbol(
                &mut obj,
                &mut symbol_ids,
                name,
                SymbolKind::Data,
                SymbolSection::Section(rodata_id),
                *offset,
            );
        }
    }

    let rodata_section_symbol = rodata_id.map(|id| obj.section_symbol(id));

    for reloc in &plan.relocs {
        let section_id = match reloc.section {
            RelocSection::Text => text_id,
            RelocSection::Rdata => {
                let Some(rodata_id) = rodata_id else {
                    return Err(Error::from("relocation refers to missing .rodata"));
                };
                rodata_id
            }
        };

        let symbol_id = if reloc.symbol == ".rodata" {
            rodata_section_symbol.ok_or_else(|| Error::from("relocation refers to .rodata"))?
        } else {
            if let Some(id) = symbol_ids.get(&reloc.symbol).copied() {
                id
            } else {
                let kind = if matches!(reloc.kind, RelocKind::CallRel32) {
                    SymbolKind::Text
                } else {
                    SymbolKind::Data
                };
                intern_symbol(
                    &mut obj,
                    &mut symbol_ids,
                    &reloc.symbol,
                    kind,
                    SymbolSection::Undefined,
                    0,
                )
            }
        };

        let flags = relocation_flags(reloc.kind, arch)?;
        obj.add_relocation(
            section_id,
            Relocation {
                offset: reloc.offset,
                symbol: symbol_id,
                addend: reloc.addend,
                flags,
            },
        )
        .map_err(|err| Error::from(err.to_string()))?;

    }

    let bytes = obj.write().map_err(|err| Error::from(err.to_string()))?;
    std::fs::write(path, bytes).map_err(|err| Error::from(err.to_string()))?;
    Ok(())
}

