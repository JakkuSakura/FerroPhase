use crate::binary::{DataRegion, RipSymbol, RipSymbolKind, TextRelocation, aarch64, x86_64};
use crate::container::container_kind_for_object_kind;
use fp_core::asmir::{
    AsmArchitecture, AsmConstant, AsmEndianness, AsmFunction, AsmFunctionSignature, AsmGlobal,
    AsmGlobalRelocation, AsmLocal, AsmObjectFormat, AsmProgram, AsmRelocationKind, AsmSection,
    AsmSectionFlag, AsmSectionKind, AsmStackFrame, AsmSyscallConvention, AsmTarget, AsmTerminator,
    AsmType,
};
use fp_core::container::{
    ContainerArchitecture, ContainerEndianness, ContainerFile, ContainerRelocation,
    ContainerRelocationEncoding, ContainerRelocationKind, ContainerRelocationSpec,
    ContainerRelocationTarget, ContainerSection, ContainerSectionKind, ContainerSymbol,
    ContainerSymbolKind, ContainerSymbolScope,
};
use fp_core::error::{Error, Result};
use fp_core::lir::{CallingConvention, Linkage, Name, Visibility};
use object::{Object, ObjectSection, ObjectSymbol, RelocationTarget, SymbolKind};
use std::collections::HashMap;
use std::collections::HashSet;

fn normalize_symbol_name<'a>(object_format: &AsmObjectFormat, raw: &'a str) -> &'a str {
    match object_format {
        // Mach-O symbols are typically prefixed with an ABI underscore. Strip
        // exactly one leading underscore so AsmIR names stay ABI-agnostic.
        AsmObjectFormat::MachO => {
            if raw.starts_with("__main") {
                "main"
            } else {
                raw.strip_prefix('_').unwrap_or(raw)
            }
        }
        _ => raw,
    }
}

fn container_symbol_scope(symbol: &object::Symbol<'_, '_>) -> ContainerSymbolScope {
    if symbol.is_weak() {
        ContainerSymbolScope::Weak
    } else if symbol.is_global() {
        ContainerSymbolScope::Global
    } else {
        ContainerSymbolScope::Local
    }
}

pub(super) fn lift_object_to_asmir(bytes: &[u8]) -> Result<AsmProgram> {
    let file = object::File::parse(bytes).map_err(|err| Error::from(err.to_string()))?;
    // ELF PIE binaries are reported as `Dynamic` by the `object` crate.
    let is_executable = matches!(
        file.kind(),
        object::ObjectKind::Executable | object::ObjectKind::Dynamic
    );
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

    let container_kind = container_kind_for_object_kind(file.kind());
    let container_format = match file.format() {
        object::BinaryFormat::Elf => AsmObjectFormat::Elf,
        object::BinaryFormat::MachO => AsmObjectFormat::MachO,
        object::BinaryFormat::Coff => AsmObjectFormat::Coff,
        object::BinaryFormat::Pe => AsmObjectFormat::Pe,
        other => AsmObjectFormat::Custom(format!("{other:?}")),
    };
    let container_arch = match file.architecture() {
        object::Architecture::X86_64 => ContainerArchitecture::X86_64,
        object::Architecture::Aarch64 => ContainerArchitecture::Aarch64,
        other => ContainerArchitecture::Other(format!("{other:?}")),
    };
    let mut container = ContainerFile::new(
        container_kind,
        container_format,
        container_arch,
        ContainerEndianness::Little,
    );
    let mut section_ids: HashMap<object::SectionIndex, usize> = HashMap::new();
    for section in file.sections() {
        let name = section
            .name()
            .ok()
            .filter(|value| !value.is_empty())
            .unwrap_or("<anon>");
        let kind = match section.kind() {
            object::SectionKind::Text => ContainerSectionKind::Text,
            object::SectionKind::ReadOnlyData => ContainerSectionKind::ReadOnlyData,
            object::SectionKind::Data => ContainerSectionKind::Data,
            object::SectionKind::UninitializedData => ContainerSectionKind::Bss,
            object::SectionKind::Debug => ContainerSectionKind::Debug,
            _ => ContainerSectionKind::Other,
        };
        let align = section.align();
        let data = match section.kind() {
            object::SectionKind::UninitializedData => vec![0u8; section.size() as usize],
            _ => section.data().map(|data| data.to_vec()).unwrap_or_default(),
        };
        let id = container.add_section(ContainerSection {
            name: name.to_string(),
            kind,
            align,
            data,
        });
        section_ids.insert(section.index(), id);
    }

    for symbol in file.symbols() {
        let name = match symbol.name() {
            Ok(name) if !name.is_empty() => name,
            _ => continue,
        };
        let kind = match symbol.kind() {
            object::SymbolKind::Text => ContainerSymbolKind::Text,
            object::SymbolKind::Data => ContainerSymbolKind::Data,
            object::SymbolKind::Section => ContainerSymbolKind::Section,
            _ => ContainerSymbolKind::Other,
        };
        let scope = container_symbol_scope(&symbol);
        let section = symbol
            .section_index()
            .and_then(|idx| section_ids.get(&idx).copied());
        let value = if let Some(idx) = symbol.section_index() {
            file.section_by_index(idx)
                .ok()
                .map(|section| symbol.address().saturating_sub(section.address()))
                .unwrap_or(symbol.address())
        } else {
            symbol.address()
        };
        container.add_symbol(ContainerSymbol {
            name: name.to_string(),
            kind,
            scope,
            section,
            value,
            size: symbol.size(),
        });
    }

    for section in file.sections() {
        let Some(&container_section) = section_ids.get(&section.index()) else {
            continue;
        };
        for (offset, relocation) in section.relocations() {
            let target = match relocation.target() {
                RelocationTarget::Symbol(symbol_index) => {
                    let symbol = match file.symbol_by_index(symbol_index) {
                        Ok(symbol) => symbol,
                        Err(_) => continue,
                    };
                    let name = symbol
                        .name()
                        .ok()
                        .filter(|value| !value.is_empty())
                        .map(|value| value.to_string())
                        .or_else(|| {
                            symbol
                                .section_index()
                                .and_then(|idx| file.section_by_index(idx).ok())
                                .and_then(|section| section.name().ok())
                                .filter(|value| !value.is_empty())
                                .map(|value| value.to_string())
                        })
                        .unwrap_or_default();
                    ContainerRelocationTarget::Symbol(name)
                }
                RelocationTarget::Section(section_index) => {
                    let Some(&target_id) = section_ids.get(&section_index) else {
                        continue;
                    };
                    ContainerRelocationTarget::Section(target_id)
                }
                _ => continue,
            };
            let kind = match relocation.kind() {
                object::RelocationKind::Absolute => ContainerRelocationKind::Absolute,
                object::RelocationKind::Relative
                | object::RelocationKind::PltRelative
                | object::RelocationKind::ImageOffset => ContainerRelocationKind::Relative,
                _ => ContainerRelocationKind::Other,
            };
            let encoding = match relocation.encoding() {
                object::RelocationEncoding::Generic => ContainerRelocationEncoding::Generic,
                object::RelocationEncoding::X86Branch => ContainerRelocationEncoding::X86Branch,
                object::RelocationEncoding::AArch64Call => ContainerRelocationEncoding::Aarch64Call,
                _ => ContainerRelocationEncoding::Other,
            };
            container.add_relocation(ContainerRelocation {
                section: container_section,
                offset,
                target,
                addend: relocation.addend(),
                spec: ContainerRelocationSpec {
                    kind,
                    encoding,
                    size: relocation.size() as u8,
                },
            });
        }
    }

    let (elf_rip_symbols, elf_weak_imports, elf_plt_targets) = if is_executable
        && matches!(architecture, AsmArchitecture::X86_64)
        && matches!(object_format, AsmObjectFormat::Elf)
    {
        build_elf_dynamic_symbol_map(bytes)
    } else {
        (HashMap::new(), HashSet::new(), HashMap::new())
    };

    // Prefer the conventional `.text`/`__text` section name first.
    // Executables can have other `Text`-kind sections like `.init` that we
    // don't want to treat as the main code section.
    let text_section = file
        .section_by_name(".text")
        .or_else(|| file.section_by_name("__text"))
        .or_else(|| {
            file.sections()
                .find(|section| section.kind() == object::SectionKind::Text)
        })
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
        let is_import = symbol.section_index().is_none()
            || symbol
                .section_index()
                .and_then(|index| file.section_by_index(index).ok())
                .and_then(|section| section.name().ok())
                .is_some_and(|name| name.contains(".plt") || name.contains("__stubs"));
        let name = match symbol.name() {
            Ok(name) if !name.is_empty() => name.to_string(),
            _ => symbol
                .section_index()
                .and_then(|index| file.section_by_index(index).ok())
                .and_then(|section| section.name().ok())
                .filter(|name| !name.is_empty())
                .map(|name| name.to_string())
                .unwrap_or_default(),
        };

        let is_got = if object_format == AsmObjectFormat::Elf
            && architecture == AsmArchitecture::X86_64
            && matches!(relocation.flags(), object::RelocationFlags::Elf { .. })
        {
            let elf_type = match relocation.flags() {
                object::RelocationFlags::Elf { r_type } => r_type,
                _ => 0,
            };
            matches!(
                elf_type,
                object::elf::R_X86_64_GOT32
                    | object::elf::R_X86_64_GOTPCREL
                    | object::elf::R_X86_64_GOTPC32
                    | object::elf::R_X86_64_GOT64
                    | object::elf::R_X86_64_GOTPCREL64
                    | object::elf::R_X86_64_GOTPC64
                    | object::elf::R_X86_64_GOTPLT64
                    | object::elf::R_X86_64_GOTPCRELX
                    | object::elf::R_X86_64_REX_GOTPCRELX
            )
        } else {
            false
        };
        relocs.push(TextRelocation {
            offset,
            symbol: normalize_symbol_name(&object_format, &name).to_string(),
            is_import,
            addend: relocation.addend(),
            kind: relocation.kind(),
            encoding: relocation.encoding(),
            size: relocation.size(),
            flags: relocation.flags(),
            is_got,
        });
    }

    let mut program = AsmProgram::new(AsmTarget {
        architecture: architecture.clone(),
        object_format: object_format.clone(),
        endianness: AsmEndianness::Little,
        pointer_width: 64,
        default_calling_convention: None,
    });
    program.lifted_from = Some(program.target.clone());

    if !elf_weak_imports.is_empty() {
        let mut seen_undefined: HashSet<String> = HashSet::new();
        for symbol in container.symbols.iter_mut() {
            if symbol.section.is_some() {
                continue;
            }
            let base = symbol
                .name
                .split_once('@')
                .map(|(head, _)| head)
                .unwrap_or(symbol.name.as_str());
            seen_undefined.insert(base.to_string());
            if elf_weak_imports.contains(base) {
                symbol.scope = ContainerSymbolScope::Weak;
            }
        }

        for import in &elf_weak_imports {
            if seen_undefined.contains(import) {
                continue;
            }
            container.symbols.push(ContainerSymbol {
                name: import.clone(),
                kind: ContainerSymbolKind::Other,
                scope: ContainerSymbolScope::Weak,
                section: None,
                value: 0,
                size: 0,
            });
        }
    }
    program.container = Some(container);
    program.sections.push(AsmSection {
        name: ".text".to_string(),
        kind: AsmSectionKind::Text,
        flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
        alignment: Some(16),
    });

    // Lift read-only data symbols as `AsmGlobal` initializers so that
    // RIP-relative references to `.rodata` can be preserved across formats.
    // This is required for simple libc-based compatibility use-cases (e.g.
    // calling `system("ls")`).
    let mut saw_rodata = false;
    let mut rodata_regions: Vec<DataRegion> = Vec::new();
    let mut next_rodata_id = 0usize;
    let mut rodata_cstrings: HashMap<String, String> = HashMap::new();
    let mut rodata_cstrings_by_addr: HashMap<u64, String> = HashMap::new();
    let mut rodata_section_symbol: HashMap<object::SectionIndex, (String, u64)> = HashMap::new();
    for section in file.sections() {
        let section_name = section.name().ok();
        let is_relro = section
            .name()
            .ok()
            .is_some_and(|name| name.starts_with(".data.rel.ro"));
        if section.kind() != object::SectionKind::ReadOnlyData
            && !(is_relro && section.kind() == object::SectionKind::Data)
        {
            continue;
        }
        let section_bytes = match section.data() {
            Ok(bytes) => bytes,
            Err(_) => continue,
        };
        if section_bytes.is_empty() {
            continue;
        }
        let section_addr = section.address();

        // Extract useful NUL-terminated UTF-8 strings even when the executable
        // is stripped. This allows us to lift RIP-relative references to
        // string literals without relying on relocations or symbol tables.
        {
            let mut cursor = 0usize;
            while cursor < section_bytes.len() {
                if cursor < section_bytes.len() && section_bytes[cursor] == 0 {
                    let start = cursor;
                    while cursor < section_bytes.len() && section_bytes[cursor] == 0 {
                        cursor += 1;
                    }
                    // Preserve the address of the first NUL in a run so that
                    // RIP-relative references to `""` literals can be lifted.
                    rodata_cstrings_by_addr
                        .entry(section_addr + start as u64)
                        .or_insert_with(String::new);
                    continue;
                }
                let start = cursor;
                while cursor < section_bytes.len() && section_bytes[cursor] != 0 {
                    cursor += 1;
                }
                if cursor >= section_bytes.len() {
                    break;
                }
                let payload = &section_bytes[start..cursor];
                if !payload.is_empty() && payload.len() <= 4096 {
                    if let Ok(text) = std::str::from_utf8(payload) {
                        rodata_cstrings_by_addr
                            .entry(section_addr + start as u64)
                            .or_insert_with(|| text.to_string());
                    }
                }
                // Also record the terminator itself as an empty string. The
                // compiler may merge `""` with an existing string terminator.
                rodata_cstrings_by_addr
                    .entry(section_addr + cursor as u64)
                    .or_insert_with(String::new);
                cursor += 1;
            }
        }
        let section_index = section.index();

        // Preserve the entire section as a relocatable global so we
        // can model RIP-relative address computations that have no relocations
        // in fully linked binaries (notably ET_DYN PIE inputs).
        //
        // Without this, we end up materializing small absolute immediates
        // (image-relative VAs) that become invalid after transpiling.
        {
            let start = section_addr;
            let end = section_addr.saturating_add(section_bytes.len() as u64);
            let symbol = format!("fp_elf_rodata_{next_rodata_id}");
            next_rodata_id += 1;
            rodata_section_symbol.insert(section.index(), (symbol.clone(), section_addr));
            let mut bytes = section_bytes.to_vec();
            let mut relocations = Vec::new();
            for (offset, relocation) in section.relocations() {
                if relocation.kind() != object::RelocationKind::Absolute {
                    continue;
                }
                if relocation.size() != 64 {
                    continue;
                }

                let (target_symbol, addend) = match relocation.target() {
                    RelocationTarget::Section(target_section) => {
                        let Some((symbol, _)) = rodata_section_symbol.get(&target_section) else {
                            continue;
                        };
                        (symbol.clone(), relocation.addend())
                    }
                    RelocationTarget::Symbol(symbol_index) => {
                        let Ok(target) = file.symbol_by_index(symbol_index) else {
                            continue;
                        };
                        let target_name = target
                            .name()
                            .ok()
                            .filter(|value| !value.is_empty())
                            .map(|value| normalize_symbol_name(&object_format, value).to_string());

                        if let Some(target_section_index) = target.section_index() {
                            if let Some((symbol, base_addr)) =
                                rodata_section_symbol.get(&target_section_index)
                            {
                                let within_section = (target.address() as i64)
                                    .saturating_sub(*base_addr as i64)
                                    .saturating_add(relocation.addend());
                                (symbol.clone(), within_section)
                            } else {
                                let Some(target_name) = target_name else {
                                    continue;
                                };
                                (target_name, relocation.addend())
                            }
                        } else {
                            let Some(target_name) = target_name else {
                                continue;
                            };
                            (target_name, relocation.addend())
                        }
                    }
                    _ => continue,
                };

                if offset.saturating_add(8) <= bytes.len() as u64 {
                    let start = offset as usize;
                    bytes[start..start + 8].fill(0);
                }

                relocations.push(AsmGlobalRelocation {
                    offset,
                    kind: AsmRelocationKind::Abs64,
                    symbol: Name::new(target_symbol),
                    addend,
                });
            }

            rodata_regions.push(DataRegion {
                start,
                end,
                symbol: symbol.clone(),
                bytes: bytes.clone(),
            });
            program.globals.push(AsmGlobal {
                name: Name::new(symbol),
                ty: AsmType::Array(Box::new(AsmType::I8), section_bytes.len() as u64),
                initializer: Some(AsmConstant::Bytes(bytes)),
                relocations,
                section: Some(".rodata".to_string()),
                linkage: Linkage::Internal,
                visibility: Visibility::Default,
                alignment: Some((section.align().max(1)).min(u64::from(u32::MAX)) as u32),
                is_constant: true,
            });
            saw_rodata = true;
        }

        let mut lifted_any_symbol = false;
        for symbol in file
            .symbols()
            .filter(|symbol| symbol.section_index() == Some(section_index))
            .filter(|symbol| symbol.kind() == SymbolKind::Data)
            .filter(|symbol| symbol.size() > 0)
        {
            let Ok(name) = symbol.name() else {
                continue;
            };
            let name = normalize_symbol_name(&object_format, name);
            let symbol_offset = symbol.address().saturating_sub(section_addr) as usize;
            let symbol_size = symbol.size() as usize;
            if symbol_offset >= section_bytes.len()
                || symbol_offset.saturating_add(symbol_size) > section_bytes.len()
            {
                continue;
            }
            let bytes = &section_bytes[symbol_offset..symbol_offset + symbol_size];

            if let Some(text) = bytes.strip_suffix(&[0]).and_then(|payload| {
                if payload.iter().any(|byte| *byte == 0) {
                    return None;
                }
                std::str::from_utf8(payload)
                    .ok()
                    .map(|value| value.to_string())
            }) {
                rodata_cstrings.insert(name.to_string(), text);
            }

            program.globals.push(AsmGlobal {
                name: Name::new(name),
                ty: AsmType::Array(Box::new(AsmType::I8), symbol_size as u64),
                initializer: Some(AsmConstant::Bytes(bytes.to_vec())),
                relocations: Vec::new(),
                section: Some(".rodata".to_string()),
                linkage: Linkage::External,
                visibility: Visibility::Default,
                alignment: Some(1),
                is_constant: true,
            });
            saw_rodata = true;
            lifted_any_symbol = true;
        }

        if !lifted_any_symbol {
            if let Some(section_name) = section_name.as_deref() {
                if relocs.iter().any(|reloc| reloc.symbol == section_name) {
                    program.globals.push(AsmGlobal {
                        name: Name::new(section_name),
                        ty: AsmType::Array(Box::new(AsmType::I8), section_bytes.len() as u64),
                        initializer: Some(AsmConstant::Bytes(section_bytes.to_vec())),
                        relocations: Vec::new(),
                        section: Some(".rodata".to_string()),
                        linkage: Linkage::Internal,
                        visibility: Visibility::Default,
                        alignment: Some(1),
                        is_constant: true,
                    });
                    saw_rodata = true;
                }
            }
        }
    }
    if is_executable
        && matches!(architecture, AsmArchitecture::X86_64)
        && matches!(object_format, AsmObjectFormat::Elf)
    {
        apply_elf_relr_relocations(&file, &mut program, &mut rodata_regions)?;
    }
    if saw_rodata {
        program.sections.push(AsmSection {
            name: ".rodata".to_string(),
            kind: AsmSectionKind::ReadOnlyData,
            flags: vec![AsmSectionFlag::Allocate],
            alignment: Some(16),
        });
    }

    let mut data_regions: Vec<DataRegion> = rodata_regions;
    let mut saw_data = false;
    let mut next_data_id = 0usize;
    for section in file.sections() {
        if !matches!(
            section.kind(),
            object::SectionKind::Data | object::SectionKind::UninitializedData
        ) {
            continue;
        }

        let bytes: Vec<u8> = match section.kind() {
            object::SectionKind::Data => {
                section.data().map(|data| data.to_vec()).unwrap_or_default()
            }
            object::SectionKind::UninitializedData => vec![0u8; section.size() as usize],
            _ => Vec::new(),
        };
        if bytes.is_empty() {
            continue;
        }

        let start = section.address();
        let end = start.saturating_add(bytes.len() as u64);
        let symbol = format!("fp_elf_data_{next_data_id}");
        next_data_id += 1;

        data_regions.push(DataRegion {
            start,
            end,
            symbol: symbol.clone(),
            bytes: bytes.clone(),
        });

        let mut relocations = Vec::new();
        for (offset, relocation) in section.relocations() {
            if relocation.kind() != object::RelocationKind::Absolute {
                continue;
            }
            if relocation.size() != 64 {
                continue;
            }
            let object::RelocationTarget::Symbol(symbol_index) = relocation.target() else {
                continue;
            };
            let Ok(symbol) = file.symbol_by_index(symbol_index) else {
                continue;
            };
            let Ok(name) = symbol.name() else {
                continue;
            };
            let name = normalize_symbol_name(&object_format, name);
            relocations.push(fp_core::asmir::AsmGlobalRelocation {
                offset,
                kind: fp_core::asmir::AsmRelocationKind::Abs64,
                symbol: Name::new(name),
                addend: relocation.addend(),
            });
        }
        program.globals.push(AsmGlobal {
            name: Name::new(symbol.clone()),
            ty: AsmType::Array(Box::new(AsmType::I8), bytes.len() as u64),
            initializer: Some(AsmConstant::Bytes(bytes)),
            relocations,
            section: Some(".data".to_string()),
            linkage: Linkage::Private,
            visibility: Visibility::Default,
            alignment: Some(16),
            is_constant: false,
        });
        saw_data = true;
    }
    if saw_data {
        if !program
            .sections
            .iter()
            .any(|section| section.name == ".data")
        {
            program.sections.push(AsmSection {
                name: ".data".to_string(),
                kind: AsmSectionKind::Data,
                flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Write],
                alignment: Some(16),
            });
        }
    }

    let mut text_symbols = file
        .symbols()
        .filter(|symbol| symbol.section_index() == Some(text_index))
        .filter_map(|symbol| {
            let name = symbol
                .name()
                .ok()
                .map(|raw| Name::new(normalize_symbol_name(&object_format, raw)))
                .unwrap_or_else(|| Name::new("lifted"));
            let section_addr = text_section.address();
            let symbol_offset = symbol.address().saturating_sub(section_addr) as usize;
            Some((name, symbol_offset))
        })
        .collect::<Vec<_>>();
    text_symbols.sort_by_key(|(_, symbol_offset)| *symbol_offset);

    // Some formats (notably Mach-O) do not reliably report symbol sizes.
    // Derive symbol extents from the next symbol or section end.
    let mut sized_text_symbols = Vec::new();
    for idx in 0..text_symbols.len() {
        let (name, symbol_offset) = &text_symbols[idx];
        let next_offset = text_symbols
            .get(idx + 1)
            .map(|(_, offset)| *offset)
            .unwrap_or(text_bytes.len());
        let symbol_size = next_offset.saturating_sub(*symbol_offset);
        if symbol_size == 0 {
            continue;
        }
        sized_text_symbols.push((name.clone(), *symbol_offset, symbol_size));
    }
    let mut text_symbols = sized_text_symbols;

    let mut canonicalize_sysv_main_args = false;
    let mut sysv_main_entry_offset: Option<u64> = None;
    if text_symbols.is_empty() {
        if text_bytes.is_empty() {
            return Err(Error::from("object lift requires non-empty .text section"));
        }

        if is_executable
            && matches!(object_format, AsmObjectFormat::Elf)
            && matches!(architecture, AsmArchitecture::X86_64)
        {
            if let Some(main_offset) = x86_64::find_elf_sysv_main_offset(
                text_bytes,
                text_section.address(),
                file.entry(),
                &elf_rip_symbols,
            ) {
                text_symbols.push((Name::new("main"), 0, text_bytes.len()));
                canonicalize_sysv_main_args = true;
                sysv_main_entry_offset = Some(main_offset as u64);
            }
        }

        if text_symbols.is_empty() {
            text_symbols.push((Name::new("lifted"), 0, text_bytes.len()));
        }
    }

    if let Some(main_entry_offset) = sysv_main_entry_offset {
        let syscall_convention = match (&architecture, &object_format) {
            (AsmArchitecture::X86_64, AsmObjectFormat::Elf) => {
                Some(AsmSyscallConvention::LinuxX86_64)
            }
            (AsmArchitecture::X86_64, AsmObjectFormat::MachO) => {
                Some(AsmSyscallConvention::DarwinX86_64)
            }
            (AsmArchitecture::Aarch64, AsmObjectFormat::Elf) => {
                Some(AsmSyscallConvention::LinuxAarch64)
            }
            (AsmArchitecture::Aarch64, AsmObjectFormat::MachO) => {
                Some(AsmSyscallConvention::DarwinAarch64)
            }
            (AsmArchitecture::X86_64 | AsmArchitecture::Aarch64, AsmObjectFormat::Coff)
            | (AsmArchitecture::X86_64 | AsmArchitecture::Aarch64, AsmObjectFormat::Pe) => None,
            _ => None,
        };

        let calling_convention = match (&architecture, &object_format) {
            (AsmArchitecture::X86_64, AsmObjectFormat::Coff | AsmObjectFormat::Pe) => {
                CallingConvention::Win64
            }
            (AsmArchitecture::X86_64, _) => CallingConvention::X86_64SysV,
            (AsmArchitecture::Aarch64, _) => CallingConvention::AAPCS,
            _ => CallingConvention::C,
        };

        // Lift the Linux SysV `main` as a distinct symbol so that the target
        // platform can provide a native `main` wrapper.
        let mut queue: Vec<(u64, Name)> = vec![(main_entry_offset, Name::new("fp_lifted_main"))];
        let mut seen: HashMap<u64, Name> = HashMap::new();
        seen.insert(main_entry_offset, Name::new("fp_lifted_main"));

        while let Some((entry_offset, name)) = queue.pop() {
            if seen.len() > 10_000 {
                return Err(Error::from("object lift exceeded function discovery limit"));
            }

            let is_entry = name.as_str() == "fp_lifted_main";

            let mut lifted = x86_64::lift_function_bytes_with_symbols(
                text_bytes,
                relocs.as_slice(),
                syscall_convention,
                text_section.address(),
                Some(&elf_rip_symbols),
                Some(&elf_plt_targets),
                Some(&rodata_cstrings),
                Some(&rodata_cstrings_by_addr),
                Some(&data_regions),
                entry_offset,
                is_entry,
                true,
            )?;

            // Ensure stable parameter mapping across lifted SysV functions.
            // The emitters only spill incoming arguments into locals that are
            // marked `is_argument`, so missing this results in null/garbage
            // pointers for functions that expect argument registers (rdi/rsi/...).
            if is_entry {
                canonicalize_x86_sysv_argument_locals(&mut lifted.locals);
            }

            for block in &mut lifted.basic_blocks {
                for inst in &mut block.instructions {
                    if let fp_core::asmir::AsmInstructionKind::Call {
                        calling_convention: cc,
                        ..
                    } = &mut inst.kind
                    {
                        if !matches!(cc, CallingConvention::FpLiftedX86_64RegFile) {
                            *cc = calling_convention.clone();
                        }
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

            let direct_calls = std::mem::take(&mut lifted.direct_call_targets);

            program.functions.push(AsmFunction {
                name: name.clone(),
                signature: AsmFunctionSignature {
                    params: Vec::new(),
                    return_type,
                    is_variadic: false,
                },
                basic_blocks: lifted.basic_blocks,
                locals: lifted.locals,
                stack_slots: lifted.stack_slots,
                frame,
                linkage: Linkage::External,
                visibility: Visibility::Default,
                calling_convention: None,
                section: Some(".text".to_string()),
                is_declaration: false,
            });

            for target in direct_calls {
                if target >= text_bytes.len() as u64 {
                    continue;
                }
                if seen.contains_key(&target) {
                    continue;
                }
                let callee = Name::new(format!("sub_{target:x}"));
                seen.insert(target, callee.clone());
                queue.push((target, callee));
            }
        }

        return Ok(program);
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
                is_import: reloc.is_import,
                addend: reloc.addend,
                kind: reloc.kind,
                encoding: reloc.encoding,
                size: reloc.size,
                flags: reloc.flags,
                is_got: reloc.is_got,
            })
            .collect::<Vec<_>>();

        let syscall_convention = match (&architecture, &object_format) {
            (AsmArchitecture::X86_64, AsmObjectFormat::Elf) => {
                Some(AsmSyscallConvention::LinuxX86_64)
            }
            (AsmArchitecture::X86_64, AsmObjectFormat::MachO) => {
                Some(AsmSyscallConvention::DarwinX86_64)
            }
            (AsmArchitecture::Aarch64, AsmObjectFormat::Elf) => {
                Some(AsmSyscallConvention::LinuxAarch64)
            }
            (AsmArchitecture::Aarch64, AsmObjectFormat::MachO) => {
                Some(AsmSyscallConvention::DarwinAarch64)
            }
            // COFF/PE lifting is supported, but we intentionally do not interpret raw syscalls
            // as a stable Windows mechanism.
            (AsmArchitecture::X86_64 | AsmArchitecture::Aarch64, AsmObjectFormat::Coff)
            | (AsmArchitecture::X86_64 | AsmArchitecture::Aarch64, AsmObjectFormat::Pe) => None,
            _ => None,
        };

        let entry_offset = if name.as_str() == "main" {
            sysv_main_entry_offset.unwrap_or(0)
        } else {
            0
        };

        let mut lifted = match &architecture {
            AsmArchitecture::Aarch64 => {
                aarch64::lift_function_bytes(code, symbol_relocs.as_slice(), syscall_convention)?
            }
            AsmArchitecture::X86_64 => x86_64::lift_function_bytes_with_symbols(
                code,
                symbol_relocs.as_slice(),
                syscall_convention,
                text_section.address() + symbol_offset as u64,
                Some(&elf_rip_symbols),
                Some(&elf_plt_targets),
                Some(&rodata_cstrings),
                Some(&rodata_cstrings_by_addr),
                Some(&data_regions),
                entry_offset,
                false,
                false,
            )?,
            _ => {
                return Err(Error::from(
                    "object lift internal error: unsupported architecture",
                ));
            }
        };

        if canonicalize_sysv_main_args && name.as_str() == "main" {
            canonicalize_x86_sysv_argument_locals(&mut lifted.locals);
        }

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
                    if !matches!(cc, CallingConvention::FpLiftedX86_64RegFile) {
                        *cc = calling_convention.clone();
                    }
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
            stack_slots: lifted.stack_slots,
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

fn build_elf_dynamic_symbol_map(
    bytes: &[u8],
) -> (
    HashMap<u64, RipSymbol>,
    HashSet<String>,
    HashMap<u64, String>,
) {
    let mut out = HashMap::new();
    let mut weak_imports: HashSet<String> = HashSet::new();
    let mut plt_targets: HashMap<u64, String> = HashMap::new();
    let Ok(elf) = goblin::elf::Elf::parse(bytes) else {
        return (out, weak_imports, plt_targets);
    };

    // Best-effort PLT entry mapping.
    //
    // Many ELF executables issue direct `call <plt_entry>` instructions that
    // do not carry relocations at the call site. If we lift those targets as
    // in-image functions we end up translating the PLT resolver stubs (which
    // attempt to patch GOT slots), which is not what we want for cross-target
    // transpilation.
    //
    // Instead, map PLT entry addresses back to their associated dynamic import
    // names using the PLT relocation order and the `.plt` section base.
    let plt_section = elf
        .section_headers
        .iter()
        .filter_map(|header| {
            let name = elf.shdr_strtab.get_at(header.sh_name)?;
            if name == ".plt" || name == ".plt.sec" {
                Some(header)
            } else {
                None
            }
        })
        .next();
    let plt_base = plt_section.map(|header| header.sh_addr);
    let plt_entry_size: u64 = 16;
    let plt_index_by_got_slot: HashMap<u64, usize> = elf
        .pltrelocs
        .iter()
        .enumerate()
        .map(|(index, reloc)| (reloc.r_offset, index))
        .collect();

    for reloc in elf.pltrelocs.iter().chain(elf.dynrelas.iter()) {
        use goblin::elf::sym;

        let sym = reloc.r_sym;
        let Some(symbol) = elf.dynsyms.get(sym) else {
            continue;
        };
        let Some(name) = elf.dynstrtab.get_at(symbol.st_name) else {
            continue;
        };
        if name.is_empty() {
            continue;
        }

        // Dynamic relocations in ELF executables typically describe GOT slots.
        // The relocation offset refers to the slot address, not the final symbol
        // address.
        let import = normalize_elf_import_symbol(name);
        if sym::st_bind(symbol.st_info) == sym::STB_WEAK {
            weak_imports.insert(import.clone());
        }
        out.insert(
            reloc.r_offset,
            RipSymbol {
                name: import.clone(),
                kind: RipSymbolKind::Data,
                import: Some(import.clone()),
                is_got: true,
            },
        );

        if let Some(plt_base) = plt_base {
            if let Some(index) = plt_index_by_got_slot.get(&reloc.r_offset).copied() {
                let entry =
                    plt_base.saturating_add(plt_entry_size.saturating_mul(index as u64 + 1));
                plt_targets.insert(entry, import.clone());
            }
        }
    }

    (out, weak_imports, plt_targets)
}

fn normalize_elf_import_symbol(name: &str) -> String {
    let base = name.split_once('@').map(|(head, _)| head).unwrap_or(name);

    // Keep glibc-style names (for example `__printf_chk`) intact so that
    // platform shims can preserve the original call ABI.
    base.to_string()
}

fn canonicalize_x86_sysv_argument_locals(locals: &mut Vec<AsmLocal>) {
    const ARG_ORDER: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

    for local in locals.iter_mut() {
        let Some(name) = local.name.as_deref() else {
            continue;
        };
        if ARG_ORDER.contains(&name) {
            // Keep / enable for stable ABI mapping.
            local.is_argument = true;
        } else if local.is_argument {
            // Avoid accidental argument shuffling from lifted register discovery order.
            local.is_argument = false;
        }
    }

    let mut reordered = locals.drain(..).enumerate().collect::<Vec<_>>();
    reordered.sort_by_key(|(idx, local)| {
        if !local.is_argument {
            return (1u8, u8::MAX, *idx);
        }
        let pos = local
            .name
            .as_deref()
            .and_then(|name| ARG_ORDER.iter().position(|arg| arg == &name))
            .map(|pos| pos as u8)
            .unwrap_or(u8::MAX);
        (0u8, pos, *idx)
    });
    locals.extend(reordered.into_iter().map(|(_, local)| local));
}

fn apply_elf_relr_relocations(
    file: &object::File<'_>,
    program: &mut AsmProgram,
    rodata_regions: &mut [DataRegion],
) -> Result<()> {
    let Some(relr_section) = file.section_by_name(".relr.dyn") else {
        return Ok(());
    };
    let relr_bytes = relr_section
        .data()
        .map_err(|err| Error::from(err.to_string()))?;
    if relr_bytes.is_empty() || relr_bytes.len() % 8 != 0 {
        return Ok(());
    }

    let mut global_by_name: HashMap<String, usize> = HashMap::new();
    for (idx, global) in program.globals.iter().enumerate() {
        global_by_name.insert(global.name.to_string(), idx);
    }

    let mut relocs = Vec::new();
    let mut cursor = 0usize;
    let mut base: u64 = 0;
    while cursor < relr_bytes.len() {
        let entry = u64::from_le_bytes(relr_bytes[cursor..cursor + 8].try_into().unwrap());
        cursor += 8;
        if entry & 1 == 0 {
            base = entry;
            relocs.push(base);
            base = base.saturating_add(8);
            continue;
        }
        let bitmap = entry;
        for bit in 1..64 {
            if (bitmap & (1u64 << bit)) == 0 {
                continue;
            }
            relocs.push(base.saturating_add((bit as u64 - 1) * 8));
        }
        base = base.saturating_add(8 * 63);
    }

    for reloc_addr in relocs {
        let source_region_idx = match rodata_regions.iter().position(|region| {
            reloc_addr >= region.start && reloc_addr.saturating_add(8) <= region.end
        }) {
            Some(idx) => idx,
            None => continue,
        };
        let source_region_start = rodata_regions[source_region_idx].start;
        let source_offset = (reloc_addr - source_region_start) as usize;
        if source_offset.saturating_add(8) > rodata_regions[source_region_idx].bytes.len() {
            continue;
        }
        let addend = u64::from_le_bytes(
            rodata_regions[source_region_idx].bytes[source_offset..source_offset + 8]
                .try_into()
                .unwrap(),
        );
        if addend == 0 {
            continue;
        }

        let target_region_idx = match rodata_regions
            .iter()
            .position(|region| addend >= region.start && addend < region.end)
        {
            Some(idx) => idx,
            None => continue,
        };
        let target_symbol = rodata_regions[target_region_idx].symbol.clone();
        let target_addend = (addend - rodata_regions[target_region_idx].start) as i64;

        rodata_regions[source_region_idx].bytes[source_offset..source_offset + 8].fill(0);

        let Some(&global_idx) = global_by_name.get(&rodata_regions[source_region_idx].symbol)
        else {
            continue;
        };
        let global = &mut program.globals[global_idx];
        if let Some(AsmConstant::Bytes(bytes)) = global.initializer.as_mut() {
            if source_offset.saturating_add(8) <= bytes.len() {
                bytes[source_offset..source_offset + 8].fill(0);
            }
        }
        global.relocations.push(AsmGlobalRelocation {
            offset: source_offset as u64,
            kind: AsmRelocationKind::Abs64,
            symbol: Name::new(target_symbol),
            addend: target_addend,
        });
    }

    Ok(())
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
