//! Inspect binary artifacts and bytecode files.

use crate::{CliError, Result, cli::CliConfig};
use clap::{Args, ValueEnum};
use object::read::archive::ArchiveFile;
use object::{Architecture, FileKind, Object, ObjectSection, ObjectSymbol, RelocationTarget};
use std::fmt::Write as _;
use std::fs;
use std::path::{Path, PathBuf};

const TEXT_BYTECODE_PREFIX: &str = "fp-bytecode";
const JVM_CLASS_MAGIC: u32 = 0xCAFEBABE;

#[derive(Debug, Clone, Args)]
pub struct InspectArgs {
    /// Input artifact to inspect
    #[arg(required = true)]
    pub input: PathBuf,

    /// Force the input format instead of auto-detecting it
    #[arg(long, value_enum, default_value = "auto")]
    pub format: InspectFormat,

    /// Print a hex dump preview of the file bytes
    #[arg(long)]
    pub hex: bool,

    /// Maximum number of bytes shown by `--hex`
    #[arg(long, default_value_t = 256)]
    pub max_bytes: usize,

    /// Write decoded FerroPhase binary bytecode as text bytecode
    #[arg(long = "export-text-bytecode")]
    pub export_text_bytecode: Option<PathBuf>,
}

fn render_object_summary(file: &object::File<'_>) {
    let mut section_count = 0usize;
    let mut executable_sections = Vec::new();
    for section in file.sections() {
        section_count += 1;
        let name = section.name().unwrap_or("<invalid>");
        let size = section.size();
        let address = section.address();
        let flags = format!("{:?}", section.flags());
        if section.kind() == object::SectionKind::Text {
            executable_sections.push((name.to_string(), address, size));
        }
        println!(
            "section {} addr=0x{:x} size={} kind={:?} flags={}",
            name,
            address,
            size,
            section.kind(),
            flags
        );
    }
    println!("section_count: {}", section_count);

    let mut symbol_count = 0usize;
    let mut exported = Vec::new();
    for symbol in file.symbols() {
        symbol_count += 1;
        if symbol.is_definition() && symbol.is_global() {
            exported.push(symbol.name().unwrap_or("<invalid>").to_string());
        }
    }
    exported.sort();
    exported.truncate(20);
    println!("symbol_count: {}", symbol_count);
    if !exported.is_empty() {
        println!("global_symbols: {}", exported.join(", "));
    }
    if !executable_sections.is_empty() {
        let mut line = String::new();
        for (idx, (name, address, size)) in executable_sections.iter().enumerate() {
            if idx > 0 {
                line.push_str(", ");
            }
            let _ = write!(&mut line, "{}@0x{:x}/{}", name, address, size);
        }
        println!("code_sections: {}", line);
    }
}

fn render_ebpf_object_summary(bytes: &[u8], file: &object::File<'_>) -> Result<()> {
    let metadata = fp_ebpf::read_object_metadata(bytes).map_err(|err| {
        CliError::InvalidInput(format!("failed to decode eBPF metadata: {}", err))
    })?;

    println!("ebpf_metadata: present");
    println!("ebpf_helper_count: {}", metadata.helpers.len());
    for helper in &metadata.helpers {
        println!(
            "ebpf_helper: id={} name={} symbol={}",
            helper.id, helper.name, helper.symbol
        );
    }

    println!("ebpf_format_count: {}", metadata.formats.len());
    for format in &metadata.formats {
        println!("ebpf_format: id={} template={:?}", format.id, format.format);
    }

    println!("ebpf_call_count: {}", metadata.callsites.len());
    for callsite in &metadata.callsites {
        println!(
            "ebpf_call: function={} offset={} helper_id={} helper_symbol={} format_id={} arg_count={}",
            callsite.function,
            callsite.offset,
            callsite.helper_id,
            callsite.helper_symbol,
            callsite
                .format_id
                .map(|value| value.to_string())
                .unwrap_or_else(|| "none".to_string()),
            callsite.arg_count
        );
    }

    for section in file.sections() {
        if section.kind() != object::SectionKind::Text {
            continue;
        }
        let name = section.name().unwrap_or("<invalid>");
        if !name.starts_with("prog/") {
            continue;
        }
        for (offset, relocation) in section.relocations() {
            let RelocationTarget::Symbol(symbol_index) = relocation.target() else {
                continue;
            };
            let symbol = file
                .symbol_by_index(symbol_index)
                .map_err(|err| CliError::InvalidInput(err.to_string()))?;
            println!(
                "ebpf_relocation: section={} offset={} symbol={} kind={:?} encoding={:?} size={}",
                name,
                offset,
                symbol.name().unwrap_or("<invalid>"),
                relocation.kind(),
                relocation.encoding(),
                relocation.size()
            );
        }
    }

    Ok(())
}

fn elf_dynamic_tag_name(tag: i64) -> &'static str {
    match tag {
        0 => "null",
        1 => "needed",
        2 => "pltrelsz",
        3 => "pltgot",
        4 => "hash",
        5 => "strtab",
        6 => "symtab",
        7 => "rela",
        8 => "relasz",
        9 => "relaent",
        10 => "strsz",
        11 => "syment",
        12 => "init",
        13 => "fini",
        14 => "soname",
        15 => "rpath",
        16 => "symbolic",
        17 => "rel",
        18 => "relsz",
        19 => "relent",
        20 => "pltrel",
        21 => "debug",
        22 => "textrel",
        23 => "jmprel",
        29 => "runpath",
        _ => "unknown",
    }
}

fn align_up(value: usize, alignment: usize) -> usize {
    if alignment <= 1 {
        return value;
    }
    (value + (alignment - 1)) & !(alignment - 1)
}

fn read_i64(bytes: &[u8], offset: usize, data: u8) -> Result<i64> {
    match data {
        1 => {
            let raw = bytes.get(offset..offset + 8).ok_or_else(|| {
                CliError::InvalidInput("header read exceeds input size".to_string())
            })?;
            Ok(i64::from_le_bytes(
                raw.try_into().expect("slice length checked"),
            ))
        }
        2 => {
            let raw = bytes.get(offset..offset + 8).ok_or_else(|| {
                CliError::InvalidInput("header read exceeds input size".to_string())
            })?;
            Ok(i64::from_be_bytes(
                raw.try_into().expect("slice length checked"),
            ))
        }
        _ => Err(CliError::InvalidInput(
            "unknown endianness marker".to_string(),
        )),
    }
}

fn pe_data_directory_name(index: usize) -> &'static str {
    match index {
        0 => "export",
        1 => "import",
        2 => "resource",
        3 => "exception",
        4 => "security",
        5 => "basereloc",
        6 => "debug",
        7 => "architecture",
        8 => "globalptr",
        9 => "tls",
        10 => "load-config",
        11 => "bound-import",
        12 => "iat",
        13 => "delay-import",
        14 => "com-descriptor",
        15 => "reserved",
        _ => "unknown",
    }
}

fn elf_program_type_name(kind: u32) -> &'static str {
    match kind {
        0 => "null",
        1 => "load",
        2 => "dynamic",
        3 => "interp",
        4 => "note",
        6 => "phdr",
        7 => "tls",
        _ => "unknown",
    }
}

fn elf_program_flags_name(flags: u32) -> String {
    let mut parts = Vec::new();
    if flags & 0x4 != 0 {
        parts.push("r");
    }
    if flags & 0x2 != 0 {
        parts.push("w");
    }
    if flags & 0x1 != 0 {
        parts.push("x");
    }
    if parts.is_empty() {
        "-".to_string()
    } else {
        parts.join("")
    }
}

fn elf_section_type_name(kind: u32) -> &'static str {
    match kind {
        0 => "null",
        1 => "progbits",
        2 => "symtab",
        3 => "strtab",
        4 => "rela",
        5 => "hash",
        6 => "dynamic",
        7 => "note",
        8 => "nobits",
        9 => "rel",
        11 => "dynsym",
        _ => "unknown",
    }
}

fn render_elf_header(bytes: &[u8]) -> Result<()> {
    if bytes.len() < 0x34 {
        return Err(CliError::InvalidInput(
            "ELF header is truncated".to_string(),
        ));
    }
    let class = bytes[4];
    let data = bytes[5];
    let e_type = read_u16(bytes, 16, data)?;
    let e_machine = read_u16(bytes, 18, data)?;
    let (entry, phoff, shoff, ehsize, phentsize, phnum, shentsize, shnum) = match class {
        1 => (
            read_u32(bytes, 24, data)? as u64,
            read_u32(bytes, 28, data)? as u64,
            read_u32(bytes, 32, data)? as u64,
            read_u16(bytes, 40, data)?,
            read_u16(bytes, 42, data)?,
            read_u16(bytes, 44, data)?,
            read_u16(bytes, 46, data)?,
            read_u16(bytes, 48, data)?,
        ),
        2 => {
            if bytes.len() < 0x40 {
                return Err(CliError::InvalidInput(
                    "ELF64 header is truncated".to_string(),
                ));
            }
            (
                read_u64(bytes, 24, data)?,
                read_u64(bytes, 32, data)?,
                read_u64(bytes, 40, data)?,
                read_u16(bytes, 52, data)?,
                read_u16(bytes, 54, data)?,
                read_u16(bytes, 56, data)?,
                read_u16(bytes, 58, data)?,
                read_u16(bytes, 60, data)?,
            )
        }
        _ => return Err(CliError::InvalidInput("unsupported ELF class".to_string())),
    };

    println!("elf_class: {}", if class == 1 { "ELF32" } else { "ELF64" });
    println!("elf_data: {}", elf_data_name(data));
    println!("elf_osabi: {}", bytes[7]);
    println!("elf_abi_version: {}", bytes[8]);
    println!("elf_type: {}", elf_type_name(e_type));
    println!("elf_machine: {}", elf_machine_name(e_machine));
    println!("elf_entry: 0x{:x}", entry);
    println!("elf_program_header_offset: {}", phoff);
    println!("elf_section_header_offset: {}", shoff);
    println!("elf_header_size: {}", ehsize);
    println!("elf_program_header_entry_size: {}", phentsize);
    println!("elf_program_header_count: {}", phnum);
    println!("elf_section_header_entry_size: {}", shentsize);
    println!("elf_section_header_count: {}", shnum);
    render_elf_program_headers(bytes, class, data, phoff, phentsize, phnum)?;
    render_elf_section_headers(bytes, class, data, shoff, shentsize, shnum)?;
    Ok(())
}

fn render_elf_interp_segment(bytes: &[u8], offset: u64, size: u64) -> Result<()> {
    let start = offset as usize;
    let end = start.saturating_add(size as usize).min(bytes.len());
    if start >= end || start >= bytes.len() {
        println!("    interp: <truncated>");
        return Ok(());
    }
    let raw = &bytes[start..end];
    let nul = raw.iter().position(|byte| *byte == 0).unwrap_or(raw.len());
    let text = String::from_utf8_lossy(&raw[..nul]);
    println!("    interp: {}", text);
    Ok(())
}

fn render_elf_dynamic_segment(bytes: &[u8], data: u8, offset: u64, size: u64) -> Result<()> {
    let start = offset as usize;
    let end = start.saturating_add(size as usize).min(bytes.len());
    if start >= end || start >= bytes.len() {
        println!("    dynamic: <truncated>");
        return Ok(());
    }
    println!("    dynamic:");
    let mut cursor = start;
    let mut index = 0usize;
    while cursor + 16 <= end && index < 24 {
        let tag = read_i64(bytes, cursor, data)?;
        let value = read_u64(bytes, cursor + 8, data)?;
        println!(
            "      dyn#{} tag={} value=0x{:x}",
            index,
            elf_dynamic_tag_name(tag),
            value
        );
        cursor += 16;
        index += 1;
        if tag == 0 {
            break;
        }
    }
    Ok(())
}

fn render_elf_note_segment(bytes: &[u8], data: u8, offset: u64, size: u64) -> Result<()> {
    let start = offset as usize;
    let end = start.saturating_add(size as usize).min(bytes.len());
    if start >= end || start >= bytes.len() {
        println!("    note: <truncated>");
        return Ok(());
    }
    println!("    notes:");
    let mut cursor = start;
    let mut index = 0usize;
    while cursor + 12 <= end && index < 8 {
        let namesz = read_u32(bytes, cursor, data)? as usize;
        let descsz = read_u32(bytes, cursor + 4, data)? as usize;
        let note_type = read_u32(bytes, cursor + 8, data)?;
        cursor += 12;
        if cursor > end {
            println!("      note#{} truncated", index);
            break;
        }
        let name_end = cursor.saturating_add(namesz).min(end);
        let name_bytes = &bytes[cursor..name_end];
        let name_nul = name_bytes
            .iter()
            .position(|byte| *byte == 0)
            .unwrap_or(name_bytes.len());
        let name = String::from_utf8_lossy(&name_bytes[..name_nul]);
        cursor = align_up(cursor + namesz, 4);
        let desc_offset = cursor;
        cursor = align_up(cursor.saturating_add(descsz), 4);
        if desc_offset > end || cursor > end {
            println!("      note#{} truncated", index);
            break;
        }
        println!(
            "      note#{} name={} type={} desc_size={}",
            index, name, note_type, descsz
        );
        index += 1;
    }
    Ok(())
}

fn render_macho_header(bytes: &[u8], kind: FileKind) -> Result<()> {
    if matches!(kind, FileKind::MachOFat32 | FileKind::MachOFat64) {
        if bytes.len() < 8 {
            return Err(CliError::InvalidInput(
                "Mach-O fat header is truncated".to_string(),
            ));
        }
        let nfat_arch = read_u32_be(bytes, 4)?;
        println!("macho_container: fat");
        println!("macho_arch_count: {}", nfat_arch);
        render_macho_fat_arches(bytes, kind, nfat_arch)?;
        return Ok(());
    }

    let is_64 = matches!(kind, FileKind::MachO64);
    let little_endian = matches!(
        &bytes[..4],
        [0xcf, 0xfa, 0xed, 0xfe] | [0xce, 0xfa, 0xed, 0xfe]
    );
    let cputype = read_u32_macho(bytes, 4, little_endian)?;
    let cpusubtype = read_u32_macho(bytes, 8, little_endian)?;
    let filetype = read_u32_macho(bytes, 12, little_endian)?;
    let ncmds = read_u32_macho(bytes, 16, little_endian)?;
    let sizeofcmds = read_u32_macho(bytes, 20, little_endian)?;
    let flags = read_u32_macho(bytes, 24, little_endian)?;
    println!("macho_class: {}", if is_64 { "MachO64" } else { "MachO32" });
    println!(
        "macho_endianness: {}",
        if little_endian { "little" } else { "big" }
    );
    println!("macho_cpu_type: {}", macho_cpu_name(cputype));
    println!("macho_cpu_subtype: 0x{:x}", cpusubtype);
    println!("macho_file_type: {}", macho_filetype_name(filetype));
    println!("macho_load_command_count: {}", ncmds);
    println!("macho_load_commands_size: {}", sizeofcmds);
    println!("macho_flags: 0x{:x}", flags);
    Ok(())
}

fn render_pe_header(bytes: &[u8]) -> Result<()> {
    if bytes.len() < 0x40 || &bytes[..2] != b"MZ" {
        return Err(CliError::InvalidInput(
            "PE header is truncated or missing MZ".to_string(),
        ));
    }
    let pe_offset = read_u32_le(bytes, 0x3c)? as usize;
    if pe_offset + 0x18 > bytes.len() || &bytes[pe_offset..pe_offset + 4] != b"PE\0\0" {
        return Err(CliError::InvalidInput(
            "PE signature is missing or truncated".to_string(),
        ));
    }
    let machine = read_u16_le(bytes, pe_offset + 4)?;
    let section_count = read_u16_le(bytes, pe_offset + 6)?;
    let characteristics = read_u16_le(bytes, pe_offset + 22)?;
    let optional_size = read_u16_le(bytes, pe_offset + 20)? as usize;
    let opt = pe_offset + 24;
    if opt + optional_size > bytes.len() || optional_size < 0x18 {
        return Err(CliError::InvalidInput(
            "PE optional header is truncated".to_string(),
        ));
    }
    let magic = read_u16_le(bytes, opt)?;
    let address_of_entry_point = read_u32_le(bytes, opt + 16)?;
    let image_base = match magic {
        0x10b => read_u32_le(bytes, opt + 28)? as u64,
        0x20b => read_u64_le(bytes, opt + 24)?,
        _ => {
            return Err(CliError::InvalidInput(
                "unknown PE optional header magic".to_string(),
            ));
        }
    };
    let subsystem_offset = if magic == 0x10b { opt + 68 } else { opt + 88 };
    let subsystem = read_u16_le(bytes, subsystem_offset)?;
    println!(
        "pe_magic: {}",
        if magic == 0x10b { "PE32" } else { "PE32+" }
    );
    println!("pe_coff_machine: {}", pe_machine_name(machine));
    println!("pe_section_count: {}", section_count);
    println!("pe_characteristics: 0x{:x}", characteristics);
    println!("pe_entry_rva: 0x{:x}", address_of_entry_point);
    println!("pe_image_base: 0x{:x}", image_base);
    println!("pe_subsystem: {}", pe_subsystem_name(subsystem));
    let data_directories = render_pe_data_directories(bytes, opt, magic, optional_size)?;
    let sections = parse_pe_sections(bytes, pe_offset, section_count, optional_size)?;
    render_pe_sections(&sections);
    render_pe_export_directory(
        bytes,
        &sections,
        data_directories.get(0).copied().unwrap_or_default(),
    )?;
    render_pe_import_directory(
        bytes,
        &sections,
        data_directories.get(1).copied().unwrap_or_default(),
    )?;
    Ok(())
}

fn render_elf_program_headers(
    bytes: &[u8],
    class: u8,
    data: u8,
    phoff: u64,
    phentsize: u16,
    phnum: u16,
) -> Result<()> {
    if phoff == 0 || phentsize == 0 || phnum == 0 {
        return Ok(());
    }
    let phoff = phoff as usize;
    let phentsize = phentsize as usize;
    println!("elf_program_headers:");
    for index in 0..usize::from(phnum).min(16) {
        let base = phoff + index * phentsize;
        if base + phentsize > bytes.len() {
            println!("  phdr#{} truncated", index);
            break;
        }
        let (p_type, p_flags, p_offset, p_vaddr, p_filesz, p_memsz, p_align) = match class {
            1 => (
                read_u32(bytes, base, data)?,
                read_u32(bytes, base + 24, data)?,
                read_u32(bytes, base + 4, data)? as u64,
                read_u32(bytes, base + 8, data)? as u64,
                read_u32(bytes, base + 16, data)? as u64,
                read_u32(bytes, base + 20, data)? as u64,
                read_u32(bytes, base + 28, data)? as u64,
            ),
            2 => (
                read_u32(bytes, base, data)?,
                read_u32(bytes, base + 4, data)?,
                read_u64(bytes, base + 8, data)?,
                read_u64(bytes, base + 16, data)?,
                read_u64(bytes, base + 32, data)?,
                read_u64(bytes, base + 40, data)?,
                read_u64(bytes, base + 48, data)?,
            ),
            _ => return Err(CliError::InvalidInput("unsupported ELF class".to_string())),
        };
        println!(
            "  phdr#{} type={} flags={} offset=0x{:x} vaddr=0x{:x} filesz={} memsz={} align={}",
            index,
            elf_program_type_name(p_type),
            elf_program_flags_name(p_flags),
            p_offset,
            p_vaddr,
            p_filesz,
            p_memsz,
            p_align
        );
        match p_type {
            2 => render_elf_dynamic_segment(bytes, data, p_offset, p_filesz)?,
            3 => render_elf_interp_segment(bytes, p_offset, p_filesz)?,
            4 => render_elf_note_segment(bytes, data, p_offset, p_filesz)?,
            _ => {}
        }
    }
    Ok(())
}

fn render_elf_section_headers(
    bytes: &[u8],
    class: u8,
    data: u8,
    shoff: u64,
    shentsize: u16,
    shnum: u16,
) -> Result<()> {
    if shoff == 0 || shentsize == 0 || shnum == 0 {
        return Ok(());
    }
    let shoff = shoff as usize;
    let shentsize = shentsize as usize;
    println!("elf_section_headers:");
    for index in 0..usize::from(shnum).min(16) {
        let base = shoff + index * shentsize;
        if base + shentsize > bytes.len() {
            println!("  shdr#{} truncated", index);
            break;
        }
        let (sh_type, sh_offset, sh_size, sh_flags) = match class {
            1 => (
                read_u32(bytes, base + 4, data)?,
                read_u32(bytes, base + 16, data)? as u64,
                read_u32(bytes, base + 20, data)? as u64,
                read_u32(bytes, base + 8, data)? as u64,
            ),
            2 => (
                read_u32(bytes, base + 4, data)?,
                read_u64(bytes, base + 24, data)?,
                read_u64(bytes, base + 32, data)?,
                read_u64(bytes, base + 8, data)?,
            ),
            _ => return Err(CliError::InvalidInput("unsupported ELF class".to_string())),
        };
        println!(
            "  shdr#{} type={} flags=0x{:x} offset=0x{:x} size={}",
            index,
            elf_section_type_name(sh_type),
            sh_flags,
            sh_offset,
            sh_size
        );
    }
    Ok(())
}

fn render_macho_fat_arches(bytes: &[u8], kind: FileKind, nfat_arch: u32) -> Result<()> {
    let arch_size = if matches!(kind, FileKind::MachOFat64) {
        32usize
    } else {
        20usize
    };
    let base = 8usize;
    println!("macho_arches:");
    for index in 0..(nfat_arch as usize).min(16) {
        let off = base + index * arch_size;
        if off + arch_size > bytes.len() {
            println!("  arch#{} truncated", index);
            break;
        }
        let cputype = read_u32_be(bytes, off)?;
        let cpusubtype = read_u32_be(bytes, off + 4)?;
        if arch_size == 20 {
            let offset = read_u32_be(bytes, off + 8)? as u64;
            let size = read_u32_be(bytes, off + 12)? as u64;
            let align = read_u32_be(bytes, off + 16)?;
            println!(
                "  arch#{} cpu={} subtype=0x{:x} offset=0x{:x} size={} align={}",
                index,
                macho_cpu_name(cputype),
                cpusubtype,
                offset,
                size,
                align
            );
        } else {
            let offset = read_u64_be(bytes, off + 8)?;
            let size = read_u64_be(bytes, off + 16)?;
            let align = read_u32_be(bytes, off + 24)?;
            println!(
                "  arch#{} cpu={} subtype=0x{:x} offset=0x{:x} size={} align={}",
                index,
                macho_cpu_name(cputype),
                cpusubtype,
                offset,
                size,
                align
            );
        }
    }
    Ok(())
}

fn render_pe_data_directories(
    bytes: &[u8],
    optional_header_offset: usize,
    magic: u16,
    optional_size: usize,
) -> Result<Vec<PeDataDirectory>> {
    let data_dir_offset = if magic == 0x10b {
        optional_header_offset + 96
    } else {
        optional_header_offset + 112
    };
    if data_dir_offset > optional_header_offset + optional_size {
        return Ok(Vec::new());
    }
    let available = optional_header_offset + optional_size - data_dir_offset;
    let count = (available / 8).min(16);
    if count == 0 {
        return Ok(Vec::new());
    }
    println!("pe_data_directories:");
    let mut directories = Vec::with_capacity(count);
    for index in 0..count {
        let off = data_dir_offset + index * 8;
        let rva = read_u32_le(bytes, off)?;
        let size = read_u32_le(bytes, off + 4)?;
        directories.push(PeDataDirectory { rva, size });
        println!(
            "  dir#{} kind={} rva=0x{:x} size={}",
            index,
            pe_data_directory_name(index),
            rva,
            size
        );
    }
    Ok(directories)
}

#[derive(Debug, Clone, Copy, Default)]
struct PeDataDirectory {
    rva: u32,
    size: u32,
}

#[derive(Debug, Clone)]
struct PeSectionHeader {
    name: String,
    virtual_size: u32,
    virtual_address: u32,
    raw_size: u32,
    raw_offset: u32,
    characteristics: u32,
}

fn parse_pe_sections(
    bytes: &[u8],
    pe_offset: usize,
    section_count: u16,
    optional_size: usize,
) -> Result<Vec<PeSectionHeader>> {
    let section_table = pe_offset + 24 + optional_size;
    let mut sections = Vec::with_capacity(section_count as usize);
    for index in 0..section_count as usize {
        let off = section_table + index * 40;
        if off + 40 > bytes.len() {
            return Err(CliError::InvalidInput(
                "PE section table is truncated".to_string(),
            ));
        }
        let name_raw = &bytes[off..off + 8];
        let name_len = name_raw
            .iter()
            .position(|byte| *byte == 0)
            .unwrap_or(name_raw.len());
        sections.push(PeSectionHeader {
            name: String::from_utf8_lossy(&name_raw[..name_len]).to_string(),
            virtual_size: read_u32_le(bytes, off + 8)?,
            virtual_address: read_u32_le(bytes, off + 12)?,
            raw_size: read_u32_le(bytes, off + 16)?,
            raw_offset: read_u32_le(bytes, off + 20)?,
            characteristics: read_u32_le(bytes, off + 36)?,
        });
    }
    Ok(sections)
}

fn render_pe_sections(sections: &[PeSectionHeader]) {
    println!("pe_sections:");
    for (index, section) in sections.iter().take(16).enumerate() {
        println!(
            "  section#{} name={} va=0x{:x} vsz={} raw_off=0x{:x} raw_sz={} chars=0x{:x}",
            index,
            section.name,
            section.virtual_address,
            section.virtual_size,
            section.raw_offset,
            section.raw_size,
            section.characteristics
        );
    }
}

fn render_pe_export_directory(
    bytes: &[u8],
    sections: &[PeSectionHeader],
    directory: PeDataDirectory,
) -> Result<()> {
    if directory.rva == 0 || directory.size == 0 {
        return Ok(());
    }
    let Some(offset) = pe_rva_to_offset(sections, directory.rva) else {
        println!("pe_exports: unresolved_rva=0x{:x}", directory.rva);
        return Ok(());
    };
    if offset + 40 > bytes.len() {
        println!("pe_exports: truncated");
        return Ok(());
    }
    let name_rva = read_u32_le(bytes, offset + 12)?;
    let ordinal_base = read_u32_le(bytes, offset + 16)?;
    let address_table_entries = read_u32_le(bytes, offset + 20)?;
    let number_of_name_pointers = read_u32_le(bytes, offset + 24)?;
    let export_address_table_rva = read_u32_le(bytes, offset + 28)?;
    let name_pointer_rva = read_u32_le(bytes, offset + 32)?;
    let ordinal_table_rva = read_u32_le(bytes, offset + 36)?;
    let dll_name =
        pe_read_c_string(bytes, sections, name_rva).unwrap_or_else(|| "<invalid>".to_string());
    println!(
        "pe_exports: dll={} ordinal_base={} address_count={} named_count={}",
        dll_name, ordinal_base, address_table_entries, number_of_name_pointers
    );
    let Some(name_ptr_off) = pe_rva_to_offset(sections, name_pointer_rva) else {
        return Ok(());
    };
    let Some(ord_table_off) = pe_rva_to_offset(sections, ordinal_table_rva) else {
        return Ok(());
    };
    let Some(addr_table_off) = pe_rva_to_offset(sections, export_address_table_rva) else {
        return Ok(());
    };
    for index in 0..(number_of_name_pointers as usize).min(16) {
        let name_rva = read_u32_le(bytes, name_ptr_off + index * 4)?;
        let ordinal_index = read_u16_le(bytes, ord_table_off + index * 2)? as u32;
        let func_rva = read_u32_le(bytes, addr_table_off + ordinal_index as usize * 4)?;
        let name =
            pe_read_c_string(bytes, sections, name_rva).unwrap_or_else(|| "<invalid>".to_string());
        println!(
            "  export#{} name={} ordinal={} rva=0x{:x}",
            index,
            name,
            ordinal_base + ordinal_index,
            func_rva
        );
    }
    Ok(())
}

fn render_pe_import_directory(
    bytes: &[u8],
    sections: &[PeSectionHeader],
    directory: PeDataDirectory,
) -> Result<()> {
    if directory.rva == 0 || directory.size == 0 {
        return Ok(());
    }
    let Some(mut offset) = pe_rva_to_offset(sections, directory.rva) else {
        println!("pe_imports: unresolved_rva=0x{:x}", directory.rva);
        return Ok(());
    };
    println!("pe_imports:");
    for dll_index in 0..16usize {
        if offset + 20 > bytes.len() {
            println!("  import#{} truncated", dll_index);
            break;
        }
        let original_first_thunk = read_u32_le(bytes, offset)?;
        let _timestamp = read_u32_le(bytes, offset + 4)?;
        let _forwarder_chain = read_u32_le(bytes, offset + 8)?;
        let name_rva = read_u32_le(bytes, offset + 12)?;
        let first_thunk = read_u32_le(bytes, offset + 16)?;
        if original_first_thunk == 0 && name_rva == 0 && first_thunk == 0 {
            break;
        }
        let dll_name =
            pe_read_c_string(bytes, sections, name_rva).unwrap_or_else(|| "<invalid>".to_string());
        println!("  dll#{} name={}", dll_index, dll_name);
        let thunk_rva = if original_first_thunk != 0 {
            original_first_thunk
        } else {
            first_thunk
        };
        render_pe_import_thunks(bytes, sections, thunk_rva)?;
        offset += 20;
    }
    Ok(())
}

fn render_pe_import_thunks(
    bytes: &[u8],
    sections: &[PeSectionHeader],
    thunk_rva: u32,
) -> Result<()> {
    let Some(mut thunk_off) = pe_rva_to_offset(sections, thunk_rva) else {
        return Ok(());
    };
    for index in 0..16usize {
        if thunk_off + 8 > bytes.len() {
            break;
        }
        let entry = read_u64_le(bytes, thunk_off)?;
        if entry == 0 {
            break;
        }
        if entry & (1u64 << 63) != 0 {
            println!("    import#{} ordinal={}", index, entry & 0xffff);
        } else {
            let hint_name_rva = entry as u32;
            if let Some(name_off) = pe_rva_to_offset(sections, hint_name_rva) {
                let hint = read_u16_le(bytes, name_off)?;
                let name = read_null_terminated_string(bytes, name_off + 2)
                    .unwrap_or_else(|| "<invalid>".to_string());
                println!("    import#{} name={} hint={}", index, name, hint);
            }
        }
        thunk_off += 8;
    }
    Ok(())
}

fn pe_rva_to_offset(sections: &[PeSectionHeader], rva: u32) -> Option<usize> {
    sections.iter().find_map(|section| {
        let span = section.virtual_size.max(section.raw_size);
        let start = section.virtual_address;
        let end = start.checked_add(span)?;
        if rva >= start && rva < end {
            let delta = rva - start;
            Some(section.raw_offset as usize + delta as usize)
        } else {
            None
        }
    })
}

fn pe_read_c_string(bytes: &[u8], sections: &[PeSectionHeader], rva: u32) -> Option<String> {
    let offset = pe_rva_to_offset(sections, rva)?;
    read_null_terminated_string(bytes, offset)
}

fn read_null_terminated_string(bytes: &[u8], offset: usize) -> Option<String> {
    let tail = bytes.get(offset..)?;
    let len = tail
        .iter()
        .position(|byte| *byte == 0)
        .unwrap_or(tail.len());
    Some(String::from_utf8_lossy(&tail[..len]).to_string())
}

fn read_u16(bytes: &[u8], offset: usize, data: u8) -> Result<u16> {
    match data {
        1 => read_u16_le(bytes, offset),
        2 => read_u16_be(bytes, offset),
        _ => Err(CliError::InvalidInput(
            "unknown endianness marker".to_string(),
        )),
    }
}

fn read_u32(bytes: &[u8], offset: usize, data: u8) -> Result<u32> {
    match data {
        1 => read_u32_le(bytes, offset),
        2 => read_u32_be(bytes, offset),
        _ => Err(CliError::InvalidInput(
            "unknown endianness marker".to_string(),
        )),
    }
}

fn read_u64(bytes: &[u8], offset: usize, data: u8) -> Result<u64> {
    match data {
        1 => read_u64_le(bytes, offset),
        2 => read_u64_be(bytes, offset),
        _ => Err(CliError::InvalidInput(
            "unknown endianness marker".to_string(),
        )),
    }
}

fn read_u16_le(bytes: &[u8], offset: usize) -> Result<u16> {
    let raw = bytes
        .get(offset..offset + 2)
        .ok_or_else(|| CliError::InvalidInput("header read exceeds input size".to_string()))?;
    Ok(u16::from_le_bytes(
        raw.try_into().expect("slice length checked"),
    ))
}

fn read_u16_be(bytes: &[u8], offset: usize) -> Result<u16> {
    let raw = bytes
        .get(offset..offset + 2)
        .ok_or_else(|| CliError::InvalidInput("header read exceeds input size".to_string()))?;
    Ok(u16::from_be_bytes(
        raw.try_into().expect("slice length checked"),
    ))
}

fn read_u32_le(bytes: &[u8], offset: usize) -> Result<u32> {
    let raw = bytes
        .get(offset..offset + 4)
        .ok_or_else(|| CliError::InvalidInput("header read exceeds input size".to_string()))?;
    Ok(u32::from_le_bytes(
        raw.try_into().expect("slice length checked"),
    ))
}

fn read_u32_be(bytes: &[u8], offset: usize) -> Result<u32> {
    let raw = bytes
        .get(offset..offset + 4)
        .ok_or_else(|| CliError::InvalidInput("header read exceeds input size".to_string()))?;
    Ok(u32::from_be_bytes(
        raw.try_into().expect("slice length checked"),
    ))
}

fn read_u64_le(bytes: &[u8], offset: usize) -> Result<u64> {
    let raw = bytes
        .get(offset..offset + 8)
        .ok_or_else(|| CliError::InvalidInput("header read exceeds input size".to_string()))?;
    Ok(u64::from_le_bytes(
        raw.try_into().expect("slice length checked"),
    ))
}

fn read_u64_be(bytes: &[u8], offset: usize) -> Result<u64> {
    let raw = bytes
        .get(offset..offset + 8)
        .ok_or_else(|| CliError::InvalidInput("header read exceeds input size".to_string()))?;
    Ok(u64::from_be_bytes(
        raw.try_into().expect("slice length checked"),
    ))
}

fn read_u32_macho(bytes: &[u8], offset: usize, little_endian: bool) -> Result<u32> {
    if little_endian {
        read_u32_le(bytes, offset)
    } else {
        read_u32_be(bytes, offset)
    }
}

fn elf_data_name(data: u8) -> &'static str {
    match data {
        1 => "little",
        2 => "big",
        _ => "unknown",
    }
}

fn elf_type_name(kind: u16) -> &'static str {
    match kind {
        1 => "relocatable",
        2 => "executable",
        3 => "shared",
        4 => "core",
        _ => "unknown",
    }
}

fn elf_machine_name(machine: u16) -> &'static str {
    match machine {
        0x03 => "x86",
        0x3e => "x86_64",
        0x28 => "arm",
        0xb7 => "aarch64",
        0xf3 => "riscv",
        _ => "unknown",
    }
}

fn macho_cpu_name(cpu: u32) -> &'static str {
    match cpu {
        7 | 0x01000007 => "x86_64",
        12 => "arm",
        0x0100000c => "arm64",
        _ => "unknown",
    }
}

fn macho_filetype_name(filetype: u32) -> &'static str {
    match filetype {
        1 => "object",
        2 => "execute",
        6 => "dylib",
        8 => "bundle",
        10 => "dSYM",
        _ => "unknown",
    }
}

fn pe_machine_name(machine: u16) -> &'static str {
    match machine {
        0x014c => "x86",
        0x8664 => "x86_64",
        0x01c0 => "arm",
        0xaa64 => "arm64",
        _ => "unknown",
    }
}

fn pe_subsystem_name(subsystem: u16) -> &'static str {
    match subsystem {
        1 => "native",
        2 => "windows-gui",
        3 => "windows-cui",
        9 => "windows-ce-gui",
        10 => "efi-application",
        14 => "xbox",
        16 => "windows-boot",
        _ => "unknown",
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum InspectFormat {
    Auto,
    Raw,
    BytecodeBinary,
    BytecodeText,
    Native,
    Archive,
    Wasm,
    JvmClass,
}

pub async fn inspect_command(args: InspectArgs, _config: &CliConfig) -> Result<()> {
    let bytes = fs::read(&args.input).map_err(CliError::Io)?;
    let detected = probe_format(&args.input, &bytes, args.format)?;

    match detected {
        ProbedFormat::BytecodeBinary => {
            let file = decode_binary_bytecode(&bytes)?;
            render_bytecode_binary(&args.input, &file, &args)?;
        }
        ProbedFormat::BytecodeText => {
            let program = decode_text_bytecode(&bytes)?;
            render_bytecode_text(&args.input, &program, &args)?;
        }
        ProbedFormat::JvmClass(header) => {
            render_jvm_class(&args.input, &bytes, &header, &args);
        }
        ProbedFormat::Archive => {
            let archive = parse_archive(&bytes)?;
            render_archive(&args.input, &bytes, &archive, &args)?;
        }
        ProbedFormat::Wasm => {
            render_wasm(&args.input, &bytes, &args)?;
        }
        ProbedFormat::Native(kind) => {
            let file = parse_native(&bytes).ok();
            render_native(&args.input, &bytes, kind, file.as_ref(), &args)?;
        }
        ProbedFormat::Raw => {
            render_raw(&args.input, &bytes, &args);
        }
    }

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ProbedFormat {
    BytecodeBinary,
    BytecodeText,
    JvmClass(JvmClassHeader),
    Archive,
    Wasm,
    Native(FileKind),
    Raw,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct JvmClassHeader {
    minor_version: u16,
    major_version: u16,
    constant_pool_count: u16,
}

fn probe_format(path: &Path, bytes: &[u8], requested: InspectFormat) -> Result<ProbedFormat> {
    match requested {
        InspectFormat::BytecodeBinary => return Ok(ProbedFormat::BytecodeBinary),
        InspectFormat::BytecodeText => return Ok(ProbedFormat::BytecodeText),
        InspectFormat::JvmClass => {
            return probe_jvm_class(bytes)
                .map(ProbedFormat::JvmClass)
                .ok_or_else(|| {
                    CliError::InvalidInput("input is not a JVM class file".to_string())
                });
        }
        InspectFormat::Archive => {
            return if has_archive_header(bytes) {
                Ok(ProbedFormat::Archive)
            } else {
                Err(CliError::InvalidInput(
                    "input is not an archive file".to_string(),
                ))
            };
        }
        InspectFormat::Wasm => {
            return if has_wasm_header(bytes) {
                Ok(ProbedFormat::Wasm)
            } else {
                Err(CliError::InvalidInput(
                    "input is not a WebAssembly module".to_string(),
                ))
            };
        }
        InspectFormat::Native => {
            let kind = probe_object_kind(bytes).ok_or_else(|| {
                CliError::InvalidInput("input is not a supported native/object file".to_string())
            })?;
            return match kind {
                FileKind::Archive => Err(CliError::InvalidInput(
                    "input is an archive; use --format archive, or auto".to_string(),
                )),
                _ => Ok(ProbedFormat::Native(kind)),
            };
        }
        InspectFormat::Raw => return Ok(ProbedFormat::Raw),
        InspectFormat::Auto => {}
    }

    let extension = path
        .extension()
        .and_then(|ext| ext.to_str())
        .unwrap_or_default();
    if extension == "fbc" || has_bytecode_binary_header(bytes) {
        return Ok(ProbedFormat::BytecodeBinary);
    }
    if extension == "ftbc" || has_bytecode_text_header(bytes) {
        return Ok(ProbedFormat::BytecodeText);
    }
    if let Some(header) = probe_jvm_class(bytes) {
        return Ok(ProbedFormat::JvmClass(header));
    }
    if has_archive_header(bytes) {
        return Ok(ProbedFormat::Archive);
    }
    if has_wasm_header(bytes) {
        return Ok(ProbedFormat::Wasm);
    }
    if let Some(kind) = probe_object_kind(bytes) {
        return Ok(match kind {
            other => ProbedFormat::Native(other),
        });
    }
    Ok(ProbedFormat::Raw)
}

fn has_bytecode_binary_header(bytes: &[u8]) -> bool {
    bytes.len() >= 4 && bytes[..4] == fp_bytecode::BYTECODE_MAGIC
}

fn has_bytecode_text_header(bytes: &[u8]) -> bool {
    match std::str::from_utf8(bytes) {
        Ok(text) => text.trim_start().starts_with(TEXT_BYTECODE_PREFIX),
        Err(_) => false,
    }
}

fn probe_jvm_class(bytes: &[u8]) -> Option<JvmClassHeader> {
    if bytes.len() < 10 {
        return None;
    }
    let magic = u32::from_be_bytes(bytes[0..4].try_into().ok()?);
    if magic != JVM_CLASS_MAGIC {
        return None;
    }
    let minor_version = u16::from_be_bytes(bytes[4..6].try_into().ok()?);
    let major_version = u16::from_be_bytes(bytes[6..8].try_into().ok()?);
    let constant_pool_count = u16::from_be_bytes(bytes[8..10].try_into().ok()?);
    if !(45..=80).contains(&major_version) {
        return None;
    }
    Some(JvmClassHeader {
        minor_version,
        major_version,
        constant_pool_count,
    })
}

fn probe_object_kind(bytes: &[u8]) -> Option<FileKind> {
    FileKind::parse(bytes).ok()
}

fn has_archive_header(bytes: &[u8]) -> bool {
    bytes.starts_with(b"!<arch>\n") || bytes.starts_with(b"!<thin>\n")
}

fn has_wasm_header(bytes: &[u8]) -> bool {
    bytes.len() >= 8 && bytes[0..4] == [0x00, b'a', b's', b'm']
}

fn decode_binary_bytecode(bytes: &[u8]) -> Result<fp_bytecode::BytecodeFile> {
    fp_bytecode::decode_file(bytes).map_err(|err| {
        CliError::InvalidInput(format!(
            "failed to decode FerroPhase binary bytecode: {err}"
        ))
    })
}

fn decode_text_bytecode(bytes: &[u8]) -> Result<fp_bytecode::BytecodeProgram> {
    let text = std::str::from_utf8(bytes).map_err(|err| {
        CliError::InvalidInput(format!("failed to decode text bytecode as UTF-8: {err}"))
    })?;
    fp_bytecode::parse_program(text).map_err(|err| {
        CliError::InvalidInput(format!("failed to parse FerroPhase text bytecode: {err}"))
    })
}

fn parse_native(bytes: &[u8]) -> Result<object::File<'_>> {
    object::File::parse(bytes).map_err(|err| {
        CliError::InvalidInput(format!("failed to parse native/object binary: {err}"))
    })
}

fn parse_archive(bytes: &[u8]) -> Result<ArchiveFile<'_>> {
    ArchiveFile::parse(bytes)
        .map_err(|err| CliError::InvalidInput(format!("failed to parse archive: {err}")))
}

fn render_bytecode_binary(
    path: &Path,
    file: &fp_bytecode::BytecodeFile,
    args: &InspectArgs,
) -> Result<()> {
    println!("format: ferro-bytecode-binary");
    println!("path: {}", path.display());
    println!("version: {}", file.version);
    println!(
        "entry: {}",
        file.program.entry.as_deref().unwrap_or("<none>")
    );
    println!("const_count: {}", file.program.const_pool.len());
    println!("function_count: {}", file.program.functions.len());
    for function in &file.program.functions {
        println!(
            "function {} params={} locals={} blocks={}",
            function.name,
            function.params,
            function.locals,
            function.blocks.len()
        );
    }

    if let Some(output) = &args.export_text_bytecode {
        let rendered = fp_bytecode::format_program(&file.program);
        fs::write(output, rendered).map_err(CliError::Io)?;
        println!("exported_text_bytecode: {}", output.display());
    }

    if args.hex {
        let bytes = fp_bytecode::encode_file(&file.program).map_err(|err| {
            CliError::Compilation(format!("failed to re-encode bytecode for hex dump: {err}"))
        })?;
        print_hex_dump(&bytes, args.max_bytes);
    }

    Ok(())
}

fn render_bytecode_text(
    path: &Path,
    program: &fp_bytecode::BytecodeProgram,
    args: &InspectArgs,
) -> Result<()> {
    println!("format: ferro-bytecode-text");
    println!("path: {}", path.display());
    println!("entry: {}", program.entry.as_deref().unwrap_or("<none>"));
    println!("const_count: {}", program.const_pool.len());
    println!("function_count: {}", program.functions.len());
    for function in &program.functions {
        println!(
            "function {} params={} locals={} blocks={}",
            function.name,
            function.params,
            function.locals,
            function.blocks.len()
        );
    }

    if let Some(output) = &args.export_text_bytecode {
        fs::write(output, fp_bytecode::format_program(program)).map_err(CliError::Io)?;
        println!("exported_text_bytecode: {}", output.display());
    }

    if args.hex {
        let rendered = fp_bytecode::format_program(program);
        print_hex_dump(rendered.as_bytes(), args.max_bytes);
    }

    Ok(())
}

fn render_jvm_class(path: &Path, bytes: &[u8], header: &JvmClassHeader, args: &InspectArgs) {
    println!("format: jvm-class");
    println!("path: {}", path.display());
    println!("version: {}.{}", header.major_version, header.minor_version);
    println!("constant_pool_count: {}", header.constant_pool_count);
    println!("size: {}", bytes.len());
    if args.hex {
        print_hex_dump(bytes, args.max_bytes);
    }
}

fn render_archive(
    path: &Path,
    bytes: &[u8],
    archive: &ArchiveFile<'_>,
    args: &InspectArgs,
) -> Result<()> {
    println!("format: archive");
    println!("path: {}", path.display());
    println!("archive_kind: {:?}", archive.kind());
    println!("is_thin: {}", archive.is_thin());

    let mut member_count = 0usize;
    let mut member_names = Vec::new();
    for member in archive.members() {
        let member = member.map_err(|err| {
            CliError::InvalidInput(format!("failed to read archive member: {err}"))
        })?;
        member_count += 1;
        if member_names.len() < 20 {
            member_names.push(String::from_utf8_lossy(member.name()).to_string());
        }
    }
    println!("member_count: {}", member_count);
    if !member_names.is_empty() {
        println!("members: {}", member_names.join(", "));
    }
    if args.hex {
        print_hex_dump(bytes, args.max_bytes);
    }
    Ok(())
}

fn render_wasm(path: &Path, bytes: &[u8], args: &InspectArgs) -> Result<()> {
    let version = u32::from_le_bytes(bytes[4..8].try_into().map_err(|_| {
        CliError::InvalidInput("wasm header is truncated; missing version field".to_string())
    })?);

    println!("format: wasm");
    println!("path: {}", path.display());
    println!("wasm_version: {}", version);
    println!("architecture: Wasm");
    println!("endianness: Little");

    let sections = scan_wasm_sections(bytes)?;
    let section_count = sections.len();
    for (index, section) in sections.iter().enumerate() {
        println!(
            "section #{} id={} kind={} offset={} size={}",
            index,
            section.id,
            wasm_section_name(section.id),
            section.offset,
            section.size
        );
    }
    println!("section_count: {}", section_count);
    if args.hex {
        print_hex_dump(bytes, args.max_bytes);
    }
    Ok(())
}

#[derive(Debug, Clone, Copy)]
struct WasmSectionHeader {
    id: u8,
    offset: usize,
    size: u32,
}

fn scan_wasm_sections(bytes: &[u8]) -> Result<Vec<WasmSectionHeader>> {
    if bytes.len() < 8 {
        return Err(CliError::InvalidInput(
            "wasm file is shorter than header".to_string(),
        ));
    }
    let mut offset = 8usize;
    let mut sections = Vec::new();
    while offset < bytes.len() {
        let id = *bytes
            .get(offset)
            .ok_or_else(|| CliError::InvalidInput("truncated wasm section id".to_string()))?;
        offset += 1;
        let (size, leb_len) = decode_u32_leb128(&bytes[offset..])?;
        offset += leb_len;
        let payload_offset = offset;
        let end = payload_offset
            .checked_add(size as usize)
            .ok_or_else(|| CliError::InvalidInput("wasm section size overflow".to_string()))?;
        if end > bytes.len() {
            return Err(CliError::InvalidInput(
                "wasm section exceeds input size".to_string(),
            ));
        }
        sections.push(WasmSectionHeader {
            id,
            offset: payload_offset,
            size,
        });
        offset = end;
    }
    Ok(sections)
}

fn decode_u32_leb128(bytes: &[u8]) -> Result<(u32, usize)> {
    let mut result = 0u32;
    let mut shift = 0u32;
    for (index, byte) in bytes.iter().copied().enumerate() {
        let chunk = (byte & 0x7f) as u32;
        result |= chunk
            .checked_shl(shift)
            .ok_or_else(|| CliError::InvalidInput("invalid wasm LEB128 shift".to_string()))?;
        if byte & 0x80 == 0 {
            return Ok((result, index + 1));
        }
        shift += 7;
        if shift >= 35 {
            break;
        }
    }
    Err(CliError::InvalidInput(
        "invalid or truncated wasm LEB128 integer".to_string(),
    ))
}

fn wasm_section_name(id: u8) -> &'static str {
    match id {
        0 => "custom",
        1 => "type",
        2 => "import",
        3 => "function",
        4 => "table",
        5 => "memory",
        6 => "global",
        7 => "export",
        8 => "start",
        9 => "element",
        10 => "code",
        11 => "data",
        12 => "data-count",
        _ => "unknown",
    }
}

fn render_native(
    path: &Path,
    bytes: &[u8],
    kind: FileKind,
    file: Option<&object::File<'_>>,
    args: &InspectArgs,
) -> Result<()> {
    println!("format: native-object");
    println!("path: {}", path.display());
    println!("kind: {}", format_file_kind(kind));

    match kind {
        FileKind::Elf32 | FileKind::Elf64 => render_elf_header(bytes)?,
        FileKind::MachO32 | FileKind::MachO64 | FileKind::MachOFat32 | FileKind::MachOFat64 => {
            render_macho_header(bytes, kind)?
        }
        FileKind::Pe32 | FileKind::Pe64 => render_pe_header(bytes)?,
        _ => {}
    }

    if let Some(file) = file {
        println!("architecture: {:?}", file.architecture());
        println!("endianness: {:?}", file.endianness());
        println!("is_little_endian: {}", file.is_little_endian());
        println!("entry: 0x{:x}", file.entry());
        render_object_summary(file);
        if file.architecture() == Architecture::Bpf {
            render_ebpf_object_summary(bytes, file)?;
        }
    } else {
        println!("object_parse: unavailable");
    }

    if args.hex {
        print_hex_dump(bytes, args.max_bytes);
    }

    Ok(())
}

fn format_file_kind(kind: FileKind) -> &'static str {
    match kind {
        FileKind::Archive => "archive",
        FileKind::Coff => "coff",
        FileKind::CoffBig => "coff-bigobj",
        FileKind::CoffImport => "coff-import",
        FileKind::DyldCache => "dyld-cache",
        FileKind::Elf32 => "elf32",
        FileKind::Elf64 => "elf64",
        FileKind::MachO32 => "macho32",
        FileKind::MachO64 => "macho64",
        FileKind::MachOFat32 => "macho-fat32",
        FileKind::MachOFat64 => "macho-fat64",
        FileKind::Pe32 => "pe32",
        FileKind::Pe64 => "pe64",
        FileKind::Xcoff32 => "xcoff32",
        FileKind::Xcoff64 => "xcoff64",
        _ => "unknown",
    }
}

fn render_raw(path: &Path, bytes: &[u8], args: &InspectArgs) {
    println!("format: raw");
    println!("path: {}", path.display());
    println!("size: {}", bytes.len());
    if args.hex {
        print_hex_dump(bytes, args.max_bytes);
    }
}

fn print_hex_dump(bytes: &[u8], max_bytes: usize) {
    let shown = bytes.len().min(max_bytes);
    println!("hex_bytes_shown: {}", shown);
    for (line_index, chunk) in bytes[..shown].chunks(16).enumerate() {
        let offset = line_index * 16;
        let mut hex = String::new();
        let mut ascii = String::new();
        for byte in chunk {
            let _ = write!(&mut hex, "{:02x} ", byte);
            ascii.push(if byte.is_ascii_graphic() || *byte == b' ' {
                *byte as char
            } else {
                '.'
            });
        }
        println!("  {:08x}  {:<48} {}", offset, hex.trim_end(), ascii);
    }
    if bytes.len() > shown {
        println!("hex_truncated: true");
    }
}
