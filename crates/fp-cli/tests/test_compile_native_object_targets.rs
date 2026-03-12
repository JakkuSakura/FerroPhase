use std::fs;

use object::Object as _;
use object::ObjectSection as _;
use object::ObjectSymbol as _;
use object::macho::{ARM64_RELOC_PAGE21, ARM64_RELOC_PAGEOFF12};
use object::write::{Object, Symbol, SymbolSection};
use object::{
    Architecture, BinaryFormat, Endianness, RelocationEncoding, RelocationFlags, RelocationKind,
    SectionKind, SymbolFlags, SymbolKind, SymbolScope,
};
use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, EmitterKind, compile_command};
use fp_cli::pipeline::BackendKind;

fn base_args(
    input: std::path::PathBuf,
    output: std::path::PathBuf,
    target_triple: &str,
) -> CompileArgs {
    CompileArgs {
        input: vec![input],
        backend: BackendKind::Binary,
        target: None,
        emitter: EmitterKind::Native,
        target_triple: Some(target_triple.to_string()),
        target_cpu: None,
        native_target: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: Some(output),
        package_graph: None,
        opt_level: 0,
        debug: false,
        release: false,
        include: Vec::new(),
        define: Vec::new(),
        exec: false,
        save_intermediates: false,
        lossy: false,
        max_errors: 10,
        source_language: Some("object".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    }
}

#[tokio::test]
async fn compile_detects_native_object_without_extension() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("input");
    let output_file = temp_dir.path().join("main.aarch64.o");

    fs::write(&input_file, build_x86_64_elf_object_with_rip_store_reloc()).unwrap();
    let args = base_args(input_file, output_file.clone(), "aarch64-apple-darwin");
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    let file = object::File::parse(bytes.as_slice()).unwrap();
    assert_eq!(file.format(), BinaryFormat::MachO);
    assert_eq!(file.architecture(), Architecture::Aarch64);
}

fn build_x86_64_elf_object_with_rip_store_reloc() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);
    // mov [rip + global], rax; ret
    obj.append_section_data(section_id, &[0x48, 0x89, 0x05, 0, 0, 0, 0, 0xC3], 1);

    let global_id = obj.add_symbol(Symbol {
        name: b"global".to_vec(),
        value: 0,
        size: 0,
        kind: SymbolKind::Data,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Undefined,
        flags: SymbolFlags::None,
    });

    obj.add_relocation(
        section_id,
        object::write::Relocation {
            offset: 3,
            symbol: global_id,
            addend: 0,
            flags: RelocationFlags::Generic {
                kind: RelocationKind::Relative,
                encoding: RelocationEncoding::X86RipRelative,
                size: 32,
            },
        },
    )
    .unwrap();

    obj.add_symbol(Symbol {
        name: b"main".to_vec(),
        value: 0,
        size: 8,
        kind: SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(section_id),
        flags: SymbolFlags::None,
    });

    obj.write().expect("write ELF object")
}

#[tokio::test]
async fn compile_native_object_preserves_rip_relative_store_reloc_x86_64_to_aarch64() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.aarch64.o");

    fs::write(&input_file, build_x86_64_elf_object_with_rip_store_reloc()).unwrap();
    let args = base_args(input_file, output_file.clone(), "aarch64-apple-darwin");
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    let file = object::File::parse(bytes.as_slice()).unwrap();
    assert_eq!(file.format(), BinaryFormat::MachO);
    assert_eq!(file.architecture(), Architecture::Aarch64);
    assert!(
        find_any_relocation_target(&file, "global") || find_any_relocation_target(&file, "_global"),
        "missing relocation to global; saw: {:?}",
        collect_any_relocation_targets(&file)
    );
}

fn build_x86_64_elf_object_with_rip_load_reloc() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);
    // mov rax, [rip + global]; ret
    obj.append_section_data(section_id, &[0x48, 0x8B, 0x05, 0, 0, 0, 0, 0xC3], 1);

    let global_id = obj.add_symbol(Symbol {
        name: b"global".to_vec(),
        value: 0,
        size: 0,
        kind: SymbolKind::Data,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Undefined,
        flags: SymbolFlags::None,
    });

    obj.add_relocation(
        section_id,
        object::write::Relocation {
            offset: 3,
            symbol: global_id,
            addend: 0,
            flags: RelocationFlags::Generic {
                kind: RelocationKind::Relative,
                encoding: RelocationEncoding::X86RipRelative,
                size: 32,
            },
        },
    )
    .unwrap();

    obj.add_symbol(Symbol {
        name: b"main".to_vec(),
        value: 0,
        size: 8,
        kind: SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(section_id),
        flags: SymbolFlags::None,
    });

    obj.write().expect("write ELF object")
}

#[tokio::test]
async fn compile_native_object_preserves_rip_relative_load_reloc_x86_64_to_aarch64() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.aarch64.o");

    fs::write(&input_file, build_x86_64_elf_object_with_rip_load_reloc()).unwrap();
    let args = base_args(input_file, output_file.clone(), "aarch64-apple-darwin");
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    let file = object::File::parse(bytes.as_slice()).unwrap();
    assert_eq!(file.format(), BinaryFormat::MachO);
    assert_eq!(file.architecture(), Architecture::Aarch64);
    assert!(
        find_any_relocation_target(&file, "global") || find_any_relocation_target(&file, "_global"),
        "missing relocation to global; saw: {:?}",
        collect_any_relocation_targets(&file)
    );
}

fn build_x86_64_elf_object_with_data_reloc() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);
    // mov rax, global; ret
    // 48 B8 imm64; C3
    obj.append_section_data(section_id, &[0x48, 0xB8, 0, 0, 0, 0, 0, 0, 0, 0, 0xC3], 1);

    let global_id = obj.add_symbol(Symbol {
        name: b"global".to_vec(),
        value: 0,
        size: 0,
        kind: SymbolKind::Data,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Undefined,
        flags: SymbolFlags::None,
    });

    obj.add_relocation(
        section_id,
        object::write::Relocation {
            offset: 2,
            symbol: global_id,
            addend: 0,
            flags: RelocationFlags::Generic {
                kind: RelocationKind::Absolute,
                encoding: RelocationEncoding::Generic,
                size: 64,
            },
        },
    )
    .unwrap();

    obj.add_symbol(Symbol {
        name: b"main".to_vec(),
        value: 0,
        size: 11,
        kind: SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(section_id),
        flags: SymbolFlags::None,
    });

    obj.write().expect("write ELF object")
}

#[tokio::test]
async fn compile_native_object_preserves_data_relocations_x86_64_to_aarch64() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.aarch64.o");

    fs::write(&input_file, build_x86_64_elf_object_with_data_reloc()).unwrap();
    let args = base_args(input_file, output_file.clone(), "aarch64-apple-darwin");
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    let file = object::File::parse(bytes.as_slice()).unwrap();
    assert_eq!(file.format(), BinaryFormat::MachO);
    assert_eq!(file.architecture(), Architecture::Aarch64);
    assert!(
        find_any_relocation_target(&file, "global") || find_any_relocation_target(&file, "_global"),
        "missing relocation to global; saw: {:?}",
        collect_any_relocation_targets(&file)
    );
}

fn build_aarch64_macho_object_with_adrp_reloc() -> Vec<u8> {
    let mut obj = Object::new(
        BinaryFormat::MachO,
        Architecture::Aarch64,
        Endianness::Little,
    );
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);
    // adrp x0, global; add x0, x0, #0; ret
    obj.append_section_data(
        section_id,
        &[
            0x00, 0x00, 0x00, 0x90, // adrp x0, #0
            0x00, 0x00, 0x00, 0x91, // add x0, x0, #0
            0xC0, 0x03, 0x5F, 0xD6, // ret
        ],
        4,
    );

    let global_id = obj.add_symbol(Symbol {
        name: b"global".to_vec(),
        value: 0,
        size: 0,
        kind: SymbolKind::Data,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Undefined,
        flags: SymbolFlags::None,
    });

    obj.add_relocation(
        section_id,
        object::write::Relocation {
            offset: 0,
            symbol: global_id,
            addend: 0,
            flags: RelocationFlags::MachO {
                r_type: ARM64_RELOC_PAGE21,
                r_pcrel: true,
                r_length: 2,
            },
        },
    )
    .unwrap();

    obj.add_relocation(
        section_id,
        object::write::Relocation {
            offset: 4,
            symbol: global_id,
            addend: 0,
            flags: RelocationFlags::MachO {
                r_type: ARM64_RELOC_PAGEOFF12,
                r_pcrel: false,
                r_length: 2,
            },
        },
    )
    .unwrap();

    obj.add_symbol(Symbol {
        name: b"main".to_vec(),
        value: 0,
        size: 12,
        kind: SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(section_id),
        flags: SymbolFlags::None,
    });

    obj.write().expect("write Mach-O object")
}

#[tokio::test]
async fn compile_native_object_preserves_data_relocations_aarch64_to_x86_64() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.x86_64.o");

    fs::write(&input_file, build_aarch64_macho_object_with_adrp_reloc()).unwrap();
    let args = base_args(input_file, output_file.clone(), "x86_64-unknown-linux-gnu");
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    let file = object::File::parse(bytes.as_slice()).unwrap();
    assert_eq!(file.format(), BinaryFormat::Elf);
    assert_eq!(file.architecture(), Architecture::X86_64);
    assert!(
        find_any_relocation_target(&file, "global") || find_any_relocation_target(&file, "_global"),
        "missing relocation to global; saw: {:?}",
        collect_any_relocation_targets(&file)
    );
}

fn find_any_relocation_target(file: &object::File<'_>, target: &str) -> bool {
    for section in file.sections() {
        for (_offset, reloc) in section.relocations() {
            let object::RelocationTarget::Symbol(symbol_index) = reloc.target() else {
                continue;
            };
            let Ok(symbol) = file.symbol_by_index(symbol_index) else {
                continue;
            };
            let Ok(name) = symbol.name() else {
                continue;
            };
            if name == target {
                return true;
            }
        }
    }
    false
}

fn collect_any_relocation_targets(file: &object::File<'_>) -> Vec<String> {
    let mut targets = Vec::new();
    for section in file.sections() {
        for (_offset, reloc) in section.relocations() {
            let object::RelocationTarget::Symbol(symbol_index) = reloc.target() else {
                continue;
            };
            let Ok(symbol) = file.symbol_by_index(symbol_index) else {
                continue;
            };
            let Ok(name) = symbol.name() else {
                continue;
            };
            targets.push(name.to_string());
        }
    }
    targets
}

#[tokio::test]
async fn compile_native_object_preserves_call_relocations_aarch64_to_x86_64() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.x86_64.o");

    fs::write(&input_file, build_aarch64_macho_object_with_call_reloc()).unwrap();
    let args = base_args(input_file, output_file.clone(), "x86_64-unknown-linux-gnu");
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    let file = object::File::parse(bytes.as_slice()).unwrap();
    assert_eq!(file.format(), BinaryFormat::Elf);
    assert_eq!(file.architecture(), Architecture::X86_64);
    assert!(
        find_text_relocation_target(&file, "callee"),
        "missing relocation to callee; saw: {:?}",
        collect_text_relocation_targets(&file)
    );
}

fn build_aarch64_macho_object_with_call_reloc() -> Vec<u8> {
    let mut obj = Object::new(
        BinaryFormat::MachO,
        Architecture::Aarch64,
        Endianness::Little,
    );
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);
    // bl callee; ret
    obj.append_section_data(
        section_id,
        &[0x00, 0x00, 0x00, 0x94, 0xC0, 0x03, 0x5F, 0xD6],
        4,
    );

    let callee_id = obj.add_symbol(Symbol {
        name: b"callee".to_vec(),
        value: 0,
        size: 0,
        kind: SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Undefined,
        flags: SymbolFlags::None,
    });

    obj.add_relocation(
        section_id,
        object::write::Relocation {
            offset: 0,
            symbol: callee_id,
            addend: 0,
            flags: RelocationFlags::Generic {
                kind: RelocationKind::Relative,
                encoding: RelocationEncoding::AArch64Call,
                size: 32,
            },
        },
    )
    .unwrap();

    obj.add_symbol(Symbol {
        name: b"main".to_vec(),
        value: 0,
        size: 8,
        kind: SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(section_id),
        flags: SymbolFlags::None,
    });

    obj.write().expect("write Mach-O object")
}

fn find_text_relocation_target(file: &object::File<'_>, target: &str) -> bool {
    let mangled = format!("_{target}");
    collect_text_relocation_targets(file)
        .iter()
        .any(|name| name == target || name == &mangled)
}

fn collect_text_relocation_targets(file: &object::File<'_>) -> Vec<String> {
    let Some(text) = file
        .sections()
        .find(|section| section.kind() == SectionKind::Text)
    else {
        return Vec::new();
    };
    let mut targets = Vec::new();
    for (_offset, reloc) in text.relocations() {
        let object::RelocationTarget::Symbol(symbol_index) = reloc.target() else {
            continue;
        };
        let Ok(symbol) = file.symbol_by_index(symbol_index) else {
            continue;
        };
        let Ok(name) = symbol.name() else {
            continue;
        };
        targets.push(name.to_string());
    }
    targets
}

fn build_x86_64_elf_object_with_call_reloc() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);
    // call callee; ret
    obj.append_section_data(section_id, &[0xE8, 0, 0, 0, 0, 0xC3], 1);

    let callee_id = obj.add_symbol(Symbol {
        name: b"callee".to_vec(),
        value: 0,
        size: 0,
        kind: SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Undefined,
        flags: SymbolFlags::None,
    });

    obj.add_relocation(
        section_id,
        object::write::Relocation {
            offset: 1,
            symbol: callee_id,
            addend: 0,
            flags: RelocationFlags::Generic {
                kind: RelocationKind::Relative,
                encoding: RelocationEncoding::X86Branch,
                size: 32,
            },
        },
    )
    .unwrap();

    obj.add_symbol(Symbol {
        name: b"main".to_vec(),
        value: 0,
        size: 6,
        kind: SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(section_id),
        flags: SymbolFlags::None,
    });

    obj.write().expect("write ELF object")
}

#[tokio::test]
async fn compile_native_object_preserves_call_relocations_x86_64_to_aarch64() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.aarch64.o");

    fs::write(&input_file, build_x86_64_elf_object_with_call_reloc()).unwrap();
    let args = base_args(input_file, output_file.clone(), "aarch64-apple-darwin");
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    let file = object::File::parse(bytes.as_slice()).unwrap();
    assert_eq!(file.format(), BinaryFormat::MachO);
    assert_eq!(file.architecture(), Architecture::Aarch64);
    assert!(
        find_text_relocation_target(&file, "callee"),
        "missing relocation to callee; saw: {:?}",
        collect_text_relocation_targets(&file)
    );
}

#[tokio::test]
async fn compile_native_object_transpiles_x86_64_elf_to_aarch64_macho() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.aarch64.o");

    fs::write(&input_file, build_minimal_x86_64_elf_object()).unwrap();
    let args = base_args(input_file, output_file.clone(), "aarch64-apple-darwin");
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    let file = object::File::parse(bytes.as_slice()).unwrap();
    assert_eq!(file.format(), BinaryFormat::MachO);
    assert_eq!(file.architecture(), Architecture::Aarch64);
    assert!(
        file.sections()
            .any(|section| section.kind() == SectionKind::Text)
    );
}

fn build_minimal_x86_64_elf_object() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);
    obj.append_section_data(section_id, &[0xC3], 1);
    obj.add_symbol(Symbol {
        name: b"main".to_vec(),
        value: 0,
        size: 1,
        kind: SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(section_id),
        flags: SymbolFlags::None,
    });
    obj.write().expect("write ELF object")
}

fn build_minimal_aarch64_macho_object() -> Vec<u8> {
    let mut obj = Object::new(
        BinaryFormat::MachO,
        Architecture::Aarch64,
        Endianness::Little,
    );
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);
    obj.append_section_data(section_id, &[0xC0, 0x03, 0x5F, 0xD6], 4);
    obj.add_symbol(Symbol {
        name: b"main".to_vec(),
        value: 0,
        size: 4,
        kind: SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(section_id),
        flags: SymbolFlags::None,
    });
    obj.write().expect("write Mach-O object")
}

#[tokio::test]
async fn compile_native_object_transpiles_aarch64_macho_to_x86_64_elf() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.x86_64.o");

    fs::write(&input_file, build_minimal_aarch64_macho_object()).unwrap();
    let args = base_args(input_file, output_file.clone(), "x86_64-unknown-linux-gnu");
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    let file = object::File::parse(bytes.as_slice()).unwrap();
    assert_eq!(file.format(), BinaryFormat::Elf);
    assert_eq!(file.architecture(), Architecture::X86_64);
    assert!(
        file.sections()
            .any(|section| section.kind() == SectionKind::Text)
    );
}
