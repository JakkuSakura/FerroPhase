use std::fs;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, EmitterKind, compile_command};
use fp_cli::pipeline::BackendKind;
use object::read::archive::ArchiveFile;
use object::write::Object;
use object::{Architecture, BinaryFormat, Endianness, Object as _, ObjectSection as _};
use tempfile::TempDir;

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
        link: false,
        save_intermediates: false,
        lossy: false,
        max_errors: 10,
        source_language: Some("archive".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    }
}

fn build_minimal_x86_64_elf_object(symbol: &str) -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), object::SectionKind::Text);
    // add rax, 1; ret
    obj.append_section_data(section_id, &[0x48, 0x05, 0x01, 0x00, 0x00, 0x00, 0xC3], 1);
    obj.add_symbol(object::write::Symbol {
        name: symbol.as_bytes().to_vec(),
        value: 0,
        size: 7,
        kind: object::SymbolKind::Text,
        scope: object::SymbolScope::Linkage,
        weak: false,
        section: object::write::SymbolSection::Section(section_id),
        flags: object::SymbolFlags::None,
    });
    obj.write().expect("write x86_64 ELF object")
}

#[tokio::test]
async fn compile_native_archive_transpiles_x86_64_elf_to_aarch64_macho_members() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("libdemo.a");
    let output_file = temp_dir.path().join("libdemo.out.a");

    let archive_bytes = fp_native::archive::write_gnu_archive(&[
        fp_native::archive::ArchiveMember {
            name: "m1.o".to_string(),
            data: build_minimal_x86_64_elf_object("m1"),
        },
        fp_native::archive::ArchiveMember {
            name: "m2.o".to_string(),
            data: build_minimal_x86_64_elf_object("m2"),
        },
    ])
    .unwrap();
    fs::write(&input_file, archive_bytes).unwrap();

    let args = base_args(
        input_file.clone(),
        output_file.clone(),
        "aarch64-apple-darwin",
    );
    compile_command(args, &CliConfig::default()).await.unwrap();

    let out_bytes = fs::read(&output_file).unwrap();
    let archive = ArchiveFile::parse(out_bytes.as_slice()).unwrap();

    let mut object_members = 0;
    for member in archive.members() {
        let member = member.unwrap();
        let data = member.data(out_bytes.as_slice()).unwrap();
        if data.is_empty() {
            continue;
        }
        let Ok(file) = object::File::parse(data) else {
            continue;
        };
        object_members += 1;
        assert_eq!(file.format(), BinaryFormat::MachO);
        assert_eq!(file.architecture(), Architecture::Aarch64);
        let text = file.section_by_name(".text").unwrap();
        assert!(!text.data().unwrap_or_default().is_empty());
    }

    assert_eq!(object_members, 2);
}
