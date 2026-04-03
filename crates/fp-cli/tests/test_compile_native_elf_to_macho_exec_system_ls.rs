use std::fs;

use object::Object as _;
use object::write::{Object, Relocation, StandardSection, Symbol, SymbolSection};
use object::{
    Architecture, BinaryFormat, Endianness, RelocationEncoding, RelocationFlags, RelocationKind,
    SectionKind, SymbolFlags, SymbolKind, SymbolScope,
};
use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, EmitterKind, compile_command};
use fp_cli::pipeline::BackendKind;

fn base_args(input: std::path::PathBuf, output: std::path::PathBuf) -> CompileArgs {
    CompileArgs {
        input: vec![input],
        backend: BackendKind::Binary,
        target: None,
        emitter: EmitterKind::Native,
        target_triple: Some("aarch64-apple-darwin".to_string()),
        target_cpu: None,
        native_target: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: Some(output),
        graph: None,
        opt_level: 0,
        debug: false,
        release: false,
        include: Vec::new(),
        define: Vec::new(),
        exec: false,
        link: true,
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

fn build_x86_64_elf_object_calls_system_ls() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);

    let text_id = obj.section_id(StandardSection::Text);
    let rodata_id = obj.add_section(Vec::new(), b".rodata".to_vec(), SectionKind::ReadOnlyData);

    // rodata: "ls\0"
    obj.append_section_data(rodata_id, b"ls\0", 1);

    // .text:
    //   lea rdi, [rip + cmd]
    //   call system
    //   xor eax, eax
    //   ret
    let mut text = Vec::new();
    text.extend_from_slice(&[0x48, 0x8D, 0x3D, 0, 0, 0, 0]); // lea rdi, [rip + disp32]
    text.extend_from_slice(&[0xE8, 0, 0, 0, 0]); // call rel32
    text.extend_from_slice(&[0x31, 0xC0]); // xor eax, eax
    text.push(0xC3); // ret
    obj.append_section_data(text_id, &text, 16);

    let cmd_id = obj.add_symbol(Symbol {
        name: b"cmd".to_vec(),
        value: 0,
        size: 3,
        kind: SymbolKind::Data,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(rodata_id),
        flags: SymbolFlags::None,
    });
    let system_id = obj.add_symbol(Symbol {
        name: b"system".to_vec(),
        value: 0,
        size: 0,
        kind: SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Undefined,
        flags: SymbolFlags::None,
    });

    // Relocate `lea` disp32 against `cmd`.
    obj.add_relocation(
        text_id,
        Relocation {
            offset: 3,
            symbol: cmd_id,
            addend: 0,
            flags: RelocationFlags::Generic {
                kind: RelocationKind::Relative,
                encoding: RelocationEncoding::X86RipRelative,
                size: 32,
            },
        },
    )
    .unwrap();

    // Relocate `call` rel32 against `system`.
    obj.add_relocation(
        text_id,
        Relocation {
            offset: 8,
            symbol: system_id,
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
        size: text.len() as u64,
        kind: SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(text_id),
        flags: SymbolFlags::None,
    });

    obj.write().expect("write ELF object")
}

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
#[tokio::test]
async fn compile_elf_object_calling_system_to_macho_exec_and_lists_files() {
    let temp_dir = TempDir::new().unwrap();
    let marker = temp_dir.path().join("fp_marker_file.txt");
    fs::write(&marker, b"marker").unwrap();

    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.arm64.out");

    fs::write(&input_file, build_x86_64_elf_object_calls_system_ls()).unwrap();
    let args = base_args(input_file, output_file.clone());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    let file = object::File::parse(bytes.as_slice()).unwrap();
    assert_eq!(file.kind(), object::ObjectKind::Executable);
    assert_eq!(file.format(), BinaryFormat::MachO);
    assert_eq!(file.architecture(), Architecture::Aarch64);

    let output = tokio::process::Command::new(&output_file)
        .current_dir(temp_dir.path())
        .output()
        .await
        .expect("execute output");
    assert!(output.status.success(), "status={:?}", output.status);
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("fp_marker_file.txt"),
        "missing marker; stdout={stdout:?} stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );
}
