use std::fs;

use object::Object as _;
use object::write::{Object, Symbol, SymbolSection};
use object::{
    Architecture, BinaryFormat, Endianness, SectionKind, SymbolFlags, SymbolKind, SymbolScope,
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
        package_graph: None,
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

fn build_x86_64_elf_object_with_linux_syscalls() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);

    // x86_64 Linux syscalls:
    //   write(1, "hi\n", 3)
    //   exit(0)
    // Uses stack storage to avoid relocations.
    //
    // sub rsp, 16
    // mov rax, 0x00000000000A6968 ; "hi\n" in little endian
    // mov [rsp], rax
    // mov rax, 1
    // mov rdi, 1
    // mov rsi, rsp
    // mov rdx, 3
    // syscall
    // mov rax, 60
    // xor rdi, rdi
    // syscall
    let mut text = Vec::new();
    text.extend_from_slice(&[0x48, 0x83, 0xEC, 0x10]);
    text.extend_from_slice(&[0x48, 0xB8]);
    text.extend_from_slice(&0x0000_0000_000A_6968u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0x89, 0x04, 0x24]);
    text.extend_from_slice(&[0x48, 0xB8]);
    text.extend_from_slice(&1u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBF]);
    text.extend_from_slice(&1u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0x89, 0xE6]);
    text.extend_from_slice(&[0x48, 0xBA]);
    text.extend_from_slice(&3u64.to_le_bytes());
    text.extend_from_slice(&[0x0F, 0x05]);
    text.extend_from_slice(&[0x48, 0xB8]);
    text.extend_from_slice(&60u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0x31, 0xFF]);
    text.extend_from_slice(&[0x0F, 0x05]);
    obj.append_section_data(section_id, &text, 16);

    obj.add_symbol(Symbol {
        name: b"main".to_vec(),
        value: 0,
        size: text.len() as u64,
        kind: SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(section_id),
        flags: SymbolFlags::None,
    });

    obj.write().expect("write ELF object")
}

#[cfg(target_os = "macos")]
#[tokio::test]
async fn compile_linux_elf_syscalls_to_darwin_macho_exec_and_run() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.arm64.out");

    fs::write(&input_file, build_x86_64_elf_object_with_linux_syscalls()).unwrap();
    let args = base_args(input_file, output_file.clone());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    let file = object::File::parse(bytes.as_slice()).unwrap();
    assert_eq!(file.kind(), object::ObjectKind::Executable);
    assert_eq!(file.format(), BinaryFormat::MachO);
    assert_eq!(file.architecture(), Architecture::Aarch64);

    let output = tokio::process::Command::new(&output_file)
        .output()
        .await
        .expect("execute output");
    assert!(output.status.success(), "status={:?}", output.status);
    assert_eq!(output.stdout, b"hi\n");
}

