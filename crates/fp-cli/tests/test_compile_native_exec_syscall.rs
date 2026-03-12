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
        target_triple: Some("x86_64-unknown-linux-gnu".to_string()),
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
        exec: true,
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

fn build_x86_64_elf_object_with_exit_syscall() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);

    // mov rax, 60; mov rdi, 0; syscall; ret
    let mut text = Vec::new();
    text.extend_from_slice(&[0x48, 0xB8]);
    text.extend_from_slice(&60u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBF]);
    text.extend_from_slice(&0u64.to_le_bytes());
    text.extend_from_slice(&[0x0F, 0x05, 0xC3]);
    obj.append_section_data(section_id, &text, 1);

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

#[tokio::test]
async fn compile_native_object_exec_preserves_syscall() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.out");

    fs::write(&input_file, build_x86_64_elf_object_with_exit_syscall()).unwrap();
    let args = base_args(input_file, output_file.clone());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    let file = object::File::parse(bytes.as_slice()).unwrap();
    assert_eq!(file.format(), BinaryFormat::Elf);
    assert_eq!(file.architecture(), Architecture::X86_64);
    assert!(matches!(
        file.kind(),
        object::ObjectKind::Executable | object::ObjectKind::Dynamic
    ));

    assert!(
        bytes.windows(2).any(|w| w == [0x0F, 0x05]),
        "missing syscall bytes in output"
    );
}
