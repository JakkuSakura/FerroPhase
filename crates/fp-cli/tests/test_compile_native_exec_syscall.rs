use std::fs;

use object::Object as _;
use object::write::{Object, Symbol, SymbolSection};
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
        link: false,
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

fn build_x86_64_elf_object_with_access_syscall() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);

    // mov rax, 21; mov rdi, 0; mov rsi, 0; syscall; ret
    let mut text = Vec::new();
    text.extend_from_slice(&[0x48, 0xB8]);
    text.extend_from_slice(&21u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBF]);
    text.extend_from_slice(&0u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBE]);
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
async fn compile_linux_access_syscall_exec_to_windows_import_call() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.exe");

    fs::write(&input_file, build_x86_64_elf_object_with_access_syscall()).unwrap();
    let mut args = base_args(input_file, output_file.clone());
    args.target_triple = Some("x86_64-pc-windows-msvc".to_string());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    assert!(
        bytes.windows(2).all(|w| w != [0x0F, 0x05]),
        "unexpected syscall bytes in Windows output"
    );
    let haystack = String::from_utf8_lossy(&bytes).to_ascii_lowercase();
    assert!(
        haystack.contains("kernel32.dll"),
        "missing kernel32.dll import"
    );
    assert!(
        haystack.contains("getfileattributesa"),
        "missing GetFileAttributesA import"
    );
}

fn build_x86_64_elf_object_with_unlink_syscall() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);

    // mov rax, 87; mov rdi, 0; syscall; ret
    let mut text = Vec::new();
    text.extend_from_slice(&[0x48, 0xB8]);
    text.extend_from_slice(&87u64.to_le_bytes());
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
async fn compile_linux_unlink_syscall_exec_to_windows_import_call() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.exe");

    fs::write(&input_file, build_x86_64_elf_object_with_unlink_syscall()).unwrap();
    let mut args = base_args(input_file, output_file.clone());
    args.target_triple = Some("x86_64-pc-windows-msvc".to_string());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    assert!(
        bytes.windows(2).all(|w| w != [0x0F, 0x05]),
        "unexpected syscall bytes in Windows output"
    );
    let haystack = String::from_utf8_lossy(&bytes).to_ascii_lowercase();
    assert!(
        haystack.contains("kernel32.dll"),
        "missing kernel32.dll import"
    );
    assert!(
        haystack.contains("deletefilea"),
        "missing DeleteFileA import"
    );
}

fn build_x86_64_elf_object_with_mkdir_syscall() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);

    // mov rax, 83; mov rdi, 0; mov rsi, 0; syscall; ret
    let mut text = Vec::new();
    text.extend_from_slice(&[0x48, 0xB8]);
    text.extend_from_slice(&83u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBF]);
    text.extend_from_slice(&0u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBE]);
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
async fn compile_linux_mkdir_syscall_exec_to_windows_import_call() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.exe");

    fs::write(&input_file, build_x86_64_elf_object_with_mkdir_syscall()).unwrap();
    let mut args = base_args(input_file, output_file.clone());
    args.target_triple = Some("x86_64-pc-windows-msvc".to_string());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    assert!(
        bytes.windows(2).all(|w| w != [0x0F, 0x05]),
        "unexpected syscall bytes in Windows output"
    );
    let haystack = String::from_utf8_lossy(&bytes).to_ascii_lowercase();
    assert!(
        haystack.contains("kernel32.dll"),
        "missing kernel32.dll import"
    );
    assert!(
        haystack.contains("createdirectorya"),
        "missing CreateDirectoryA import"
    );
}

fn build_x86_64_elf_object_with_rename_syscall() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);

    // mov rax, 82; mov rdi, 0; mov rsi, 0; syscall; ret
    let mut text = Vec::new();
    text.extend_from_slice(&[0x48, 0xB8]);
    text.extend_from_slice(&82u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBF]);
    text.extend_from_slice(&0u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBE]);
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
async fn compile_linux_rename_syscall_exec_to_windows_import_call() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.exe");

    fs::write(&input_file, build_x86_64_elf_object_with_rename_syscall()).unwrap();
    let mut args = base_args(input_file, output_file.clone());
    args.target_triple = Some("x86_64-pc-windows-msvc".to_string());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    assert!(
        bytes.windows(2).all(|w| w != [0x0F, 0x05]),
        "unexpected syscall bytes in Windows output"
    );
    let haystack = String::from_utf8_lossy(&bytes).to_ascii_lowercase();
    assert!(
        haystack.contains("kernel32.dll"),
        "missing kernel32.dll import"
    );
    assert!(
        haystack.contains("movefileexa"),
        "missing MoveFileExA import"
    );
}

fn build_x86_64_elf_object_with_dlopen_call() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);

    // call dlopen; ret
    let text: [u8; 6] = [0xE8, 0x00, 0x00, 0x00, 0x00, 0xC3];
    obj.append_section_data(section_id, &text, 1);

    let main_symbol = obj.add_symbol(Symbol {
        name: b"main".to_vec(),
        value: 0,
        size: text.len() as u64,
        kind: SymbolKind::Text,
        scope: SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(section_id),
        flags: SymbolFlags::None,
    });

    let dlopen_symbol = obj.add_symbol(Symbol {
        name: b"dlopen".to_vec(),
        value: 0,
        size: 0,
        kind: SymbolKind::Text,
        scope: SymbolScope::Unknown,
        weak: false,
        section: SymbolSection::Undefined,
        flags: SymbolFlags::None,
    });

    obj.add_relocation(
        section_id,
        object::write::Relocation {
            offset: 1,
            symbol: dlopen_symbol,
            addend: 0,
            flags: RelocationFlags::Generic {
                kind: RelocationKind::Relative,
                encoding: RelocationEncoding::X86Branch,
                size: 32,
            },
        },
    )
    .expect("add relocation");

    let _ = main_symbol;
    obj.write().expect("write ELF object")
}

#[tokio::test]
async fn compile_linux_dlopen_call_exec_to_windows_import_call() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.exe");

    fs::write(&input_file, build_x86_64_elf_object_with_dlopen_call()).unwrap();
    let mut args = base_args(input_file, output_file.clone());
    args.target_triple = Some("x86_64-pc-windows-msvc".to_string());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    let haystack = String::from_utf8_lossy(&bytes).to_ascii_lowercase();
    assert!(
        haystack.contains("kernel32.dll"),
        "missing kernel32.dll import"
    );
    assert!(
        haystack.contains("loadlibrarya"),
        "missing LoadLibraryA import"
    );
}

fn build_x86_64_elf_object_with_getpid_syscall() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);

    // mov rax, 39; syscall; ret
    let mut text = Vec::new();
    text.extend_from_slice(&[0x48, 0xB8]);
    text.extend_from_slice(&39u64.to_le_bytes());
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
async fn compile_linux_getpid_syscall_exec_to_windows_import_call() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.exe");

    fs::write(&input_file, build_x86_64_elf_object_with_getpid_syscall()).unwrap();
    let mut args = base_args(input_file, output_file.clone());
    args.target_triple = Some("x86_64-pc-windows-msvc".to_string());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    assert!(
        bytes.windows(2).all(|w| w != [0x0F, 0x05]),
        "unexpected syscall bytes in Windows output"
    );
    let haystack = String::from_utf8_lossy(&bytes).to_ascii_lowercase();
    assert!(
        haystack.contains("kernel32.dll"),
        "missing kernel32.dll import"
    );
    assert!(
        haystack.contains("getcurrentprocessid"),
        "missing GetCurrentProcessId import"
    );
}

fn build_x86_64_elf_object_with_seek_syscall() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);

    // mov rax, 8; mov rdi, 1; mov rsi, 0; mov rdx, 0; syscall; ret
    let mut text = Vec::new();
    text.extend_from_slice(&[0x48, 0xB8]);
    text.extend_from_slice(&8u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBF]);
    text.extend_from_slice(&1u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBE]);
    text.extend_from_slice(&0u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBA]);
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
async fn compile_linux_seek_syscall_exec_to_windows_import_call() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.exe");

    fs::write(&input_file, build_x86_64_elf_object_with_seek_syscall()).unwrap();
    let mut args = base_args(input_file, output_file.clone());
    args.target_triple = Some("x86_64-pc-windows-msvc".to_string());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    assert!(
        bytes.windows(2).all(|w| w != [0x0F, 0x05]),
        "unexpected syscall bytes in Windows output"
    );
    let haystack = String::from_utf8_lossy(&bytes).to_ascii_lowercase();
    assert!(
        haystack.contains("kernel32.dll"),
        "missing kernel32.dll import"
    );
    assert!(
        haystack.contains("setfilepointerex"),
        "missing SetFilePointerEx import"
    );
}

fn build_x86_64_elf_object_with_mmap_syscall() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);

    // mov rax, 9; mov rdi, 0; mov rsi, 4096; mov rdx, 3; mov r10, 34; mov r8, -1; mov r9, 0; syscall; ret
    let mut text = Vec::new();
    text.extend_from_slice(&[0x48, 0xB8]);
    text.extend_from_slice(&9u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBF]);
    text.extend_from_slice(&0u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBE]);
    text.extend_from_slice(&4096u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBA]);
    text.extend_from_slice(&3u64.to_le_bytes());
    text.extend_from_slice(&[0x49, 0xBA]);
    text.extend_from_slice(&34u64.to_le_bytes());
    text.extend_from_slice(&[0x49, 0xB8]);
    text.extend_from_slice(&u64::MAX.to_le_bytes());
    text.extend_from_slice(&[0x49, 0xB9]);
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
async fn compile_linux_mmap_syscall_exec_to_windows_import_call() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.exe");

    fs::write(&input_file, build_x86_64_elf_object_with_mmap_syscall()).unwrap();
    let mut args = base_args(input_file, output_file.clone());
    args.target_triple = Some("x86_64-pc-windows-msvc".to_string());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    assert!(
        bytes.windows(2).all(|w| w != [0x0F, 0x05]),
        "unexpected syscall bytes in Windows output"
    );
    let haystack = String::from_utf8_lossy(&bytes).to_ascii_lowercase();
    assert!(
        haystack.contains("kernel32.dll"),
        "missing kernel32.dll import"
    );
    assert!(
        haystack.contains("virtualalloc"),
        "missing VirtualAlloc import"
    );
}

fn build_x86_64_elf_object_with_open_syscall() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);

    // mov rax, 2; mov rdi, 0; mov rsi, 0; mov rdx, 0; syscall; ret
    let mut text = Vec::new();
    text.extend_from_slice(&[0x48, 0xB8]);
    text.extend_from_slice(&2u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBF]);
    text.extend_from_slice(&0u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBE]);
    text.extend_from_slice(&0u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBA]);
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
async fn compile_linux_open_syscall_exec_to_windows_import_call() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.exe");

    fs::write(&input_file, build_x86_64_elf_object_with_open_syscall()).unwrap();
    let mut args = base_args(input_file, output_file.clone());
    args.target_triple = Some("x86_64-pc-windows-msvc".to_string());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    assert!(
        bytes.windows(2).all(|w| w != [0x0F, 0x05]),
        "unexpected syscall bytes in Windows output"
    );
    let haystack = String::from_utf8_lossy(&bytes).to_ascii_lowercase();
    assert!(
        haystack.contains("kernel32.dll"),
        "missing kernel32.dll import"
    );
    assert!(
        haystack.contains("createfilea"),
        "missing CreateFileA import"
    );
}

fn build_x86_64_elf_object_with_close_syscall() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);

    // mov rax, 3; mov rdi, 1; syscall; ret
    let mut text = Vec::new();
    text.extend_from_slice(&[0x48, 0xB8]);
    text.extend_from_slice(&3u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBF]);
    text.extend_from_slice(&1u64.to_le_bytes());
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
async fn compile_linux_close_syscall_exec_to_windows_import_call() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.exe");

    fs::write(&input_file, build_x86_64_elf_object_with_close_syscall()).unwrap();
    let mut args = base_args(input_file, output_file.clone());
    args.target_triple = Some("x86_64-pc-windows-msvc".to_string());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    assert!(
        bytes.windows(2).all(|w| w != [0x0F, 0x05]),
        "unexpected syscall bytes in Windows output"
    );
    let haystack = String::from_utf8_lossy(&bytes).to_ascii_lowercase();
    assert!(
        haystack.contains("kernel32.dll"),
        "missing kernel32.dll import"
    );
    assert!(
        haystack.contains("closehandle"),
        "missing CloseHandle import"
    );
    assert!(
        haystack.contains("getstdhandle"),
        "missing GetStdHandle import"
    );
}

fn build_x86_64_elf_object_with_write_syscall() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);

    // mov rax, 1; mov rdi, 1; mov rsi, 0; mov rdx, 0; syscall; ret
    let mut text = Vec::new();
    text.extend_from_slice(&[0x48, 0xB8]);
    text.extend_from_slice(&1u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBF]);
    text.extend_from_slice(&1u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBE]);
    text.extend_from_slice(&0u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBA]);
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

fn build_x86_64_elf_object_with_read_syscall() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);

    // mov rax, 0; mov rdi, 0; mov rsi, 0; mov rdx, 0; syscall; ret
    let mut text = Vec::new();
    text.extend_from_slice(&[0x48, 0xB8]);
    text.extend_from_slice(&0u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBF]);
    text.extend_from_slice(&0u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBE]);
    text.extend_from_slice(&0u64.to_le_bytes());
    text.extend_from_slice(&[0x48, 0xBA]);
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
async fn compile_linux_write_syscall_exec_to_windows_import_call() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.exe");

    fs::write(&input_file, build_x86_64_elf_object_with_write_syscall()).unwrap();
    let mut args = base_args(input_file, output_file.clone());
    args.target_triple = Some("x86_64-pc-windows-msvc".to_string());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    assert!(
        bytes.windows(2).all(|w| w != [0x0F, 0x05]),
        "unexpected syscall bytes in Windows output"
    );
    let haystack = String::from_utf8_lossy(&bytes).to_ascii_lowercase();
    assert!(
        haystack.contains("kernel32.dll"),
        "missing kernel32.dll import"
    );
    assert!(haystack.contains("writefile"), "missing WriteFile import");
    assert!(
        haystack.contains("getstdhandle"),
        "missing GetStdHandle import"
    );
}

#[tokio::test]
async fn compile_linux_read_syscall_exec_to_windows_import_call() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.exe");

    fs::write(&input_file, build_x86_64_elf_object_with_read_syscall()).unwrap();
    let mut args = base_args(input_file, output_file.clone());
    args.target_triple = Some("x86_64-pc-windows-msvc".to_string());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    assert!(
        bytes.windows(2).all(|w| w != [0x0F, 0x05]),
        "unexpected syscall bytes in Windows output"
    );
    let haystack = String::from_utf8_lossy(&bytes).to_ascii_lowercase();
    assert!(
        haystack.contains("kernel32.dll"),
        "missing kernel32.dll import"
    );
    assert!(haystack.contains("readfile"), "missing ReadFile import");
    assert!(
        haystack.contains("getstdhandle"),
        "missing GetStdHandle import"
    );
}

#[tokio::test]
async fn compile_linux_syscall_exec_to_windows_import_call() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.o");
    let output_file = temp_dir.path().join("main.exe");

    fs::write(&input_file, build_x86_64_elf_object_with_exit_syscall()).unwrap();
    let mut args = base_args(input_file, output_file.clone());
    args.target_triple = Some("x86_64-pc-windows-msvc".to_string());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    assert!(
        bytes.windows(2).all(|w| w != [0x0F, 0x05]),
        "unexpected syscall bytes in Windows output"
    );
    let haystack = String::from_utf8_lossy(&bytes).to_ascii_lowercase();
    assert!(
        haystack.contains("kernel32.dll"),
        "missing kernel32.dll import"
    );
    assert!(
        haystack.contains("exitprocess"),
        "missing ExitProcess import"
    );
}

#[tokio::test]
async fn compile_windows_import_exec_to_linux_syscall() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.obj");
    let output_file = temp_dir.path().join("main.out");

    fs::write(
        &input_file,
        build_x86_64_coff_object_with_exitprocess_import(),
    )
    .unwrap();
    let args = base_args(input_file, output_file.clone());
    compile_command(args, &CliConfig::default()).await.unwrap();

    let bytes = fs::read(&output_file).unwrap();
    assert!(
        bytes.windows(2).any(|w| w == [0x0F, 0x05]),
        "missing syscall bytes in Linux output"
    );
}

fn build_x86_64_coff_object_with_exitprocess_import() -> Vec<u8> {
    let mut obj = Object::new(BinaryFormat::Coff, Architecture::X86_64, Endianness::Little);
    let section_id = obj.add_section(Vec::new(), b".text".to_vec(), SectionKind::Text);

    // mov rcx, 0; call kernel32!ExitProcess; ret
    let mut text = Vec::new();
    text.extend_from_slice(&[0x48, 0xC7, 0xC1]);
    text.extend_from_slice(&0u32.to_le_bytes());
    text.extend_from_slice(&[0xE8, 0, 0, 0, 0]);
    text.push(0xC3);
    obj.append_section_data(section_id, &text, 1);

    let callee_id = obj.add_symbol(Symbol {
        name: b"kernel32!ExitProcess".to_vec(),
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
            offset: 8,
            symbol: callee_id,
            addend: 0,
            flags: object::RelocationFlags::Generic {
                kind: object::RelocationKind::Relative,
                encoding: object::RelocationEncoding::X86Branch,
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
        section: SymbolSection::Section(section_id),
        flags: SymbolFlags::None,
    });

    obj.write().expect("write COFF object")
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
