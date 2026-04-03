use assert_cmd::Command;
use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, EmitterKind, compile_command};
use fp_cli::pipeline::BackendKind;
use predicates::prelude::*;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

fn fp_cmd() -> Command {
    Command::new(env!("CARGO_BIN_EXE_fp"))
}

#[test]
fn inspect_macho_fat_lists_architectures() {
    let temp_dir = TempDir::new().unwrap();
    let input = temp_dir.path().join("fat.bin");
    let mut bytes = vec![0u8; 8 + 20];
    bytes[0..4].copy_from_slice(&[0xCA, 0xFE, 0xBA, 0xBE]);
    bytes[4..8].copy_from_slice(&1u32.to_be_bytes());
    bytes[8..12].copy_from_slice(&0x0100000cu32.to_be_bytes());
    bytes[12..16].copy_from_slice(&0u32.to_be_bytes());
    bytes[16..20].copy_from_slice(&0x1000u32.to_be_bytes());
    bytes[20..24].copy_from_slice(&0x2000u32.to_be_bytes());
    bytes[24..28].copy_from_slice(&14u32.to_be_bytes());
    fs::write(&input, bytes).unwrap();

    fp_cmd()
        .arg("inspect")
        .arg(&input)
        .arg("--format")
        .arg("native")
        .assert()
        .success()
        .stdout(predicate::str::contains("kind: macho-fat32"))
        .stdout(predicate::str::contains("macho_arch_count: 1"))
        .stdout(predicate::str::contains("arch#0 cpu=arm64"));
}

fn base_compile_args(input: PathBuf, output: PathBuf) -> CompileArgs {
    CompileArgs {
        input: vec![input],
        backend: BackendKind::Bytecode,
        target: None,
        emitter: EmitterKind::Native,
        target_triple: None,
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
        link: false,
        save_intermediates: false,
        lossy: false,
        max_errors: 10,
        source_language: None,
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    }
}

fn compile_args_for_backend(input: PathBuf, output: PathBuf, backend: BackendKind) -> CompileArgs {
    let mut args = base_compile_args(input, output);
    args.backend = backend;
    args
}

#[test]
fn inspect_help_is_available() {
    fp_cmd()
        .arg("inspect")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains(
            "Inspect binary, bytecode, and object artifacts",
        ));
}

#[tokio::test]
async fn inspect_binary_bytecode_can_export_text_form() {
    let temp_dir = TempDir::new().unwrap();
    let input = temp_dir.path().join("main.fp");
    let bytecode = temp_dir.path().join("main.fbc");
    let text_output = temp_dir.path().join("main.ftbc");

    fs::write(
        &input,
        r#"
fn main() {
    println!("hello inspect");
}
"#,
    )
    .unwrap();

    compile_command(
        base_compile_args(input, bytecode.clone()),
        &CliConfig::default(),
    )
    .await
    .unwrap();

    fp_cmd()
        .arg("inspect")
        .arg(&bytecode)
        .arg("--export-text-bytecode")
        .arg(&text_output)
        .assert()
        .success()
        .stdout(predicate::str::contains("format: ferro-bytecode-binary"))
        .stdout(predicate::str::contains("function_count: 1"));

    let rendered = fs::read_to_string(text_output).unwrap();
    assert!(rendered.contains("fp-bytecode {"));
    assert!(rendered.contains("fn main"));
}

#[test]
fn inspect_native_binary_prints_summary() {
    fp_cmd()
        .arg("inspect")
        .arg(env!("CARGO_BIN_EXE_fp"))
        .assert()
        .success()
        .stdout(predicate::str::contains("format: native-object"))
        .stdout(predicate::str::contains("macho_"))
        .stdout(predicate::str::contains("architecture:"))
        .stdout(predicate::str::contains("section_count:"));
}

#[tokio::test]
async fn inspect_ebpf_object_prints_runtime_metadata_and_relocations() {
    let temp_dir = TempDir::new().unwrap();
    let input = temp_dir.path().join("main.fp");
    let object = temp_dir.path().join("main.o");

    fs::write(
        &input,
        r#"
fn main() -> i32 {
    println!("value={}", 7)
    0
}
"#,
    )
    .unwrap();

    compile_command(
        compile_args_for_backend(input, object.clone(), BackendKind::Ebpf),
        &CliConfig::default(),
    )
    .await
    .unwrap();

    fp_cmd()
        .arg("inspect")
        .arg(&object)
        .assert()
        .success()
        .stdout(predicate::str::contains("format: native-object"))
        .stdout(predicate::str::contains("architecture: Bpf"))
        .stdout(predicate::str::contains("ebpf_metadata: present"))
        .stdout(predicate::str::contains("ebpf_helper: id=3 name=println symbol=__fp_helper_println"))
        .stdout(predicate::str::contains("ebpf_format: id=0 template=\"value=%lld\\n\""))
        .stdout(predicate::str::contains(
            "ebpf_call: function=main offset=40 helper_id=3 helper_symbol=__fp_helper_println format_id=0 arg_count=1",
        ))
        .stdout(predicate::str::contains(
            "ebpf_relocation: section=prog/main offset=40 symbol=__fp_helper_println",
        ));
}

#[test]
fn inspect_unknown_file_falls_back_to_raw() {
    let temp_dir = TempDir::new().unwrap();
    let input = temp_dir.path().join("blob.bin");
    fs::write(&input, [0xde, 0xad, 0xbe, 0xef, 0x00, 0x01]).unwrap();

    fp_cmd()
        .arg("inspect")
        .arg(&input)
        .assert()
        .success()
        .stdout(predicate::str::contains("format: raw"))
        .stdout(predicate::str::contains("size: 6"));
}

#[test]
fn inspect_jvm_class_uses_class_header_not_macho_fat() {
    let temp_dir = TempDir::new().unwrap();
    let input = temp_dir.path().join("Main.class");
    let bytes = [
        0xCA, 0xFE, 0xBA, 0xBE, // magic
        0x00, 0x00, // minor
        0x00, 0x34, // major = 52
        0x00, 0x01, // constant_pool_count
    ];
    fs::write(&input, bytes).unwrap();

    fp_cmd()
        .arg("inspect")
        .arg(&input)
        .assert()
        .success()
        .stdout(predicate::str::contains("format: jvm-class"))
        .stdout(predicate::str::contains("version: 52.0"));
}

#[test]
fn inspect_wasm_detects_header_family() {
    let temp_dir = TempDir::new().unwrap();
    let input = temp_dir.path().join("module.wasm");
    let bytes = [
        0x00, 0x61, 0x73, 0x6d, // magic
        0x01, 0x00, 0x00, 0x00, // version 1
    ];
    fs::write(&input, bytes).unwrap();

    fp_cmd()
        .arg("inspect")
        .arg(&input)
        .assert()
        .success()
        .stdout(predicate::str::contains("format: wasm"))
        .stdout(predicate::str::contains("wasm_version: 1"));
}

#[test]
fn inspect_archive_detects_magic() {
    let temp_dir = TempDir::new().unwrap();
    let input = temp_dir.path().join("libfoo.a");
    fs::write(&input, b"!<arch>\n").unwrap();

    fp_cmd()
        .arg("inspect")
        .arg(&input)
        .assert()
        .success()
        .stdout(predicate::str::contains("format: archive"))
        .stdout(predicate::str::contains("member_count: 0"));
}

#[test]
fn inspect_elf_header_reports_elf_metadata_without_full_parse() {
    let temp_dir = TempDir::new().unwrap();
    let input = temp_dir.path().join("tiny.elf");
    let mut bytes = vec![0u8; 0x240];
    bytes[0..4].copy_from_slice(&[0x7f, b'E', b'L', b'F']);
    bytes[4] = 2;
    bytes[5] = 1;
    bytes[6] = 1;
    bytes[16..18].copy_from_slice(&2u16.to_le_bytes());
    bytes[18..20].copy_from_slice(&0x3eu16.to_le_bytes());
    bytes[24..32].copy_from_slice(&0x401000u64.to_le_bytes());
    bytes[32..40].copy_from_slice(&64u64.to_le_bytes());
    bytes[40..48].copy_from_slice(&0u64.to_le_bytes());
    bytes[52..54].copy_from_slice(&64u16.to_le_bytes());
    bytes[54..56].copy_from_slice(&56u16.to_le_bytes());
    bytes[56..58].copy_from_slice(&3u16.to_le_bytes());
    bytes[58..60].copy_from_slice(&64u16.to_le_bytes());
    bytes[60..62].copy_from_slice(&1u16.to_le_bytes());

    let ph0 = 64usize;
    bytes[ph0..ph0 + 4].copy_from_slice(&1u32.to_le_bytes());
    bytes[ph0 + 4..ph0 + 8].copy_from_slice(&5u32.to_le_bytes());
    bytes[ph0 + 8..ph0 + 16].copy_from_slice(&0u64.to_le_bytes());
    bytes[ph0 + 16..ph0 + 24].copy_from_slice(&0x401000u64.to_le_bytes());
    bytes[ph0 + 32..ph0 + 40].copy_from_slice(&0x34u64.to_le_bytes());
    bytes[ph0 + 40..ph0 + 48].copy_from_slice(&0x34u64.to_le_bytes());
    bytes[ph0 + 48..ph0 + 56].copy_from_slice(&0x1000u64.to_le_bytes());

    let ph1 = ph0 + 56;
    bytes[ph1..ph1 + 4].copy_from_slice(&3u32.to_le_bytes());
    bytes[ph1 + 4..ph1 + 8].copy_from_slice(&4u32.to_le_bytes());
    bytes[ph1 + 8..ph1 + 16].copy_from_slice(&0x200u64.to_le_bytes());
    bytes[ph1 + 16..ph1 + 24].copy_from_slice(&0x402000u64.to_le_bytes());
    bytes[ph1 + 32..ph1 + 40].copy_from_slice(&28u64.to_le_bytes());
    bytes[ph1 + 40..ph1 + 48].copy_from_slice(&28u64.to_le_bytes());
    bytes[ph1 + 48..ph1 + 56].copy_from_slice(&1u64.to_le_bytes());

    let ph2 = ph1 + 56;
    bytes[ph2..ph2 + 4].copy_from_slice(&2u32.to_le_bytes());
    bytes[ph2 + 4..ph2 + 8].copy_from_slice(&4u32.to_le_bytes());
    bytes[ph2 + 8..ph2 + 16].copy_from_slice(&0x220u64.to_le_bytes());
    bytes[ph2 + 16..ph2 + 24].copy_from_slice(&0x402020u64.to_le_bytes());
    bytes[ph2 + 32..ph2 + 40].copy_from_slice(&32u64.to_le_bytes());
    bytes[ph2 + 40..ph2 + 48].copy_from_slice(&32u64.to_le_bytes());
    bytes[ph2 + 48..ph2 + 56].copy_from_slice(&8u64.to_le_bytes());

    let interp = b"/lib64/ld-linux-x86-64.so.2\0";
    bytes[0x200..0x200 + interp.len()].copy_from_slice(interp);
    bytes[0x220..0x228].copy_from_slice(&1i64.to_le_bytes());
    bytes[0x228..0x230].copy_from_slice(&5u64.to_le_bytes());
    bytes[0x230..0x238].copy_from_slice(&0i64.to_le_bytes());
    bytes[0x238..0x240].copy_from_slice(&0u64.to_le_bytes());
    fs::write(&input, bytes).unwrap();

    fp_cmd()
        .arg("inspect")
        .arg(&input)
        .assert()
        .success()
        .stdout(predicate::str::contains("kind: elf64"))
        .stdout(predicate::str::contains("elf_class: ELF64"))
        .stdout(predicate::str::contains("elf_machine: x86_64"))
        .stdout(predicate::str::contains("phdr#0 type=load flags=rx"))
        .stdout(predicate::str::contains(
            "interp: /lib64/ld-linux-x86-64.so.2",
        ))
        .stdout(predicate::str::contains("dyn#0 tag=needed value=0x5"));
}

#[test]
fn inspect_pe_header_reports_pe_metadata_without_full_parse() {
    let temp_dir = TempDir::new().unwrap();
    let input = temp_dir.path().join("tiny.exe");
    let mut bytes = vec![0u8; 0x800];
    bytes[0..2].copy_from_slice(b"MZ");
    bytes[0x3c..0x40].copy_from_slice(&0x80u32.to_le_bytes());
    bytes[0x80..0x84].copy_from_slice(b"PE\0\0");
    bytes[0x84..0x86].copy_from_slice(&0x8664u16.to_le_bytes());
    bytes[0x86..0x88].copy_from_slice(&2u16.to_le_bytes());
    bytes[0x94..0x96].copy_from_slice(&0xf0u16.to_le_bytes());
    bytes[0x96..0x98].copy_from_slice(&0x22u16.to_le_bytes());
    let opt = 0x98usize;
    bytes[opt..opt + 2].copy_from_slice(&0x20bu16.to_le_bytes());
    bytes[opt + 16..opt + 20].copy_from_slice(&0x1000u32.to_le_bytes());
    bytes[opt + 24..opt + 32].copy_from_slice(&0x140000000u64.to_le_bytes());
    bytes[opt + 88..opt + 90].copy_from_slice(&3u16.to_le_bytes());

    bytes[opt + 112..opt + 120].copy_from_slice(&[0x00, 0x20, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00]);
    bytes[opt + 120..opt + 128].copy_from_slice(&[0x00, 0x21, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00]);

    let sec = 0x188usize;
    bytes[sec..sec + 8].copy_from_slice(b".text\0\0\0");
    bytes[sec + 8..sec + 12].copy_from_slice(&0x200u32.to_le_bytes());
    bytes[sec + 12..sec + 16].copy_from_slice(&0x1000u32.to_le_bytes());
    bytes[sec + 16..sec + 20].copy_from_slice(&0x200u32.to_le_bytes());
    bytes[sec + 20..sec + 24].copy_from_slice(&0x200u32.to_le_bytes());
    bytes[sec + 36..sec + 40].copy_from_slice(&0x60000020u32.to_le_bytes());

    let sec2 = sec + 40;
    bytes[sec2..sec2 + 8].copy_from_slice(b".rdata\0\0");
    bytes[sec2 + 8..sec2 + 12].copy_from_slice(&0x400u32.to_le_bytes());
    bytes[sec2 + 12..sec2 + 16].copy_from_slice(&0x2000u32.to_le_bytes());
    bytes[sec2 + 16..sec2 + 20].copy_from_slice(&0x400u32.to_le_bytes());
    bytes[sec2 + 20..sec2 + 24].copy_from_slice(&0x400u32.to_le_bytes());
    bytes[sec2 + 36..sec2 + 40].copy_from_slice(&0x40000040u32.to_le_bytes());

    let export_dir = 0x400usize;
    bytes[export_dir + 12..export_dir + 16].copy_from_slice(&0x2050u32.to_le_bytes());
    bytes[export_dir + 16..export_dir + 20].copy_from_slice(&1u32.to_le_bytes());
    bytes[export_dir + 20..export_dir + 24].copy_from_slice(&1u32.to_le_bytes());
    bytes[export_dir + 24..export_dir + 28].copy_from_slice(&1u32.to_le_bytes());
    bytes[export_dir + 28..export_dir + 32].copy_from_slice(&0x2060u32.to_le_bytes());
    bytes[export_dir + 32..export_dir + 36].copy_from_slice(&0x2070u32.to_le_bytes());
    bytes[export_dir + 36..export_dir + 40].copy_from_slice(&0x2080u32.to_le_bytes());
    bytes[0x450..0x459].copy_from_slice(b"test.dll\0");
    bytes[0x460..0x464].copy_from_slice(&0x1000u32.to_le_bytes());
    bytes[0x470..0x474].copy_from_slice(&0x2090u32.to_le_bytes());
    bytes[0x480..0x482].copy_from_slice(&0u16.to_le_bytes());
    bytes[0x490..0x49b].copy_from_slice(b"ExportedFn\0");

    let import_dir = 0x500usize;
    bytes[import_dir..import_dir + 4].copy_from_slice(&0x2110u32.to_le_bytes());
    bytes[import_dir + 12..import_dir + 16].copy_from_slice(&0x2140u32.to_le_bytes());
    bytes[import_dir + 16..import_dir + 20].copy_from_slice(&0x2110u32.to_le_bytes());
    bytes[0x510..0x518].copy_from_slice(&0x2130u64.to_le_bytes());
    bytes[0x518..0x520].copy_from_slice(&0u64.to_le_bytes());
    bytes[0x530..0x532].copy_from_slice(&7u16.to_le_bytes());
    bytes[0x532..0x53d].copy_from_slice(b"ImportedFn\0");
    bytes[0x540..0x54d].copy_from_slice(b"KERNEL32.dll\0");
    fs::write(&input, bytes).unwrap();

    fp_cmd()
        .arg("inspect")
        .arg(&input)
        .assert()
        .success()
        .stdout(predicate::str::contains("kind: pe64"))
        .stdout(predicate::str::contains("pe_magic: PE32+"))
        .stdout(predicate::str::contains("pe_coff_machine: x86_64"))
        .stdout(predicate::str::contains(
            "dir#0 kind=export rva=0x2000 size=64",
        ))
        .stdout(predicate::str::contains("section#1 name=.rdata"))
        .stdout(predicate::str::contains("pe_exports: dll=test.dll"))
        .stdout(predicate::str::contains(
            "export#0 name=ExportedFn ordinal=1 rva=0x1000",
        ))
        .stdout(predicate::str::contains("dll#0 name=KERNEL32.dll"))
        .stdout(predicate::str::contains("import#0 name=ImportedFn hint=7"));
}
