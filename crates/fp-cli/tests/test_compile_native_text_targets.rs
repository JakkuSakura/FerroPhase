use std::fs;
use tempfile::TempDir;

use fp_cli::cli::CliConfig;
use fp_cli::commands::compile::{CompileArgs, EmitterKind, compile_command};
use fp_cli::pipeline::BackendKind;

fn base_args(
    input: std::path::PathBuf,
    output: std::path::PathBuf,
    emitter: EmitterKind,
    native_target: Option<&str>,
) -> CompileArgs {
    CompileArgs {
        input: vec![input],
        backend: BackendKind::Binary,
        target: None,
        emitter,
        target_triple: None,
        target_cpu: None,
        native_target: native_target.map(ToString::to_string),
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

#[tokio::test]
async fn compile_urcl_emits_text_output() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.fp");
    let output_file = temp_dir.path().join("main.urcl");

    fs::write(
        &input_file,
        r#"
fn main() -> i64 {
    42
}
"#,
    )
    .unwrap();

    let args = base_args(input_file, output_file.clone(), EmitterKind::Urcl, None);
    compile_command(args, &CliConfig::default()).await.unwrap();

    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains("BITS 64"));
    assert!(text.contains(".function main"));
}

#[tokio::test]
async fn compile_goasm_emits_text_output() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.fp");
    let output_file = temp_dir.path().join("main.s");

    fs::write(
        &input_file,
        r#"
fn main() -> i64 {
    7
}
"#,
    )
    .unwrap();

    let args = base_args(input_file, output_file.clone(), EmitterKind::Goasm, None);
    compile_command(args, &CliConfig::default()).await.unwrap();

    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains("TEXT ·main(SB), NOSPLIT, $0-0"));
    assert!(text.contains("RET"));
}

#[tokio::test]
async fn compile_urcl_uses_default_urcl_extension() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.fp");

    fs::write(
        &input_file,
        r#"
fn main() -> i64 {
    9
}
"#,
    )
    .unwrap();

    let args = base_args(
        input_file.clone(),
        temp_dir.path().join("ignored.out"),
        EmitterKind::Urcl,
        None,
    );
    let args = CompileArgs {
        output: None,
        ..args
    };
    compile_command(args, &CliConfig::default()).await.unwrap();

    let output_file = input_file.with_extension("urcl");
    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains("BITS 64"));
}

#[tokio::test]
async fn compile_goasm_emitter_rejects_exec() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.fp");
    let output_file = temp_dir.path().join("main.s");

    fs::write(
        &input_file,
        r#"
fn main() -> i64 {
    7
}
"#,
    )
    .unwrap();

    let mut args = base_args(input_file, output_file, EmitterKind::Goasm, None);
    args.exec = true;

    let err = compile_command(args, &CliConfig::default())
        .await
        .unwrap_err();
    assert!(
        err.to_string()
            .contains("--exec is not supported for text assembly emitters")
    );
}

#[tokio::test]
async fn compile_urcl_emitter_rejects_exec() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.fp");
    let output_file = temp_dir.path().join("main.urcl");

    fs::write(
        &input_file,
        r#"
fn main() -> i64 {
    7
}
"#,
    )
    .unwrap();

    let mut args = base_args(input_file, output_file, EmitterKind::Urcl, None);
    args.exec = true;

    let err = compile_command(args, &CliConfig::default())
        .await
        .unwrap_err();
    assert!(
        err.to_string()
            .contains("--exec is not supported for text assembly emitters")
    );
}

#[tokio::test]
async fn compile_native_asm_reemits_same_isa_text() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.s");
    let output_file = temp_dir.path().join("main.out.s");

    fs::write(
        &input_file,
        ".globl main\nmain:\nbb0:\n    add v1:64, v2:64, 4\n    ret\n",
    )
    .unwrap();

    let args = CompileArgs {
        input: vec![input_file],
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
        output: Some(output_file.clone()),
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
        source_language: Some("x86_64-asm".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    compile_command(args, &CliConfig::default()).await.unwrap();

    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains(".globl main"));
    assert!(text.contains("add v1:64, v2:64, 4"));
}

#[tokio::test]
async fn compile_native_asm_transpiles_triplet_architecture() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("main.s");
    let output_file = temp_dir.path().join("main.aarch64.s");

    fs::write(
        &input_file,
        ".globl main\nmain:\nbb0:\n    add v1:64, v2:64, 4\n    ret\n",
    )
    .unwrap();

    let args = CompileArgs {
        input: vec![input_file],
        backend: BackendKind::Binary,
        target: None,
        emitter: EmitterKind::Native,
        target_triple: Some("aarch64-unknown-linux-gnu".to_string()),
        target_cpu: None,
        native_target: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: Some(output_file.clone()),
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
        source_language: Some("x86_64-asm".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    compile_command(args, &CliConfig::default()).await.unwrap();

    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains(".globl main"));
    assert!(text.contains("add "));
    assert!(text.contains("v2:64, #4"));
    assert!(text.contains("ret"));
}

#[tokio::test]
async fn compile_native_asm_transpiles_memory_load() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("load.s");
    let output_file = temp_dir.path().join("load.aarch64.s");

    fs::write(
        &input_file,
        ".globl main\nmain:\nbb0:\n    mov v1:64, [v2:64]:8\n    ret\n",
    )
    .unwrap();

    let args = CompileArgs {
        input: vec![input_file],
        backend: BackendKind::Binary,
        target: None,
        emitter: EmitterKind::Native,
        target_triple: Some("aarch64-unknown-linux-gnu".to_string()),
        target_cpu: None,
        native_target: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: Some(output_file.clone()),
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
        source_language: Some("x86_64-asm".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    compile_command(args, &CliConfig::default()).await.unwrap();

    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains("ldr "));
    assert!(text.contains("[v2:64]:8"));
}

#[tokio::test]
async fn compile_native_asm_transpiles_memory_store() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("store.s");
    let output_file = temp_dir.path().join("store.aarch64.s");

    fs::write(
        &input_file,
        ".globl main\nmain:\nbb0:\n    mov [v2:64]:8, v1:64\n    ret\n",
    )
    .unwrap();

    let args = CompileArgs {
        input: vec![input_file],
        backend: BackendKind::Binary,
        target: None,
        emitter: EmitterKind::Native,
        target_triple: Some("aarch64-unknown-linux-gnu".to_string()),
        target_cpu: None,
        native_target: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: Some(output_file.clone()),
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
        source_language: Some("x86_64-asm".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    compile_command(args, &CliConfig::default()).await.unwrap();

    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains("str "));
    assert!(text.contains("v1:64"));
    assert!(text.contains("[v2:64]:8"));
}

#[tokio::test]
async fn compile_native_asm_transpiles_indirect_register_call() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("call.s");
    let output_file = temp_dir.path().join("call.aarch64.s");

    fs::write(
        &input_file,
        ".globl main\nmain:\nbb0:\n    call v1:64\n    ret\n",
    )
    .unwrap();

    let args = CompileArgs {
        input: vec![input_file],
        backend: BackendKind::Binary,
        target: None,
        emitter: EmitterKind::Native,
        target_triple: Some("aarch64-unknown-linux-gnu".to_string()),
        target_cpu: None,
        native_target: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: Some(output_file.clone()),
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
        source_language: Some("x86_64-asm".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    compile_command(args, &CliConfig::default()).await.unwrap();

    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains("bl v1:64"));
}

#[tokio::test]
async fn compile_native_asm_transpiles_compare_branch() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("branch.s");
    let output_file = temp_dir.path().join("branch.aarch64.s");

    fs::write(
        &input_file,
        ".globl main\nmain:\nbb0:\n    cmp.eq v1:64, 0\n    jcc.eq bb1, bb2\nbb1:\n    ret\nbb2:\n    ret\n",
    )
    .unwrap();

    let args = CompileArgs {
        input: vec![input_file],
        backend: BackendKind::Binary,
        target: None,
        emitter: EmitterKind::Native,
        target_triple: Some("aarch64-unknown-linux-gnu".to_string()),
        target_cpu: None,
        native_target: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: Some(output_file.clone()),
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
        source_language: Some("x86_64-asm".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    compile_command(args, &CliConfig::default()).await.unwrap();

    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains("cmp.eq "));
    assert!(text.contains("b.eq bb1, bb2"));
}

#[tokio::test]
async fn compile_native_asm_reemits_same_isa_physical_operands() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("same-isa.s");
    let output_file = temp_dir.path().join("same-isa.out.s");

    fs::write(
        &input_file,
        ".globl main\nmain:\nbb0:\n    mov rax, [rbx + rcx*2 + 8]:8\n    call rax\n    ret\n",
    )
    .unwrap();

    let args = CompileArgs {
        input: vec![input_file],
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
        output: Some(output_file.clone()),
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
        source_language: Some("x86_64-asm".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    compile_command(args, &CliConfig::default()).await.unwrap();

    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains("mov rax, [rbx + rcx*2 + 8]:8"));
    assert!(text.contains("call rax"));
}

#[tokio::test]
async fn compile_native_asm_translates_x86_physical_registers_to_aarch64() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("x86-physical.s");
    let output_file = temp_dir.path().join("aarch64-physical.s");

    fs::write(
        &input_file,
        ".globl main\nmain:\nbb0:\n    mov rax, [rbx]:8\n    call rax\n    ret\n",
    )
    .unwrap();

    let args = CompileArgs {
        input: vec![input_file],
        backend: BackendKind::Binary,
        target: None,
        emitter: EmitterKind::Native,
        target_triple: Some("aarch64-unknown-linux-gnu".to_string()),
        target_cpu: None,
        native_target: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: Some(output_file.clone()),
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
        source_language: Some("x86_64-asm".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    compile_command(args, &CliConfig::default()).await.unwrap();

    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains("ldr v"));
    assert!(text.contains(", [v"));
    assert!(text.contains("bl v"));
}

#[tokio::test]
async fn compile_native_asm_translates_aarch64_physical_registers_to_x86() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("aarch64-physical.s");
    let output_file = temp_dir.path().join("x86-physical.s");

    fs::write(
        &input_file,
        ".globl main\nmain:\nbb0:\n    ldr x0, [x1]:8\n    bl x7\n    ret\n",
    )
    .unwrap();

    let args = CompileArgs {
        input: vec![input_file],
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
        output: Some(output_file.clone()),
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
        source_language: Some("aarch64-asm".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    compile_command(args, &CliConfig::default()).await.unwrap();

    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains("mov v"));
    assert!(text.contains(", [v"));
    assert!(text.contains("call v"));
}

#[tokio::test]
async fn compile_native_asm_translates_indexed_x86_address_to_aarch64() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("x86-indexed.s");
    let output_file = temp_dir.path().join("aarch64-indexed.s");

    fs::write(
        &input_file,
        ".globl main\nmain:\nbb0:\n    mov rax, [rbx + rcx*2 + 8]:8\n    ret\n",
    )
    .unwrap();

    let args = CompileArgs {
        input: vec![input_file],
        backend: BackendKind::Binary,
        target: None,
        emitter: EmitterKind::Native,
        target_triple: Some("aarch64-unknown-linux-gnu".to_string()),
        target_cpu: None,
        native_target: None,
        target_features: None,
        target_sysroot: None,
        linker: "clang".to_string(),
        target_linker: None,
        output: Some(output_file.clone()),
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
        source_language: Some("x86_64-asm".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    compile_command(args, &CliConfig::default()).await.unwrap();

    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains("ldr v"));
    assert!(text.contains("lsl #2, #8]:8"));
}

#[tokio::test]
async fn compile_native_asm_translates_indexed_aarch64_address_to_x86() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("aarch64-indexed.s");
    let output_file = temp_dir.path().join("x86-indexed.s");

    fs::write(
        &input_file,
        ".globl main\nmain:\nbb0:\n    str x0, [x1, x2, lsl #3, #16]:8\n    ret\n",
    )
    .unwrap();

    let args = CompileArgs {
        input: vec![input_file],
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
        output: Some(output_file.clone()),
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
        source_language: Some("aarch64-asm".to_string()),
        disable_stage: Vec::new(),
        const_eval: true,
        type_defs: false,
        single_world: false,
    };

    compile_command(args, &CliConfig::default()).await.unwrap();

    let text = fs::read_to_string(&output_file).unwrap();
    assert!(text.contains("mov [v"));
    assert!(text.contains("*3 + 16]:8"));
}
