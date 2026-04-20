use fp_core::lir::{
    CallingConvention, Linkage, LirBasicBlock, LirConstant, LirFunction, LirFunctionSignature,
    LirInstruction, LirInstructionKind, LirProgram, LirTerminator, LirType, LirValue, Name,
};

fn sample_program() -> LirProgram {
    let add_inst = LirInstruction {
        id: 1,
        kind: LirInstructionKind::Add(
            LirValue::Constant(LirConstant::Int(40, LirType::I64)),
            LirValue::Constant(LirConstant::Int(2, LirType::I64)),
        ),
        type_hint: Some(LirType::I64),
        debug_info: None,
    };
    let main = LirFunction {
        name: Name::new("main"),
        signature: LirFunctionSignature {
            params: Vec::new(),
            return_type: LirType::I64,
            is_variadic: false,
        },
        basic_blocks: vec![LirBasicBlock {
            id: 0,
            label: Some(Name::new("entry")),
            instructions: vec![add_inst],
            terminator: LirTerminator::Return(Some(LirValue::Register(1))),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }],
        locals: Vec::new(),
        stack_slots: Vec::new(),
        calling_convention: CallingConvention::C,
        linkage: Linkage::External,
        is_declaration: false,
    };
    LirProgram {
        functions: vec![main],
        globals: Vec::new(),
        type_definitions: Vec::new(),
        queries: Vec::new(),
    }
}

fn go_tool_available() -> bool {
    std::process::Command::new("go")
        .arg("version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

#[test]
fn emits_goasm_text_for_amd64() {
    let out_dir = tempfile::tempdir().unwrap();
    let path = out_dir.path().join("main.s");
    let cfg = fp_goasm::config::GoAsmConfig::new(&path)
        .with_target(Some(fp_goasm::config::GoAsmTarget::Amd64));
    let emitter = fp_goasm::GoAsmEmitter::new(cfg);
    emitter.emit(sample_program(), None).unwrap();
    let text = std::fs::read_to_string(&path).unwrap();
    assert!(text.contains("#include \"textflag.h\""));
    assert!(text.contains("TEXT ·main(SB), NOSPLIT, $0-0"));
    assert!(text.contains("MOVQ $40, R11"));
    assert!(text.contains("ADDQ $2, R11"));
}

#[test]
fn goasm_arm64_text_assembles() {
    if !go_tool_available() {
        return;
    }
    let goroot = String::from_utf8(
        std::process::Command::new("go")
            .args(["env", "GOROOT"])
            .output()
            .unwrap()
            .stdout,
    )
    .unwrap();
    let out_dir = tempfile::tempdir().unwrap();
    let path = out_dir.path().join("main.s");
    let cfg = fp_goasm::config::GoAsmConfig::new(&path)
        .with_target(Some(fp_goasm::config::GoAsmTarget::Arm64));
    fp_goasm::GoAsmEmitter::new(cfg)
        .emit(sample_program(), None)
        .unwrap();
    let status = std::process::Command::new("go")
        .args([
            "tool",
            "asm",
            "-I",
            &format!("{}/pkg/include", goroot.trim()),
            path.to_str().unwrap(),
        ])
        .current_dir(out_dir.path())
        .env("GOARCH", "arm64")
        .status()
        .unwrap();
    assert!(status.success());
}
