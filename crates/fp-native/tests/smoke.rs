use fp_core::lir::{
    CallingConvention, LirBasicBlock, LirFunction, LirFunctionSignature, LirProgram, LirTerminator,
    LirType, Linkage, Name,
};

#[test]
fn emits_and_links_minimal_exec() {
    // This test validates the end-to-end plumbing:
    // LIR -> native binary emission -> runnable executable.
    let lir = LirProgram {
        functions: vec![LirFunction {
            name: Name::new("main"),
            signature: LirFunctionSignature {
                params: Vec::new(),
                return_type: LirType::I32,
                is_variadic: false,
            },
            basic_blocks: vec![LirBasicBlock {
                id: 0,
                label: Some(Name::new("entry")),
                instructions: Vec::new(),
                terminator: LirTerminator::Return(None),
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            calling_convention: CallingConvention::C,
            linkage: Linkage::External,
        }],
        globals: Vec::new(),
        type_definitions: Vec::new(),
    };

    let out_dir = tempfile::tempdir().unwrap();
    let exe = out_dir.path().join("smoke.out");

    let cfg = fp_native::config::NativeConfig::executable(&exe);
    let c = fp_native::NativeEmitter::new(cfg);
    let produced = c.emit(lir, None).unwrap();
    assert_eq!(produced, exe);

    if cfg!(any(target_arch = "x86_64", target_arch = "aarch64")) {
        let status = std::process::Command::new(&exe).status().unwrap();
        assert!(status.success());
    }
}
