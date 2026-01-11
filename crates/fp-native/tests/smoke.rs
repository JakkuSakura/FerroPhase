use fp_core::lir::LirProgram;

#[test]
fn emits_and_links_minimal_exec() {
    // This test validates the end-to-end plumbing:
    // LIR -> Mach-O object emission -> clang link -> runnable executable.
    let lir = LirProgram::new();

    let out_dir = tempfile::tempdir().unwrap();
    let exe = out_dir.path().join("smoke.out");

    let cfg = fp_native::config::NativeConfig::executable(&exe);
    let c = fp_native::NativeCompiler::new(cfg);
    let produced = c.compile(lir, None).unwrap();
    assert_eq!(produced, exe);

    let status = std::process::Command::new(&exe).status().unwrap();
    assert!(status.success());
}

