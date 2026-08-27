use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;

use fp_compiler::{CompilerExecutor, CompilerSession};
use fp_core::ast::package::PackageId;
use fp_core::ast::package::provider::CompositeProvider;
use fp_core::ast::program::AstProgram;
use fp_core::ast::path::QualifiedPath;
use fp_core::frontend::LanguageFrontend;
use fp_core::lir::LirDataLayout;
use fp_interpret::LirInterpreter;
use fp_lang::provider::{FerroPhaseProvider, single_file_provider};

use host_struct_and_statics_host::{host_globals, host_layouts, HOST_POINT};

const PROGRAM: &str = include_str!("../host_struct_and_statics_host.fp");

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let layouts = host_layouts();
    assert!(layouts.get("Point").is_some());
    let frontend = fp_lang::FerroFrontend::new();
    let source_path = PathBuf::from("host_struct_and_statics_host.fp");
    let file = frontend.parse_file(PROGRAM, &source_path)?.ast;
    let package_id = PackageId::new("host_struct_and_statics_host");
    let workspace_provider = single_file_provider(
        package_id.clone(),
        QualifiedPath::new(vec![package_id.as_str().to_owned()]),
        file,
    )?;
    let provider = CompositeProvider::new(
        vec![Arc::new(FerroPhaseProvider)],
        workspace_provider,
    );
    let workspace = Rc::new(AstProgram::new(Arc::new(provider)));
    let executor = CompilerExecutor::new();
    let mut session = CompilerSession::new(
        LirDataLayout::new(64, 8, vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)])?,
        &executor,
        workspace,
    );
    executor.run(session.driver().compile_package(&package_id))?;
    executor.run(session.driver().compile_package_module_native(
        &package_id,
        &QualifiedPath::new(vec![package_id.as_str().to_owned()]),
        "main",
    ))?;
    executor.run(session.driver().evaluate_package_comptime_constants(&package_id))?;

    let mut interpreter = LirInterpreter::new();
    interpreter.set_host_globals(host_globals()?);
    interpreter
        .load_program(session.driver().state.borrow().lir_program_rc())
        .map_err(|error| error.to_string())?;
    let blob = session
        .driver()
        .state
        .borrow()
        .lir_program()
        .merged_blob_for_package(&package_id)?;
    let ferro_result = interpreter
        .run_main(&blob)
        .map_err(|error| error.to_string())?;

    println!("Ferro result: {ferro_result:?}");
    let host_point = unsafe { (HOST_POINT.x, HOST_POINT.y) };
    println!("Host Point: ({}, {})", host_point.0, host_point.1);

    assert!(ferro_result.is_unit());
    assert_eq!(host_point, (3, 4));
    Ok(())
}
