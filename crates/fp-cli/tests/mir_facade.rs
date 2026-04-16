use std::path::PathBuf;
use std::thread;

use fp_cli::languages;
use fp_cli::pipeline::{BackendKind, Pipeline, PipelineOptions};
use fp_core::{hir, mir};

fn examples_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../..")
        .join("examples")
}

fn assert_query_bundle(name: &'static str, expected_language: &'static str) {
    thread::Builder::new()
        .name(format!("fp-cli-mir-{name}"))
        .stack_size(32 * 1024 * 1024)
        .spawn(move || {
            let path = examples_root().join(name);
            let mut pipeline = Pipeline::new();
            let mut options = PipelineOptions::default();
            options.target = BackendKind::Bytecode;
            let bundle = pipeline
                .compile_file_to_mir(path.as_path(), options)
                .expect("compile example to mir");
            assert_eq!(bundle.frontend.source_language, expected_language);
            assert!(
                bundle
                    .mir_program
                    .items
                    .iter()
                    .any(|item| matches!(item.kind, mir::ItemKind::Query(_))),
                "expected query item in MIR for {name}"
            );
        })
        .expect("spawn compile thread")
        .join()
        .expect("join compile thread");
}

fn assert_host_query_bundle(name: &'static str) {
    thread::Builder::new()
        .name(format!("fp-cli-host-mir-{name}"))
        .stack_size(32 * 1024 * 1024)
        .spawn(move || {
            let path = examples_root().join(name);
            let mut pipeline = Pipeline::new();
            let mut options = PipelineOptions::default();
            options.target = BackendKind::Bytecode;
            let bundle = pipeline
                .compile_file_to_mir(path.as_path(), options)
                .expect("compile host example to mir");
            assert_eq!(bundle.frontend.source_language, languages::FERROPHASE);
            assert!(
                bundle
                    .hir_program
                    .items
                    .iter()
                    .any(|item| matches!(item.kind, hir::ItemKind::Function(_))),
                "expected function item in HIR for {name}"
            );
            assert!(
                bundle
                    .hir_program
                    .items
                    .iter()
                    .any(|item| matches!(item.kind, hir::ItemKind::Query(_))),
                "expected embedded query item in HIR for {name}"
            );
            assert!(
                bundle
                    .mir_program
                    .items
                    .iter()
                    .any(|item| matches!(item.kind, mir::ItemKind::Function(_))),
                "expected function item in MIR for {name}"
            );
            assert!(
                bundle
                    .mir_program
                    .items
                    .iter()
                    .any(|item| matches!(item.kind, mir::ItemKind::Query(_))),
                "expected embedded query item in MIR for {name}"
            );
        })
        .expect("spawn compile thread")
        .join()
        .expect("join compile thread");
}

#[test]
fn compiles_fp_example_to_mir_bundle() {
    assert_query_bundle("query.fp", languages::FERROPHASE);
}

#[test]
fn compiles_sql_example_to_mir_bundle() {
    assert_query_bundle("query.sql", languages::SQL);
}

#[test]
fn compiles_prql_example_to_mir_bundle() {
    assert_query_bundle("query.prql", languages::PRQL);
}

#[test]
fn compiles_host_query_main_fp_to_mir_bundle() {
    assert_host_query_bundle("host_query_main.fp");
}

#[test]
fn compiles_host_query_functions_fp_to_mir_bundle() {
    assert_host_query_bundle("host_query_functions.fp");
}

#[test]
fn compiles_host_query_struct_methods_fp_to_mir_bundle() {
    assert_host_query_bundle("host_query_struct_methods.fp");
}
