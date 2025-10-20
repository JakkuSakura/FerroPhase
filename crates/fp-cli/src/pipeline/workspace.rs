use std::collections::{HashMap, HashSet};

use fp_core::lir;
use fp_core::workspace::{WorkspaceDocument, WorkspacePackage, WorkspaceModule};

/// Minimal LIR capture per workspace module during replay.
#[derive(Clone)]
pub(crate) struct WorkspaceLirModule {
    pub(crate) id: String,
    pub(crate) package: String,
    pub(crate) kind: Option<String>,
    pub(crate) lir: lir::LirProgram,
}

/// Accumulates results of per-module LIR replay for workspace assembly.
#[derive(Clone, Default)]
pub(crate) struct WorkspaceLirReplay {
    pub(crate) modules: Vec<WorkspaceLirModule>,
    pub(crate) missing_snapshots: Vec<String>,
    pub(crate) failed_modules: Vec<(String, String)>,
}

/// Derive the main package name used for body retention in merged LIR.
pub(crate) fn determine_main_package_name(workspace: &WorkspaceDocument) -> String {
    // Priority: FP_BOOTSTRAP_MAIN → package named "fp-cli" → first package name
    if let Ok(val) = std::env::var("FP_BOOTSTRAP_MAIN") {
        if !val.trim().is_empty() {
            return val;
        }
    }
    if let Some(pkg) = workspace.packages.iter().find(|p| p.name == "fp-cli") {
        return pkg.name.clone();
    }
    workspace
        .packages
        .first()
        .map(|p| p.name.clone())
        .unwrap_or_else(|| "main".to_string())
}

/// Assemble a single LIR program for the workspace, keeping full bodies for the main package
/// and externalizing others.
pub(crate) fn assemble_workspace_lir_program(
    workspace: &WorkspaceDocument,
    lir_replay: &WorkspaceLirReplay,
) -> lir::LirProgram {
    use fp_core::lir::Linkage;

    let main_package = determine_main_package_name(workspace);

    // Index whether to keep bodies per module id
    let mut keep_body: HashMap<&str, bool> = HashMap::new();
    for m in &lir_replay.modules {
        keep_body.insert(m.id.as_str(), m.package == main_package);
    }

    let mut merged = lir::LirProgram::new();
    let mut seen_funcs: HashSet<String> = HashSet::new();
    let mut seen_globals: HashSet<String> = HashSet::new();
    let mut seen_types: HashSet<String> = HashSet::new();

    // First, add definitions with bodies for main package modules
    for m in &lir_replay.modules {
        if !keep_body.get(m.id.as_str()).copied().unwrap_or(false) {
            continue;
        }
        for f in m.lir.functions.clone() {
            let name = f.name.as_str().to_string();
            if seen_funcs.insert(name) {
                merged.add_function(f);
            }
        }
        for g in m.lir.globals.clone() {
            let name = g.name.as_str().to_string();
            if seen_globals.insert(name) {
                merged.add_global(g);
            }
        }
        for t in m.lir.type_definitions.clone() {
            let name = t.name.as_str().to_string();
            if seen_types.insert(name) {
                merged.type_definitions.push(t);
            }
        }
    }

    // Then, add declarations for other modules
    for m in &lir_replay.modules {
        if keep_body.get(m.id.as_str()).copied().unwrap_or(false) {
            continue;
        }
        for mut f in m.lir.functions.clone() {
            let name = f.name.as_str().to_string();
            if seen_funcs.insert(name) {
                f.basic_blocks.clear();
                f.locals.clear();
                f.stack_slots.clear();
                f.linkage = Linkage::External;
                merged.add_function(f);
            }
        }
        for g in m.lir.globals.clone() {
            let name = g.name.as_str().to_string();
            if seen_globals.insert(name) {
                merged.add_global(g);
            }
        }
        for t in m.lir.type_definitions.clone() {
            let name = t.name.as_str().to_string();
            if seen_types.insert(name) {
                merged.type_definitions.push(t);
            }
        }
    }

    merged
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::lir::{CallingConvention, LirBasicBlock, LirFunction, LirFunctionSignature, LirProgram, Linkage, Name, Ty as LirType};
    use fp_core::workspace::{WorkspaceDependency, WorkspacePackage};

    fn make_func(name: &str) -> LirFunction {
        let sig = LirFunctionSignature { params: vec![], return_type: LirType::Void, is_variadic: false };
        let mut f = LirFunction::new(Name::from(name), sig, CallingConvention::C, Linkage::External);
        // Add a dummy block so we can detect body presence
        f.add_basic_block(LirBasicBlock::new(0, None));
        f
    }

    fn program_with(name: &str) -> LirProgram {
        let mut p = LirProgram::new();
        p.add_function(make_func(name));
        p
    }

    fn workspace_with(pkgs: &[(&str, &[&str])]) -> WorkspaceDocument {
        let packages = pkgs
            .iter()
            .map(|(name, modules)| {
                let mods = modules
                    .iter()
                    .map(|m| WorkspaceModule::new(format!("{name}::{m}"), format!("{name}/{m}.json")).with_module_path(vec!["bin".to_string()]))
                    .collect();
                WorkspacePackage::new(*name, format!("{name}/Cargo.toml"), format!("{name}"))
                    .with_modules(mods)
                    .with_dependencies(vec![WorkspaceDependency::new("std", None)])
            })
            .collect();
        WorkspaceDocument::new("dummy").with_packages(packages)
    }

    #[test]
    fn merge_keeps_main_bodies_and_externalizes_others() {
        // Given two packages, main = alpha, with one module each, define the same function name.
        let ws = workspace_with(&[("fp-cli", &["a"]), ("beta", &["b"]) ]);

        let mut replay = WorkspaceLirReplay::default();
        replay.modules.push(WorkspaceLirModule {
            id: "fp-cli::a".to_string(),
            package: "fp-cli".to_string(),
            kind: Some("bin".to_string()),
            lir: program_with("foo"),
        });
        replay.modules.push(WorkspaceLirModule {
            id: "beta::b".to_string(),
            package: "beta".to_string(),
            kind: Some("bin".to_string()),
            lir: {
                let mut p = LirProgram::new();
                p.add_function(make_func("foo"));
                p.add_function(make_func("bar")); // unique to non-main
                p
            },
        });

        let merged = assemble_workspace_lir_program(&ws, &replay);

        // Expect `foo` has a body from alpha and `bar` exists but is external (no blocks).
        let mut found_foo = None;
        let mut found_bar = None;
        for f in &merged.functions {
            match f.name.as_str() {
                "foo" => found_foo = Some(f.clone()),
                "bar" => found_bar = Some(f.clone()),
                _ => {}
            }
        }
        let foo = found_foo.expect("foo present");
        assert!(foo.basic_blocks.len() >= 1, "foo has body from main package");
        let bar = found_bar.expect("bar present");
        assert!(bar.basic_blocks.is_empty(), "bar is externalized (no body)");
    }
}
