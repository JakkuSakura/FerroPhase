use fp_shell::{CompileOptions, ScriptTarget, compile_source_with_options, load_inventory};
use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Deserialize)]
struct CaseManifest {
    copied_from: Option<String>,
    inventory: Option<String>,
    absent: Vec<String>,
    contains: Vec<String>,
    target: Option<String>,
}

#[test]
fn pyinfra_cases_compile_to_expected_shell_output() {
    let fixtures_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("pyinfra_port");
    let case_paths = collect_case_paths(&fixtures_root);
    assert!(
        !case_paths.is_empty(),
        "expected at least one case under {}",
        fixtures_root.display()
    );

    for case_path in case_paths {
        run_case(&case_path);
    }
}

fn collect_case_paths(root: &Path) -> Vec<PathBuf> {
    let mut pending = vec![root.to_path_buf()];
    let mut case_paths = Vec::new();

    while let Some(path) = pending.pop() {
        let entries = fs::read_dir(&path).unwrap_or_else(|error| {
            panic!(
                "failed to read fixture directory {}: {error}",
                path.display()
            )
        });

        for entry in entries {
            let entry = entry.unwrap_or_else(|error| {
                panic!(
                    "failed to read fixture entry in {}: {error}",
                    path.display()
                )
            });
            let entry_path = entry.path();
            if entry
                .file_type()
                .map(|file_type| file_type.is_dir())
                .unwrap_or(false)
            {
                if entry_path.join("case.json").is_file() {
                    case_paths.push(entry_path);
                } else {
                    pending.push(entry_path);
                }
            }
        }
    }

    case_paths.sort();
    case_paths
}

fn run_case(case_path: &Path) {
    let manifest_path = case_path.join("case.json");
    let manifest: CaseManifest = serde_json::from_str(
        &fs::read_to_string(&manifest_path)
            .unwrap_or_else(|error| panic!("failed to read {}: {error}", manifest_path.display())),
    )
    .unwrap_or_else(|error| panic!("failed to parse {}: {error}", manifest_path.display()));
    let source_path = case_path.join("source.fp");
    let source = fs::read_to_string(&source_path)
        .unwrap_or_else(|error| panic!("failed to read {}: {error}", source_path.display()));
    let inventory = manifest.inventory.as_ref().map(|inventory_name| {
        let inventory_path = case_path.join(inventory_name);
        load_inventory(&inventory_path).unwrap_or_else(|error| {
            panic!(
                "failed to load inventory for {} from {}: {error}",
                case_name(case_path),
                inventory_path.display()
            )
        })
    });

    let output = compile_source_with_options(
        &source,
        &source_path,
        parse_target(manifest.target.as_deref()),
        &CompileOptions { inventory },
    )
    .unwrap_or_else(|error| {
        let copied_from = manifest
            .copied_from
            .as_deref()
            .map(|value| format!(" ({value})"))
            .unwrap_or_default();
        panic!(
            "failed to compile case {}{}: {error}",
            case_name(case_path),
            copied_from
        )
    });

    for fragment in &manifest.contains {
        assert!(
            output.code.contains(fragment),
            "case {} missing fragment {:?}\n\n{}",
            case_name(case_path),
            fragment,
            output.code
        );
    }

    for fragment in &manifest.absent {
        assert!(
            !output.code.contains(fragment),
            "case {} unexpectedly contained fragment {:?}\n\n{}",
            case_name(case_path),
            fragment,
            output.code
        );
    }
}

fn case_name(case_path: &Path) -> String {
    case_path
        .strip_prefix(
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("tests")
                .join("fixtures")
                .join("pyinfra_port"),
        )
        .unwrap_or(case_path)
        .display()
        .to_string()
}

fn parse_target(raw: Option<&str>) -> ScriptTarget {
    match raw.unwrap_or("bash") {
        "bash" => ScriptTarget::Bash,
        "powershell" => ScriptTarget::PowerShell,
        other => panic!("unsupported fixture target: {other}"),
    }
}
