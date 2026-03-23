use fp_shell::{CompileOptions, ScriptTarget, compile_source_with_options};
use serde::Deserialize;
use serde_json::Value;
use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

#[derive(Debug, Deserialize)]
struct UpstreamCase {
    args: Vec<Value>,
    #[serde(default)]
    kwargs: BTreeMap<String, Value>,
}

#[test]
fn pyinfra_upstream_ssh_cases_compile_to_expected_invocations() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("pyinfra_upstream")
        .join("operations");
    let families = ["ssh.command", "ssh.keyscan", "ssh.upload", "ssh.download"];
    let mut case_paths = Vec::new();

    for family in families {
        let family_root = root.join(family);
        let entries = fs::read_dir(&family_root)
            .unwrap_or_else(|error| panic!("failed to read {}: {error}", family_root.display()));
        for entry in entries {
            let entry = entry.unwrap_or_else(|error| {
                panic!("failed to read entry in {}: {error}", family_root.display())
            });
            let path = entry.path();
            if path.extension().and_then(|ext| ext.to_str()) == Some("json") {
                case_paths.push(path);
            }
        }
    }

    case_paths.sort();
    assert!(!case_paths.is_empty(), "expected upstream ssh fixtures");

    for case_path in case_paths {
        run_case(&case_path);
    }
}

fn run_case(case_path: &Path) {
    let raw = fs::read_to_string(case_path)
        .unwrap_or_else(|error| panic!("failed to read {}: {error}", case_path.display()));
    let fixture: UpstreamCase = serde_json::from_str(&raw)
        .unwrap_or_else(|error| panic!("failed to parse {}: {error}", case_path.display()));
    let source = build_source(case_path, &fixture);
    let output = compile_source_with_options(
        &source,
        case_path,
        ScriptTarget::Bash,
        &CompileOptions::default(),
    )
    .unwrap_or_else(|error| panic!("failed to compile {}: {error}", case_name(case_path)));
    let expected = expected_invocation(case_path, &fixture);
    assert!(
        output.code.contains(&expected),
        "case {} missing invocation {:?}\n\n{}",
        case_name(case_path),
        expected,
        output.code
    );
}

fn build_source(case_path: &Path, fixture: &UpstreamCase) -> String {
    let family = family_name(case_path);
    let call = match family.as_str() {
        "ssh.command" => {
            let hostname = arg_string(&fixture.args, 0);
            let command = arg_string(&fixture.args, 1);
            let mut kwargs = Vec::new();
            push_optional_string_kwarg(&mut kwargs, "user", fixture.kwargs.get("user"));
            push_optional_port_kwarg(&mut kwargs, fixture.kwargs.get("port"));
            format_call("std::ops::ssh::command", vec![hostname, command], kwargs)
        }
        "ssh.keyscan" => {
            let hostname = arg_string(&fixture.args, 0);
            let mut kwargs = Vec::new();
            push_optional_bool_kwarg(&mut kwargs, "force", fixture.kwargs.get("force"));
            push_optional_port_kwarg(&mut kwargs, fixture.kwargs.get("port"));
            format_call("std::ops::ssh::keyscan", vec![hostname], kwargs)
        }
        "ssh.upload" => {
            let hostname = arg_string(&fixture.args, 0);
            let filename = arg_string(&fixture.args, 1);
            let remote_filename = fixture
                .args
                .get(2)
                .map(value_to_fp_literal)
                .unwrap_or_else(|| filename.clone());
            let mut kwargs = Vec::new();
            kwargs.push(format!("remote_filename={remote_filename}"));
            push_optional_string_kwarg(&mut kwargs, "user", fixture.kwargs.get("user"));
            push_optional_port_kwarg(&mut kwargs, fixture.kwargs.get("port"));
            push_optional_bool_kwarg(
                &mut kwargs,
                "use_remote_sudo",
                fixture.kwargs.get("use_remote_sudo"),
            );
            push_optional_bool_kwarg(
                &mut kwargs,
                "ssh_keyscan",
                fixture.kwargs.get("ssh_keyscan"),
            );
            format_call("std::ops::ssh::upload", vec![hostname, filename], kwargs)
        }
        "ssh.download" => {
            let hostname = arg_string(&fixture.args, 0);
            let filename = arg_string(&fixture.args, 1);
            let local_filename = fixture
                .args
                .get(2)
                .map(value_to_fp_literal)
                .unwrap_or_else(|| filename.clone());
            let mut kwargs = Vec::new();
            kwargs.push(format!("local_filename={local_filename}"));
            push_optional_bool_kwarg(&mut kwargs, "force", fixture.kwargs.get("force"));
            push_optional_port_kwarg(&mut kwargs, fixture.kwargs.get("port"));
            push_optional_string_kwarg(&mut kwargs, "user", fixture.kwargs.get("user"));
            push_optional_bool_kwarg(
                &mut kwargs,
                "ssh_keyscan",
                fixture.kwargs.get("ssh_keyscan"),
            );
            format_call("std::ops::ssh::download", vec![hostname, filename], kwargs)
        }
        other => panic!("unsupported upstream family: {other}"),
    };

    format!("const fn main() {{\n    {call};\n}}\n")
}

fn expected_invocation(case_path: &Path, fixture: &UpstreamCase) -> String {
    let family = family_name(case_path);
    match family.as_str() {
        "ssh.command" => format!(
            "__fp_std_ops_ssh_command_ {} {} 'localhost' {} {} 'false'",
            shell_quote(&value_to_string(&fixture.args[0])),
            shell_quote(&value_to_string(&fixture.args[1])),
            shell_quote(fixture_kwarg_string(fixture, "user", "")),
            shell_quote(&fixture_kwarg_port(fixture, "22")),
        ),
        "ssh.keyscan" => format!(
            "__fp_std_ops_ssh_keyscan_ {} 'localhost' {} {} 'false'",
            shell_quote(&value_to_string(&fixture.args[0])),
            shell_quote(fixture_kwarg_bool(fixture, "force", false)),
            shell_quote(&fixture_kwarg_port(fixture, "22")),
        ),
        "ssh.upload" => {
            let remote_filename = fixture
                .args
                .get(2)
                .map(value_to_string)
                .unwrap_or_else(|| value_to_string(&fixture.args[1]));
            format!(
                "__fp_std_ops_ssh_upload_ {} {} 'localhost' {} {} {} {} {} 'false'",
                shell_quote(&value_to_string(&fixture.args[0])),
                shell_quote(&value_to_string(&fixture.args[1])),
                shell_quote(&remote_filename),
                shell_quote(&fixture_kwarg_port(fixture, "22")),
                shell_quote(fixture_kwarg_string(fixture, "user", "")),
                shell_quote(fixture_kwarg_bool(fixture, "use_remote_sudo", false)),
                shell_quote(fixture_kwarg_bool(fixture, "ssh_keyscan", false)),
            )
        }
        "ssh.download" => {
            let local_filename = fixture
                .args
                .get(2)
                .map(value_to_string)
                .unwrap_or_else(|| value_to_string(&fixture.args[1]));
            format!(
                "__fp_std_ops_ssh_download_ {} {} 'localhost' {} {} {} {} {} 'false'",
                shell_quote(&value_to_string(&fixture.args[0])),
                shell_quote(&value_to_string(&fixture.args[1])),
                shell_quote(&local_filename),
                shell_quote(fixture_kwarg_bool(fixture, "force", false)),
                shell_quote(&fixture_kwarg_port(fixture, "22")),
                shell_quote(fixture_kwarg_string(fixture, "user", "")),
                shell_quote(fixture_kwarg_bool(fixture, "ssh_keyscan", false)),
            )
        }
        other => panic!("unsupported upstream family: {other}"),
    }
}

fn format_call(function: &str, args: Vec<String>, kwargs: Vec<String>) -> String {
    let mut parts = args;
    parts.extend(kwargs);
    format!("{function}({})", parts.join(", "))
}

fn arg_string(args: &[Value], index: usize) -> String {
    value_to_fp_literal(
        args.get(index)
            .unwrap_or_else(|| panic!("missing arg {index}")),
    )
}

fn value_to_fp_literal(value: &Value) -> String {
    match value {
        Value::String(text) => {
            serde_json::to_string(text).expect("string literal should serialize")
        }
        Value::Bool(flag) => {
            if *flag {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        Value::Number(number) => number.to_string(),
        other => panic!("unsupported fixture value: {other:?}"),
    }
}

fn value_to_string(value: &Value) -> String {
    match value {
        Value::String(text) => text.clone(),
        Value::Bool(flag) => {
            if *flag {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        Value::Number(number) => number.to_string(),
        other => panic!("unsupported fixture value: {other:?}"),
    }
}

fn push_optional_string_kwarg(kwargs: &mut Vec<String>, name: &str, value: Option<&Value>) {
    if let Some(value) = value {
        kwargs.push(format!("{name}={}", value_to_fp_literal(value)));
    }
}

fn push_optional_port_kwarg(kwargs: &mut Vec<String>, value: Option<&Value>) {
    if let Some(value) = value {
        kwargs.push(format!("port={}", value_to_fp_literal(value)));
    }
}

fn push_optional_bool_kwarg(kwargs: &mut Vec<String>, name: &str, value: Option<&Value>) {
    if let Some(value) = value {
        kwargs.push(format!("{name}={}", value_to_fp_literal(value)));
    }
}

fn fixture_kwarg_string<'a>(fixture: &'a UpstreamCase, name: &str, default: &'a str) -> &'a str {
    fixture
        .kwargs
        .get(name)
        .and_then(Value::as_str)
        .unwrap_or(default)
}

fn fixture_kwarg_port(fixture: &UpstreamCase, default: &str) -> String {
    fixture
        .kwargs
        .get("port")
        .map(value_to_string)
        .unwrap_or_else(|| default.to_string())
}

fn fixture_kwarg_bool(fixture: &UpstreamCase, name: &str, default: bool) -> &'static str {
    match fixture
        .kwargs
        .get(name)
        .and_then(Value::as_bool)
        .unwrap_or(default)
    {
        true => "true",
        false => "false",
    }
}

fn family_name(case_path: &Path) -> String {
    case_path
        .parent()
        .and_then(Path::file_name)
        .and_then(|name| name.to_str())
        .unwrap_or_else(|| panic!("invalid upstream case path: {}", case_path.display()))
        .to_string()
}

fn case_name(case_path: &Path) -> String {
    case_path
        .strip_prefix(
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("tests")
                .join("fixtures")
                .join("pyinfra_upstream")
                .join("operations"),
        )
        .unwrap_or(case_path)
        .display()
        .to_string()
}

fn shell_quote(value: &str) -> String {
    format!("'{}'", value.replace('\'', "'\"'\"'"))
}
