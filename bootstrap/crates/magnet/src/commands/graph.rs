use std::path::{Path, PathBuf};
use std::fs;

use fp_core::ast::{Value, ValueList, ValueMap};
use fp_core::formats::json;

use crate::resolver::project::resolve_graph;

pub fn run(args: &[String]) -> crate::Result<()> {
    let mut path: Option<PathBuf> = None;
    let mut output: Option<PathBuf> = None;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--output" | "-o" => {
                let Some(value) = args.get(i + 1) else {
                    return Err("missing output path".to_string().into());
                };
                output = Some(PathBuf::from(value));
                i += 2;
            }
            value => {
                if path.is_none() {
                    path = Some(PathBuf::from(value));
                    i += 1;
                } else {
                    return Err(format!("unexpected argument: {value}").into());
                }
            }
        }
    }
    let root = path.unwrap_or_else(|| PathBuf::from("."));
    let graph = resolve_graph(&root)?;
    if let Some(output) = output {
        if is_dot(&output) {
            write_dot(&graph, &output)?;
        } else {
            let payload = json::to_string_pretty(&graph_to_value(&graph))?;
            if let Some(parent) = output.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(&output, payload)?;
        }
    } else {
        let payload = json::to_string_pretty(&graph_to_value(&graph))?;
        println!("{payload}");
    }
    Ok(())
}

fn is_dot(path: &Path) -> bool {
    matches!(path.extension().and_then(|ext| ext.to_str()), Some("dot" | "gv"))
}

fn write_dot(graph: &fp_core::package::graph::PackageGraph, output: &Path) -> crate::Result<()> {
    let mut content = String::new();
    content.push_str("digraph magnet {\n");
    for pkg in graph.packages() {
        content.push_str(&format!("  \"{}\";\n", pkg.name));
    }
    for pkg in graph.packages() {
        for dep in &pkg.metadata.dependencies {
            content.push_str(&format!("  \"{}\" -> \"{}\";\n", pkg.name, dep.package));
        }
    }
    content.push_str("}\n");
    if let Some(parent) = output.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(output, content)?;
    Ok(())
}

pub(crate) fn graph_to_value(graph: &fp_core::package::graph::PackageGraph) -> Value {
    let mut packages = Vec::new();
    for package in graph.packages() {
        packages.push(package_to_value(package));
    }
    Value::Map(ValueMap::from_pairs([
        (
            Value::string("packages".to_string()),
            Value::List(ValueList::new(packages)),
        ),
        (
            Value::string("modules".to_string()),
            Value::List(ValueList::new(Vec::new())),
        ),
    ]))
}

fn package_to_value(package: &fp_core::package::PackageDescriptor) -> Value {
    let metadata = metadata_to_value(&package.metadata);
    let modules = Value::List(ValueList::new(
        package
            .modules
            .iter()
            .map(|id| Value::string(id.as_str().to_string()))
            .collect(),
    ));
    Value::Map(ValueMap::from_pairs([
        (
            Value::string("id".to_string()),
            Value::string(package.id.as_str().to_string()),
        ),
        (Value::string("name".to_string()), Value::string(package.name.clone())),
        (
            Value::string("version".to_string()),
            package
                .version
                .as_ref()
                .map(|v| Value::string(v.clone()))
                .unwrap_or_else(Value::null),
        ),
        (
            Value::string("manifest_path".to_string()),
            Value::string(package.manifest_path.to_string()),
        ),
        (
            Value::string("root".to_string()),
            Value::string(package.root.to_string()),
        ),
        (Value::string("metadata".to_string()), metadata),
        (Value::string("modules".to_string()), modules),
    ]))
}

fn metadata_to_value(metadata: &fp_core::package::PackageMetadata) -> Value {
    let authors = Value::List(ValueList::new(
        metadata
            .authors
            .iter()
            .map(|value| Value::string(value.clone()))
            .collect(),
    ));
    let keywords = Value::List(ValueList::new(
        metadata
            .keywords
            .iter()
            .map(|value| Value::string(value.clone()))
            .collect(),
    ));
    let dependencies = Value::List(ValueList::new(
        metadata
            .dependencies
            .iter()
            .map(|dep| Value::string(dep.package.clone()))
            .collect(),
    ));
    let features = Value::Map(ValueMap::new());

    Value::Map(ValueMap::from_pairs([
        (
            Value::string("edition".to_string()),
            metadata
                .edition
                .as_ref()
                .map(|value| Value::string(value.clone()))
                .unwrap_or_else(Value::null),
        ),
        (Value::string("authors".to_string()), authors),
        (
            Value::string("description".to_string()),
            metadata
                .description
                .as_ref()
                .map(|value| Value::string(value.clone()))
                .unwrap_or_else(Value::null),
        ),
        (
            Value::string("license".to_string()),
            metadata
                .license
                .as_ref()
                .map(|value| Value::string(value.clone()))
                .unwrap_or_else(Value::null),
        ),
        (Value::string("keywords".to_string()), keywords),
        (
            Value::string("registry".to_string()),
            metadata
                .registry
                .as_ref()
                .map(|value| Value::string(value.clone()))
                .unwrap_or_else(Value::null),
        ),
        (Value::string("features".to_string()), features),
        (
            Value::string("dependencies".to_string()),
            dependencies,
        ),
    ]))
}
