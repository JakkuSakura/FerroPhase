use std::path::PathBuf;

use fp_core::ast::{Value, ValueList, ValueMap};

use crate::resolver::project::resolve_graph;
use crate::utils::write_toml_file;

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
    let lock_value = lock_from_graph(&graph);
    let output = output.unwrap_or_else(|| root.join("Magnet.lock"));
    write_toml_file(&output, &lock_value)
}

fn lock_from_graph(graph: &fp_core::package::graph::PackageGraph) -> Value {
    let mut packages = Vec::new();
    for package in graph.packages() {
        let deps = Value::List(ValueList::new(
            package
                .metadata
                .dependencies
                .iter()
                .map(|dep| Value::string(dep.package.clone()))
                .collect(),
        ));
        let entry = Value::Map(ValueMap::from_pairs([
            (
                Value::string("name".to_string()),
                Value::string(package.name.clone()),
            ),
            (
                Value::string("version".to_string()),
                package
                    .version
                    .as_ref()
                    .map(|v| Value::string(v.clone()))
                    .unwrap_or_else(Value::null),
            ),
            (Value::string("dependencies".to_string()), deps),
        ]));
        packages.push(entry);
    }
    Value::Map(ValueMap::from_pairs([(
        Value::string("package".to_string()),
        Value::List(ValueList::new(packages)),
    )]))
}
