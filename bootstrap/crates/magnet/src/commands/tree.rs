use std::path::PathBuf;

use crate::models::PackageGraphSummary;
use crate::resolver::project::resolve_graph;

pub fn run(args: &[String]) -> crate::Result<()> {
    let mut path: Option<PathBuf> = None;
    if let Some(first) = args.first() {
        if !first.starts_with('-') {
            path = Some(PathBuf::from(first));
        }
    }
    let root = path.unwrap_or_else(|| PathBuf::from("."));
    let graph = resolve_graph(&root)?;
    let summary = PackageGraphSummary::from_graph(&graph);
    for (pkg, deps) in summary.packages {
        println!("{pkg}");
        for dep in deps {
            println!("  - {dep}");
        }
    }
    Ok(())
}
