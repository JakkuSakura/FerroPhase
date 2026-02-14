use eyre::Result;
use magnet::utils::diff_path;
use std::path::Path;

#[test]
fn diff_path_errors_on_mismatched_path_kinds() -> Result<()> {
    let root = Path::new("relative-root");
    let path = Path::new("/tmp/absolute-target");

    let err = diff_path(root, path).unwrap_err();
    let message = err.to_string();

    assert!(
        message.contains("Failed to compute relative path"),
        "unexpected error message: {message}"
    );
    assert!(
        message.contains(&root.display().to_string()),
        "missing root context: {message}"
    );
    assert!(
        message.contains(&path.display().to_string()),
        "missing path context: {message}"
    );

    Ok(())
}
