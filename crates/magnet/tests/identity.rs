use magnet::MagnetCli;
use std::fs;

#[test]
fn resolves_nested_rust_package_identity() -> eyre::Result<()> {
    let temp = tempfile::tempdir()?;
    let root = temp.path();
    fs::create_dir_all(root.join("crates").join("skln-cli").join("src"))?;
    fs::write(
        root.join("Cargo.toml"),
        r#"[workspace]
members = ["crates/*"]
"#,
    )?;
    fs::write(
        root.join("crates").join("skln-cli").join("Cargo.toml"),
        r#"[package]
name = "skln-cli"
version = "0.1.0"
edition = "2024"
"#,
    )?;

    let cli = MagnetCli::new();
    let identity = cli.resolve_identity(&root.join("crates").join("skln-cli").join("src"))?;
    let package = identity
        .primary_package()
        .expect("missing package candidate");
    assert_eq!(package.name, "skln-cli");
    assert_eq!(
        package.root_path,
        root.join("crates").join("skln-cli").canonicalize()?
    );
    Ok(())
}
