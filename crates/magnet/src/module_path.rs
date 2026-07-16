use crate::models::{IdentityCandidate, LanguageKind, PackageModel, ProjectIdentity};
use crate::resolver::identity;
use eyre::{Result, eyre};
use std::path::Path;

pub fn resolve_project_module_path(identity: &ProjectIdentity, path: &Path) -> Result<Vec<String>> {
    let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let candidate = identity
        .packages()
        .find(|candidate| candidate.contains_path(&path))
        .or_else(|| identity.primary().filter(|candidate| candidate.contains_path(&path)))
        .ok_or_else(|| eyre!("No identity candidate found for {}", path.display()))?;
    resolve_candidate_module_path(candidate, &path)
}

pub fn resolve_candidate_module_path(
    candidate: &IdentityCandidate,
    path: &Path,
) -> Result<Vec<String>> {
    let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let package = identity::resolve_package(candidate)?;
    Ok(dispatch_module_path(candidate.language, &package, &path))
}

fn dispatch_module_path(language: LanguageKind, package: &PackageModel, path: &Path) -> Vec<String> {
    match language {
        LanguageKind::Magnet | LanguageKind::Rust => {
            fp_lang::module_path::estimate_module_path(&package.root_path, path)
        }
        LanguageKind::Python => fp_python::estimate_module_path(&package.root_path, path),
        LanguageKind::JavaScript | LanguageKind::TypeScript => {
            fp_typescript::estimate_module_path(&package.root_path, path)
        }
        LanguageKind::Go => fp_golang::estimate_module_path(&package.root_path, path),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn package(root: &str) -> PackageModel {
        PackageModel {
            root_path: PathBuf::from(root),
            ..PackageModel::default()
        }
    }

    #[test]
    fn dispatches_rust_module_path_to_fp_lang() {
        let temp = tempfile::tempdir().expect("tempdir");
        std::fs::create_dir_all(temp.path().join("src/graph")).expect("src dir");
        std::fs::write(temp.path().join("src/graph/view.rs"), "fn view() {}\n").expect("write");
        assert_eq!(
            dispatch_module_path(
                LanguageKind::Rust,
                &package(temp.path().to_str().expect("path")),
                &temp.path().join("src/graph/view.rs"),
            ),
            vec!["graph".to_string(), "view".to_string()]
        );
    }

    #[test]
    fn dispatches_python_module_path_to_fp_python() {
        assert_eq!(
            dispatch_module_path(
                LanguageKind::Python,
                &package("/proj"),
                Path::new("/proj/pkg/__init__.py"),
            ),
            vec!["pkg".to_string()]
        );
    }
}
