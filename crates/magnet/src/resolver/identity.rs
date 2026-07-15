use crate::models::{IdentityCandidate, ManifestKind, PackageModel, ProjectIdentity};
use crate::resolver::lang;
use eyre::Result;
use std::collections::HashSet;
use std::path::{Path, PathBuf};

pub fn resolve_identity(path: &Path) -> Result<ProjectIdentity> {
    let input_path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let start_dir = if input_path.is_file() {
        input_path.parent().unwrap_or(Path::new(".")).to_path_buf()
    } else {
        input_path.clone()
    };

    let mut candidates = Vec::new();
    let mut seen = HashSet::new();
    for ancestor in start_dir.ancestors() {
        for candidate in lang::discover(ancestor)? {
            let key = (
                candidate.language,
                candidate.manifest_kind,
                candidate.root_path.clone(),
                candidate.manifest_path.clone(),
            );
            if seen.insert(key) {
                candidates.push(candidate);
            }
        }
    }
    candidates.sort_by(|left, right| {
        right
            .depth()
            .cmp(&left.depth())
            .then_with(|| {
                manifest_rank(right.manifest_kind).cmp(&manifest_rank(left.manifest_kind))
            })
            .then_with(|| left.name.cmp(&right.name))
    });

    Ok(ProjectIdentity {
        input_path,
        candidates,
    })
}

pub fn resolve_packages(identity: &ProjectIdentity) -> Result<Vec<PackageModel>> {
    let mut packages = Vec::new();
    let mut seen = HashSet::<PathBuf>::new();
    for candidate in identity.packages() {
        if seen.insert(candidate.manifest_path.clone()) {
            packages.push(lang::package_from_candidate(candidate)?);
        }
    }
    Ok(packages)
}

pub fn resolve_package(candidate: &IdentityCandidate) -> Result<PackageModel> {
    lang::package_from_candidate(candidate)
}

fn manifest_rank(kind: ManifestKind) -> u8 {
    match kind {
        ManifestKind::Package => 3,
        ManifestKind::Workspace => 2,
        ManifestKind::Nexus => 1,
    }
}
