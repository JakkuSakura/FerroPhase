use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum LanguageKind {
    Magnet,
    Rust,
    Python,
    #[serde(rename = "javascript")]
    JavaScript,
    #[serde(rename = "typescript")]
    TypeScript,
    Go,
}

impl LanguageKind {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Magnet => "magnet",
            Self::Rust => "rust",
            Self::Python => "python",
            Self::JavaScript => "javascript",
            Self::TypeScript => "typescript",
            Self::Go => "go",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum ManifestKind {
    Nexus,
    Workspace,
    Package,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct IdentityCandidate {
    pub language: LanguageKind,
    pub manifest_kind: ManifestKind,
    pub name: String,
    pub version: Option<String>,
    pub root_path: PathBuf,
    pub manifest_path: PathBuf,
    pub source_roots: Vec<PathBuf>,
}

impl IdentityCandidate {
    pub fn contains_path(&self, path: &Path) -> bool {
        path.starts_with(&self.root_path)
    }

    pub fn depth(&self) -> usize {
        self.root_path.components().count()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct ProjectIdentity {
    pub input_path: PathBuf,
    pub candidates: Vec<IdentityCandidate>,
}

impl ProjectIdentity {
    pub fn primary_package(&self) -> Option<&IdentityCandidate> {
        self.candidates
            .iter()
            .filter(|candidate| candidate.manifest_kind == ManifestKind::Package)
            .max_by_key(|candidate| candidate.depth())
    }

    pub fn primary(&self) -> Option<&IdentityCandidate> {
        self.primary_package().or_else(|| {
            self.candidates
                .iter()
                .max_by_key(|candidate| candidate.depth())
        })
    }

    pub fn packages(&self) -> impl Iterator<Item = &IdentityCandidate> {
        self.candidates
            .iter()
            .filter(|candidate| candidate.manifest_kind == ManifestKind::Package)
    }
}
