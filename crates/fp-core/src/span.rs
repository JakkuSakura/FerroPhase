use std::path::PathBuf;

pub type FileId = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct Span {
    pub file: FileId,
    pub lo: u32,
    pub hi: u32,
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Span({}:{}-{})", self.file, self.lo, self.hi)
    }
}

impl Span {
    pub fn new(file: FileId, lo: u32, hi: u32) -> Span {
        Span { file, lo, hi }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileInfo {
    pub file: PathBuf,
}
