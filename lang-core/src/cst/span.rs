use std::path::PathBuf;

pub type SpanId = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub id: SpanId,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileLocation {
    pub line: u32,
    pub column: u32,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SpanData {
    pub file: PathBuf,
    pub begin: FileLocation,
    pub end: FileLocation,
}
