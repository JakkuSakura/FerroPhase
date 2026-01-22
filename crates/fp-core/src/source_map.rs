use crate::span::{FileId, Span};
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, RwLock};

#[derive(Clone, Debug)]
pub struct SourceFile {
    pub id: FileId,
    pub path: PathBuf,
    pub source: Arc<str>,
    line_starts: Arc<Vec<usize>>,
}

impl SourceFile {
    pub fn line_col(&self, offset: u32) -> (usize, usize) {
        let offset = offset as usize;
        let idx = match self.line_starts.binary_search(&offset) {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        };
        let line_start = self.line_starts.get(idx).copied().unwrap_or(0);
        let line = idx + 1;
        let col = offset.saturating_sub(line_start) + 1;
        (line, col)
    }

    pub fn line_text(&self, line: usize) -> Option<&str> {
        if line == 0 {
            return None;
        }
        let idx = line - 1;
        let start = *self.line_starts.get(idx)?;
        let end = self
            .line_starts
            .get(idx + 1)
            .copied()
            .unwrap_or_else(|| self.source.len());
        self.source
            .get(start..end)
            .map(|s| s.trim_end_matches('\n'))
    }

    pub fn span_on_line(&self, span: Span) -> Option<LineSpan> {
        let (line, col_start) = self.line_col(span.lo);
        let (_, col_end) = self.line_col(span.hi.max(span.lo + 1));
        let text = self.line_text(line)?.to_string();
        Some(LineSpan {
            line,
            col_start,
            col_end,
            text,
        })
    }

    pub fn offset_for_line_col(&self, line: usize, col: usize) -> Option<u32> {
        if line == 0 || col == 0 {
            return None;
        }
        let start = *self.line_starts.get(line - 1)?;
        let offset = start.saturating_add(col.saturating_sub(1));
        if offset > self.source.len() {
            return None;
        }
        Some(offset as u32)
    }
}

#[derive(Clone, Debug)]
pub struct LineSpan {
    pub line: usize,
    pub col_start: usize,
    pub col_end: usize,
    pub text: String,
}

#[derive(Debug)]
pub struct SourceMap {
    files: RwLock<HashMap<FileId, SourceFile>>,
    paths: RwLock<HashMap<PathBuf, FileId>>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self {
            files: RwLock::new(HashMap::new()),
            paths: RwLock::new(HashMap::new()),
        }
    }

    pub fn register_source(&self, path: PathBuf, source: &str) -> FileId {
        let mut id = GLOBAL_FILE_ID.fetch_add(1, Ordering::Relaxed);
        if id == 0 {
            // Reserve file id 0 for `Span::null()`.
            id = GLOBAL_FILE_ID.fetch_add(1, Ordering::Relaxed);
        }
        let file = SourceFile {
            id,
            path: path.clone(),
            source: Arc::from(source),
            line_starts: Arc::new(compute_line_starts(source)),
        };
        if let Ok(mut files) = self.files.write() {
            files.insert(id, file);
        }
        if let Ok(mut paths) = self.paths.write() {
            paths.insert(path, id);
        }
        id
    }

    pub fn register_source_with_id(&self, id: FileId, path: PathBuf, source: &str) -> FileId {
        let file = SourceFile {
            id,
            path: path.clone(),
            source: Arc::from(source),
            line_starts: Arc::new(compute_line_starts(source)),
        };
        if let Ok(mut files) = self.files.write() {
            files.insert(id, file);
        }
        if let Ok(mut paths) = self.paths.write() {
            paths.insert(path, id);
        }
        id
    }

    pub fn register_path(&self, path: &Path, source: &str) -> FileId {
        self.register_source(path.to_path_buf(), source)
    }

    pub fn file(&self, id: FileId) -> Option<SourceFile> {
        self.files
            .read()
            .ok()
            .and_then(|files| files.get(&id).cloned())
    }

    pub fn file_id(&self, path: &Path) -> Option<FileId> {
        self.paths
            .read()
            .ok()
            .and_then(|paths| paths.get(path).copied())
    }
}

fn compute_line_starts(source: &str) -> Vec<usize> {
    let mut starts = vec![0];
    for (idx, ch) in source.char_indices() {
        if ch == '\n' {
            starts.push(idx + 1);
        }
    }
    starts
}

static GLOBAL_SOURCE_MAP: Lazy<Arc<SourceMap>> = Lazy::new(|| Arc::new(SourceMap::new()));
static GLOBAL_FILE_ID: AtomicU64 = AtomicU64::new(1);

pub fn source_map() -> Arc<SourceMap> {
    GLOBAL_SOURCE_MAP.clone()
}

pub fn register_source(path: &Path, source: &str) -> FileId {
    source_map().register_path(path, source)
}
