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

    pub fn null() -> Span {
        Span {
            file: 0,
            lo: 0,
            hi: 0,
        }
    }

    pub fn is_null(self) -> bool {
        self.file == 0 && self.lo == 0 && self.hi == 0
    }

    pub fn union<I>(spans: I) -> Span
    where
        I: IntoIterator<Item = Span>,
    {
        let mut iter = spans.into_iter().filter(|span| !span.is_null());
        let Some(first) = iter.next() else {
            return Span::null();
        };
        let mut lo = first.lo;
        let mut hi = first.hi;
        let file = first.file;
        for span in iter {
            if span.file != file {
                return Span::null();
            }
            lo = lo.min(span.lo);
            hi = hi.max(span.hi);
        }
        Span { file, lo, hi }
    }

    pub fn or(self, other: Span) -> Span {
        if self.is_null() {
            other
        } else {
            self
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Span::null()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileInfo {
    pub file: PathBuf,
}
