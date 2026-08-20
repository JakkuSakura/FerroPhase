use std::cell::Cell;
use std::path::PathBuf;

pub type FileId = u64;

thread_local! {
    /// The `FileId` currently being parsed on this thread, if any. Some
    /// low-level span-construction call sites (e.g. `fp-lang`'s
    /// `token_span_to_span`) only have a bare token/byte range in hand, not
    /// the `FileId` of the file that token came from — those sites read this
    /// instead of hardcoding a placeholder (`0`, i.e. [`FileId`]'s "no file"
    /// value), so a span they build still resolves back to real source text
    /// via [`Span::snippet`]. A parser sets this once per file
    /// (`set_current_parse_file`) before it starts producing tokens/spans for
    /// that file, rather than threading a `file: FileId` parameter through
    /// every low-level parsing function that can construct a `Span` — this
    /// crate has dozens of those call sites, and none of them run
    /// concurrently with a different file's parse on the same thread.
    static CURRENT_PARSE_FILE: Cell<FileId> = const { Cell::new(0) };
}

/// Set the file whose tokens/spans are about to be produced on this thread.
/// See [`CURRENT_PARSE_FILE`]'s doc comment for why this exists instead of
/// threading a `file: FileId` parameter everywhere a `Span` is built.
pub fn set_current_parse_file(file: FileId) {
    CURRENT_PARSE_FILE.with(|cell| cell.set(file));
}

/// The file set by the most recent [`set_current_parse_file`] call on this
/// thread, or `0` (no file) if none has been set.
pub fn current_parse_file() -> FileId {
    CURRENT_PARSE_FILE.with(|cell| cell.get())
}

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

    /// The literal source text this span covers, if `file` is registered in
    /// the global source map. Used to turn a bare `[span file:lo-hi]` byte
    /// range in an error message into the actual offending text, since a raw
    /// byte range alone (e.g. `[span 0:30-33]`) isn't enough to tell what
    /// went wrong without manually slicing the source file by hand.
    pub fn snippet(self) -> Option<String> {
        let file = crate::source_map::source_map().file(self.file)?;
        file.source
            .get(self.lo as usize..self.hi as usize)
            .map(|s| s.to_string())
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
        if self.is_null() { other } else { self }
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
