//! `StyledWriter`: a small line-oriented buffer for backends that
//! generate source text (Kotlin, Go, Python, ...). It owns indentation
//! bookkeeping and brace pairing so individual backends don't each
//! reimplement `indent: usize` + manual `"    ".repeat(n)` calls.

use std::cell::RefCell;
use std::fmt;
use std::fs;
use std::io::Write as _;
use std::path::Path;
use std::process::{Command, Stdio};
use std::rc::Rc;
use std::sync::Arc;

/// How one indentation level is rendered.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IndentStyle {
    Spaces(usize),
    Tabs,
}

impl IndentStyle {
    fn unit(&self) -> &'static str {
        match self {
            IndentStyle::Spaces(_) => " ",
            IndentStyle::Tabs => "\t",
        }
    }

    fn width(&self) -> usize {
        match self {
            IndentStyle::Spaces(n) => *n,
            IndentStyle::Tabs => 1,
        }
    }
}

/// A standalone indent-depth tracker: how deep the current nesting level is,
/// and how to render that depth as a literal prefix string. [`StyledWriter`]
/// is built on top of one of these rather than reimplementing depth tracking
/// itself — and it's just as usable on its own, independent of any output
/// buffer, whenever code needs to build a self-consistent, *relatively*
/// indented snippet (e.g. as a plain `String`, to be embedded later at
/// whatever real depth it ends up at) without hardcoding literal
/// `"    "`/`"        "` constants for each nesting level by hand.
#[derive(Clone, Debug)]
pub struct Indent {
    style: IndentStyle,
    depth: usize,
}

impl Indent {
    pub fn new(style: IndentStyle) -> Self {
        Self { style, depth: 0 }
    }

    pub fn depth(&self) -> usize {
        self.depth
    }

    pub fn increase(&mut self) -> &mut Self {
        self.depth += 1;
        self
    }

    pub fn decrease(&mut self) -> &mut Self {
        self.depth = self.depth.saturating_sub(1);
        self
    }

    /// The literal prefix string for the current depth.
    pub fn prefix(&self) -> String {
        self.prefix_at(self.depth)
    }

    /// The literal prefix string for an arbitrary depth (e.g. the current
    /// depth plus some extra levels, for a wrapped line's continuation).
    pub fn prefix_at(&self, depth: usize) -> String {
        self.style.unit().repeat(self.style.width() * depth)
    }
}

/// Where the opening brace of a `block(...)` lands relative to its header.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BraceStyle {
    /// `header {` — Rust/Kotlin/Go/C-style.
    SameLine,
    /// `header` then `{` on its own line — Allman-style.
    NextLine,
}

/// Post-processes the fully-rendered buffer before it's returned/written.
/// Kept pluggable rather than hardcoding a formatter binary per language.
#[derive(Clone)]
pub enum Formatter {
    /// Pipe the buffer through an external formatter's stdin and read its
    /// stdout back (e.g. `rustfmt`, `gofmt`).
    Command { program: String, args: Vec<String> },
    /// Arbitrary in-process formatting function.
    Function(Arc<dyn Fn(&str) -> eyre::Result<String> + Send + Sync>),
}

impl fmt::Debug for Formatter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Formatter::Command { program, args } => f
                .debug_struct("Command")
                .field("program", program)
                .field("args", args)
                .finish(),
            Formatter::Function(_) => f.write_str("Function(..)"),
        }
    }
}

impl Formatter {
    pub fn command(
        program: impl Into<String>,
        args: impl IntoIterator<Item = impl Into<String>>,
    ) -> Self {
        Formatter::Command {
            program: program.into(),
            args: args.into_iter().map(Into::into).collect(),
        }
    }

    pub fn function<F>(f: F) -> Self
    where
        F: Fn(&str) -> eyre::Result<String> + Send + Sync + 'static,
    {
        Formatter::Function(Arc::new(f))
    }

    fn apply(&self, source: &str) -> eyre::Result<String> {
        match self {
            Formatter::Function(f) => f(source),
            Formatter::Command { program, args } => {
                let mut child = Command::new(program)
                    .args(args)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .stderr(Stdio::piped())
                    .spawn()?;
                child
                    .stdin
                    .take()
                    .expect("piped stdin")
                    .write_all(source.as_bytes())?;
                let output = child.wait_with_output()?;
                if !output.status.success() {
                    eyre::bail!(
                        "formatter `{program}` exited with {}: {}",
                        output.status,
                        String::from_utf8_lossy(&output.stderr)
                    );
                }
                Ok(String::from_utf8(output.stdout)?)
            }
        }
    }
}

/// Spacing/wrapping knobs for [`IndentedBuffer`] alone — the subset of
/// [`WriterConfig`] relevant to plain indented-line buffering, without any
/// of [`StyledWriter`]'s brace-pairing/formatting concerns on top.
#[derive(Clone, Debug)]
pub struct BufferConfig {
    pub indent_style: IndentStyle,
    /// Maximum line width, indentation included. `None` (the default)
    /// disables wrapping. Lines are only ever split on ASCII space
    /// boundaries — an unbreakable token wider than `max_width` on its own
    /// is left as-is rather than cut mid-token.
    pub max_width: Option<usize>,
    /// Extra indent levels applied to a wrapped line's continuation lines,
    /// on top of the depth the line itself was written at.
    pub continuation_indent: usize,
}

impl Default for BufferConfig {
    fn default() -> Self {
        Self {
            indent_style: IndentStyle::Spaces(4),
            max_width: None,
            continuation_indent: 1,
        }
    }
}

/// Indented line buffer: owns the raw text accumulator, indent-depth
/// tracking (via [`Indent`]), and optional max-width wrapping. This is the
/// layer [`StyledWriter`] is built on top of (adding brace-pairing and
/// formatting) — but it's a complete, useful primitive on its own wherever
/// code needs to build a self-consistent, depth-tracked multi-line snippet
/// (e.g. as a plain `String`, to be embedded later at whatever real depth it
/// ends up at) without pulling in brace/formatter machinery, or hand-rolling
/// `"    "`/`"        "` literals for each nesting level.
pub struct IndentedBuffer {
    config: BufferConfig,
    indent: Indent,
    out: String,
    /// Content of the line currently being built, without its indent prefix.
    pending: String,
}

impl IndentedBuffer {
    pub fn new(config: BufferConfig) -> Self {
        let indent = Indent::new(config.indent_style.clone());
        Self {
            config,
            indent,
            out: String::new(),
            pending: String::new(),
        }
    }

    pub fn indent_depth(&self) -> usize {
        self.indent.depth()
    }

    fn indent_prefix(&self) -> String {
        self.indent.prefix()
    }

    fn continuation_prefix(&self) -> String {
        self.indent
            .prefix_at(self.indent.depth() + self.config.continuation_indent)
    }

    /// Append raw text to the current line with no spacing logic applied.
    pub fn raw(&mut self, text: impl AsRef<str>) -> &mut Self {
        self.pending.push_str(text.as_ref());
        self
    }

    /// Append a token to the current line, separated from any prior content
    /// on that line by exactly one space.
    pub fn atom(&mut self, text: impl AsRef<str>) -> &mut Self {
        let text = text.as_ref();
        if text.is_empty() {
            return self;
        }
        if !self.pending.is_empty() {
            self.pending.push(' ');
        }
        self.pending.push_str(text);
        self
    }

    /// Greedily split `self.pending` into `max_width`-wide (indentation
    /// included) chunks on space boundaries. Returns the line as-is if it
    /// already fits, or if there's no `max_width` configured.
    fn wrap_pending(&self) -> Vec<String> {
        let Some(max_width) = self.config.max_width else {
            return vec![self.pending.clone()];
        };
        if self.indent_prefix().len() + self.pending.len() <= max_width {
            return vec![self.pending.clone()];
        }

        let cont_width = self.continuation_prefix().len();
        let mut lines = Vec::new();
        let mut current = String::new();
        let mut budget = max_width.saturating_sub(self.indent_prefix().len());
        for word in self.pending.split(' ') {
            let needed = if current.is_empty() {
                word.len()
            } else {
                current.len() + 1 + word.len()
            };
            if !current.is_empty() && needed > budget {
                lines.push(std::mem::take(&mut current));
                budget = max_width.saturating_sub(cont_width);
                current.push_str(word);
            } else {
                if !current.is_empty() {
                    current.push(' ');
                }
                current.push_str(word);
            }
        }
        lines.push(current);
        lines
    }

    /// Flush the current line (indented if non-empty, blank otherwise) and
    /// start a new one. Calling this with nothing pending emits a blank line.
    /// If `max_width` is configured and the line doesn't fit, it's split
    /// across multiple lines on space boundaries, with continuation lines
    /// indented per `continuation_indent`.
    pub fn newline(&mut self) -> &mut Self {
        if self.pending.is_empty() {
            self.out.push('\n');
            return self;
        }
        for (i, line) in self.wrap_pending().into_iter().enumerate() {
            let prefix = if i == 0 {
                self.indent_prefix()
            } else {
                self.continuation_prefix()
            };
            self.out.push_str(&prefix);
            self.out.push_str(&line);
            self.out.push('\n');
        }
        self.pending.clear();
        self
    }

    /// Write a complete, self-contained line.
    pub fn write_line(&mut self, line: impl AsRef<str>) -> &mut Self {
        self.raw(line);
        self.newline()
    }

    /// Write `text`, applying the current indent prefix to *every* line it
    /// contains (splitting on `\n`) rather than only the first, unlike
    /// [`Self::write_line`]. Each line may still carry its own additional
    /// baked-in *relative* indentation (e.g. from a snippet rendered
    /// standalone and now being embedded at whatever depth this call
    /// happens to sit at) — that stacks additively with the real prefix.
    /// Without this, embedding a pre-rendered multi-line expression via
    /// `write_line` leaves every line after the first with no real indent
    /// at all, since `write_line` indents the string once, up front, not
    /// per embedded newline.
    pub fn write_lines(&mut self, text: impl AsRef<str>) -> &mut Self {
        for line in text.as_ref().split('\n') {
            self.write_line(line);
        }
        self
    }

    /// Append pre-rendered text verbatim, ignoring current indentation.
    /// Useful for splicing output built by an independent renderer (e.g. a
    /// side-channel `IndentedBuffer`, or a hand-built literal block)
    /// directly into this buffer. Flushes any pending unterminated line
    /// first, and ensures the appended block ends with exactly one newline.
    pub fn write_verbatim(&mut self, text: &str) -> &mut Self {
        if !self.pending.is_empty() {
            self.newline();
        }
        self.out.push_str(text.trim_end_matches('\n'));
        self.out.push('\n');
        self
    }

    /// Ensure the buffer ends with exactly one blank line before whatever's
    /// written next — without adding one to an otherwise-empty buffer.
    /// Handy as a declaration separator (struct/function/etc.) that doesn't
    /// pile up blank lines when called repeatedly. Flushes any pending
    /// unterminated line first.
    pub fn ensure_blank_line(&mut self) -> &mut Self {
        if !self.pending.is_empty() {
            self.newline();
        }
        if self.out.is_empty() {
            return self;
        }
        if !self.out.ends_with('\n') {
            self.out.push('\n');
        }
        if !self.out.ends_with("\n\n") {
            self.out.push('\n');
        }
        self
    }

    /// Increase the indent depth by one level. Low-level primitive for
    /// callers that need to interleave indentation with logic that isn't
    /// expressible as a closure over just `&mut Self` (e.g. an emitter
    /// struct with its own state) — prefer [`Self::indented`] when a
    /// closure works.
    pub fn increase_indent(&mut self) -> &mut Self {
        self.indent.increase();
        self
    }

    /// Decrease the indent depth by one level (saturating at zero).
    pub fn decrease_indent(&mut self) -> &mut Self {
        self.indent.decrease();
        self
    }

    /// Run `body` with the indent depth increased by one level. Does not
    /// emit any delimiters — see [`StyledWriter::block`] for brace pairs.
    pub fn indented<F, T, E>(&mut self, body: F) -> Result<T, E>
    where
        F: FnOnce(&mut Self) -> Result<T, E>,
    {
        self.increase_indent();
        let result = body(self);
        self.decrease_indent();
        result
    }

    /// Swap out the raw buffered output built so far for `replacement`,
    /// returning whatever was previously buffered. Indent depth and config
    /// are untouched. Useful for redirecting a nested render into a scratch
    /// buffer — e.g. rendering a statement as a standalone string for
    /// embedding inline elsewhere — without losing this buffer's place
    /// (indent depth) in the overall output. Pair two calls to swap out and
    /// back in: `let saved = b.swap_buffer(String::new()); /* render into b
    /// */ let scratch = b.swap_buffer(saved);`.
    pub fn swap_buffer(&mut self, replacement: String) -> String {
        debug_assert!(
            self.pending.is_empty(),
            "swap_buffer() called with an unterminated line pending"
        );
        std::mem::replace(&mut self.out, replacement)
    }

    /// The raw buffered contents, as-is — no formatter pass, no
    /// trailing-newline normalization (that's [`StyledWriter::finish`]'s
    /// job, one layer up).
    pub fn raw_contents(&self) -> &str {
        &self.out
    }

    fn has_pending(&self) -> bool {
        !self.pending.is_empty()
    }
}

impl fmt::Write for IndentedBuffer {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let mut parts = s.split('\n');
        if let Some(first) = parts.next() {
            self.pending.push_str(first);
        }
        for part in parts {
            self.newline();
            self.pending.push_str(part);
        }
        Ok(())
    }
}

/// Spacing/indentation knobs for [`StyledWriter`].
#[derive(Clone, Debug)]
pub struct WriterConfig {
    pub indent_style: IndentStyle,
    pub brace_style: BraceStyle,
    /// Ensure the emitted text ends with exactly one trailing newline.
    pub trailing_newline: bool,
    /// Run when `finish`/`write_to_file` is called. If it errors (e.g. the
    /// formatter binary isn't installed), the writer falls back to the
    /// unformatted buffer rather than failing the whole generation — running
    /// a formatter is a nicety, not a correctness requirement.
    pub formatter: Option<Formatter>,
    /// Maximum line width, indentation included. `None` (the default)
    /// disables wrapping. Lines are only ever split on ASCII space
    /// boundaries — an unbreakable token wider than `max_width` on its own
    /// is left as-is rather than cut mid-token.
    pub max_width: Option<usize>,
    /// Extra indent levels applied to a wrapped line's continuation lines,
    /// on top of the depth the line itself was written at.
    pub continuation_indent: usize,
}

impl Default for WriterConfig {
    fn default() -> Self {
        Self {
            indent_style: IndentStyle::Spaces(4),
            brace_style: BraceStyle::SameLine,
            trailing_newline: true,
            formatter: None,
            max_width: None,
            continuation_indent: 1,
        }
    }
}

impl WriterConfig {
    fn buffer_config(&self) -> BufferConfig {
        BufferConfig {
            indent_style: self.indent_style.clone(),
            max_width: self.max_width,
            continuation_indent: self.continuation_indent,
        }
    }
}

/// Line-oriented text buffer with automatic indentation, brace pairing, and
/// optional post-hoc formatting. Intended for backends that render an AST/HIR
/// into textual source. Layers brace-pairing/formatting on top of an
/// [`IndentedBuffer`], which owns the actual indent tracking and line buffer.
///
/// Three ways to add content, from lowest to highest level:
/// - [`StyledWriter::atom`] appends a token to the current line,
///   inserting exactly one space of separation from whatever's already on it.
/// - [`StyledWriter::write_line`] appends a complete line and terminates it.
/// - [`StyledWriter::block`] writes a header, opens a brace pair, indents
///   a closure's output, then closes the brace pair.
///
/// It also implements [`std::fmt::Write`], so existing call sites built
/// around `write!`/`writeln!` into a `String` buffer can switch to a
/// `StyledWriter` with no other changes.
///
/// When [`WriterConfig::max_width`] is set, a line that doesn't fit
/// (indentation included) is wrapped across multiple lines on space
/// boundaries, with continuation lines indented per
/// [`WriterConfig::continuation_indent`].
///
/// The buffer is `Rc<RefCell<..>>`-backed and `Clone` is cheap — a clone
/// shares the same underlying output/indent state as the original, it is
/// not an independent copy. This is what lets `.block()` hand its closure
/// body a plain `&Self` rather than a `&mut Self`: pass a clone of the
/// writer into code that also needs `&mut self` on some *other* struct
/// holding the writer (e.g. an emitter), and the two no longer alias from
/// the borrow checker's point of view, even though they share one real
/// buffer underneath. Prefer [`Self::block`] over pairing
/// [`Self::increase_indent`]/[`Self::decrease_indent`] by hand wherever a
/// header/brace-pair applies — it can't be left unbalanced by an early
/// return, and needs no explicit closer.
#[derive(Clone)]
pub struct StyledWriter {
    buffer: Rc<RefCell<IndentedBuffer>>,
    brace_style: BraceStyle,
    trailing_newline: bool,
    formatter: Option<Formatter>,
}

impl StyledWriter {
    pub fn new(config: WriterConfig) -> Self {
        Self {
            buffer: Rc::new(RefCell::new(IndentedBuffer::new(config.buffer_config()))),
            brace_style: config.brace_style,
            trailing_newline: config.trailing_newline,
            formatter: config.formatter,
        }
    }

    pub fn indent_depth(&self) -> usize {
        self.buffer.borrow().indent_depth()
    }

    /// Append raw text to the current line with no spacing logic applied.
    pub fn raw(&self, text: impl AsRef<str>) -> &Self {
        self.buffer.borrow_mut().raw(text);
        self
    }

    /// Append a token to the current line, separated from any prior content
    /// on that line by exactly one space.
    pub fn atom(&self, text: impl AsRef<str>) -> &Self {
        self.buffer.borrow_mut().atom(text);
        self
    }

    /// Flush the current line (indented if non-empty, blank otherwise) and
    /// start a new one. Calling this with nothing pending emits a blank line.
    /// If `max_width` is configured and the line doesn't fit, it's split
    /// across multiple lines on space boundaries, with continuation lines
    /// indented per `continuation_indent`.
    pub fn newline(&self) -> &Self {
        self.buffer.borrow_mut().newline();
        self
    }

    /// Write a complete, self-contained line.
    pub fn write_line(&self, line: impl AsRef<str>) -> &Self {
        self.buffer.borrow_mut().write_line(line);
        self
    }

    /// Write `text`, applying the current indent prefix to *every* line it
    /// contains (splitting on `\n`) rather than only the first, unlike
    /// [`Self::write_line`]. Each line may still carry its own additional
    /// baked-in *relative* indentation (e.g. from a snippet rendered
    /// standalone and now being embedded at whatever depth this call
    /// happens to sit at) — that stacks additively with the real prefix.
    /// Without this, embedding a pre-rendered multi-line expression via
    /// `write_line` leaves every line after the first with no real indent
    /// at all, since `write_line` indents the string once, up front, not
    /// per embedded newline.
    pub fn write_lines(&self, text: impl AsRef<str>) -> &Self {
        self.buffer.borrow_mut().write_lines(text);
        self
    }

    /// Append pre-rendered text verbatim, ignoring current indentation.
    /// Useful for splicing output built by an independent renderer (e.g. a
    /// side-channel `StyledWriter`/`IndentedBuffer`, or a hand-built
    /// literal block) directly into this buffer. Flushes any pending
    /// unterminated line first, and ensures the appended block ends with
    /// exactly one newline.
    pub fn write_verbatim(&self, text: &str) -> &Self {
        self.buffer.borrow_mut().write_verbatim(text);
        self
    }

    /// Ensure the buffer ends with exactly one blank line before whatever's
    /// written next — without adding one to an otherwise-empty buffer.
    /// Handy as a declaration separator (struct/function/etc.) that doesn't
    /// pile up blank lines when called repeatedly. Flushes any pending
    /// unterminated line first.
    pub fn ensure_blank_line(&self) -> &Self {
        self.buffer.borrow_mut().ensure_blank_line();
        self
    }

    /// Increase the indent depth by one level. Low-level primitive for
    /// callers that need to interleave indentation with logic that isn't
    /// expressible as a closure (e.g. across several separate statements in
    /// an emitter method) — prefer [`Self::indented`] or [`Self::block`]
    /// when a closure works.
    pub fn increase_indent(&self) -> &Self {
        self.buffer.borrow_mut().increase_indent();
        self
    }

    /// Decrease the indent depth by one level (saturating at zero).
    pub fn decrease_indent(&self) -> &Self {
        self.buffer.borrow_mut().decrease_indent();
        self
    }

    /// Run `body` with the indent depth increased by one level. Does not
    /// emit any delimiters — use [`StyledWriter::block`] for brace pairs.
    pub fn indented<F, T, E>(&self, body: F) -> Result<T, E>
    where
        F: FnOnce(&Self) -> Result<T, E>,
    {
        self.increase_indent();
        let result = body(self);
        self.decrease_indent();
        result
    }

    /// Write `header`, open a brace pair, run `body` at one deeper indent
    /// level, then close the brace pair. `header` may be empty for a bare
    /// `{ ... }` block.
    ///
    /// ```
    /// # use fp_core::writer::{StyledWriter, WriterConfig};
    /// let w = StyledWriter::new(WriterConfig::default());
    /// w.block("fn main()", |w| -> Result<(), ()> {
    ///     w.write_line("println(\"hi\")");
    ///     Ok(())
    /// }).unwrap();
    /// assert_eq!(w.finish(), "fn main() {\n    println(\"hi\")\n}\n");
    /// ```
    pub fn block<F, E>(&self, header: impl AsRef<str>, body: F) -> Result<(), E>
    where
        F: FnOnce(&Self) -> Result<(), E>,
    {
        let header = header.as_ref();
        match self.brace_style {
            BraceStyle::SameLine if header.is_empty() => self.write_line("{"),
            BraceStyle::SameLine => self.write_line(format!("{header} {{")),
            BraceStyle::NextLine => {
                if !header.is_empty() {
                    self.write_line(header);
                }
                self.write_line("{")
            }
        };
        let result = self.indented(body);
        self.write_line("}");
        result
    }

    /// Swap out the raw buffered output built so far for `replacement`,
    /// returning whatever was previously buffered. Indent depth and config
    /// are untouched. Useful for redirecting a nested render into a scratch
    /// buffer — e.g. rendering a statement as a standalone string for
    /// embedding inline elsewhere — without losing this writer's place
    /// (indent depth) in the overall output. Pair two calls to swap out and
    /// back in: `let saved = w.swap_buffer(String::new()); /* render into w
    /// */ let scratch = w.swap_buffer(saved);`.
    ///
    /// Note this mutates the *shared* buffer — every clone of this writer
    /// observes the swap, not just this handle.
    pub fn swap_buffer(&self, replacement: String) -> String {
        self.buffer.borrow_mut().swap_buffer(replacement)
    }

    /// The raw buffered contents so far, as-is — no formatter pass, no
    /// trailing-newline normalization (that's [`Self::finish`]'s job).
    /// Returns an owned `String` (rather than `&str`) since the buffer sits
    /// behind a `RefCell` now that this writer is cloneable/shared.
    pub fn raw_contents(&self) -> String {
        self.buffer.borrow().raw_contents().to_string()
    }

    /// Apply the configured formatter (if any) and return the final text.
    /// Formatter failures are swallowed (the unformatted buffer is returned
    /// instead) since formatting is a nicety, not a correctness requirement.
    pub fn finish(&self) -> String {
        let buffer = self.buffer.borrow();
        debug_assert!(
            !buffer.has_pending(),
            "finish() called with an unterminated line pending"
        );
        let mut text = buffer.raw_contents().to_string();
        drop(buffer);
        if let Some(formatter) = &self.formatter {
            match formatter.apply(&text) {
                Ok(formatted) => text = formatted,
                Err(err) => tracing::warn!(
                    "StyledWriter: formatter failed, using unformatted output: {err}"
                ),
            }
        }
        if self.trailing_newline {
            let trimmed = text.trim_end_matches('\n');
            text.truncate(trimmed.len());
            text.push('\n');
        }
        text
    }

    /// [`finish`](Self::finish) and write the result to `path`, creating
    /// parent directories as needed.
    pub fn write_to_file(&self, path: impl AsRef<Path>) -> crate::error::Result<()> {
        let path = path.as_ref();
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(path, self.finish())?;
        Ok(())
    }
}

impl fmt::Write for StyledWriter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.buffer.borrow_mut().write_str(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt::Write as _;

    #[test]
    fn write_line_indents_and_terminates() {
        let mut w = StyledWriter::new(WriterConfig::default());
        w.write_line("a");
        w.write_line("b");
        assert_eq!(w.finish(), "a\nb\n");
    }

    #[test]
    fn atom_joins_with_single_space() {
        let mut w = StyledWriter::new(WriterConfig::default());
        w.atom("let").atom("x").atom("=").atom("1").newline();
        assert_eq!(w.finish(), "let x = 1\n");
    }

    #[test]
    fn atom_ignores_empty_tokens() {
        let mut w = StyledWriter::new(WriterConfig::default());
        w.atom("x").atom("").atom("y").newline();
        assert_eq!(w.finish(), "x y\n");
    }

    #[test]
    fn block_indents_body_and_closes_brace() {
        let mut w = StyledWriter::new(WriterConfig::default());
        w.block("fn main()", |w| -> Result<(), ()> {
            w.write_line("a()");
            w.block("if true", |w| {
                w.write_line("b()");
                Ok(())
            })?;
            Ok(())
        })
        .unwrap();
        assert_eq!(
            w.finish(),
            "fn main() {\n    a()\n    if true {\n        b()\n    }\n}\n"
        );
    }

    #[test]
    fn block_next_line_brace_style() {
        let mut config = WriterConfig::default();
        config.brace_style = BraceStyle::NextLine;
        let mut w = StyledWriter::new(config);
        w.block("void main()", |w| -> Result<(), ()> {
            w.write_line("a();");
            Ok(())
        })
        .unwrap();
        assert_eq!(w.finish(), "void main()\n{\n    a();\n}\n");
    }

    #[test]
    fn block_closes_brace_even_when_body_errors() {
        let mut w = StyledWriter::new(WriterConfig::default());
        let result = w.block("fn main()", |w| -> Result<(), &'static str> {
            w.write_line("a()");
            Err("boom")
        });
        assert_eq!(result, Err("boom"));
        assert_eq!(w.finish(), "fn main() {\n    a()\n}\n");
    }

    #[test]
    fn tabs_and_custom_spacing() {
        let config = WriterConfig {
            indent_style: IndentStyle::Tabs,
            ..WriterConfig::default()
        };
        let mut w = StyledWriter::new(config);
        w.block("fn main()", |w| -> Result<(), ()> {
            w.write_line("a()");
            Ok(())
        })
        .unwrap();
        assert_eq!(w.finish(), "fn main() {\n\ta()\n}\n");
    }

    #[test]
    fn fmt_write_interop_splits_on_newlines() {
        let mut w = StyledWriter::new(WriterConfig::default());
        write!(w, "a = {}", 1).unwrap();
        writeln!(w).unwrap();
        write!(w, "b").unwrap();
        w.newline();
        assert_eq!(w.finish(), "a = 1\nb\n");
    }

    #[test]
    fn blank_line_has_no_trailing_whitespace() {
        let mut w = StyledWriter::new(WriterConfig::default());
        w.indented(|w| -> Result<(), ()> {
            w.write_line("a");
            w.newline();
            w.write_line("b");
            Ok(())
        })
        .unwrap();
        assert_eq!(w.finish(), "    a\n\n    b\n");
    }

    #[test]
    fn trailing_newline_is_normalized() {
        let mut w = StyledWriter::new(WriterConfig::default());
        w.write_line("a");
        w.newline();
        w.newline();
        assert_eq!(w.finish(), "a\n");
    }

    #[test]
    fn custom_function_formatter_runs_on_finish() {
        let config = WriterConfig {
            formatter: Some(Formatter::function(|s| Ok(s.to_uppercase()))),
            ..WriterConfig::default()
        };
        let mut w = StyledWriter::new(config);
        w.write_line("hello");
        assert_eq!(w.finish(), "HELLO\n");
    }

    #[test]
    fn write_to_file_creates_parent_dirs() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("nested/out.txt");
        let mut w = StyledWriter::new(WriterConfig::default());
        w.write_line("hi");
        w.write_to_file(&path).unwrap();
        assert_eq!(fs::read_to_string(&path).unwrap(), "hi\n");
    }

    #[test]
    fn wraps_long_lines_on_word_boundaries() {
        let config = WriterConfig {
            max_width: Some(20),
            ..WriterConfig::default()
        };
        let mut w = StyledWriter::new(config);
        w.atom("let")
            .atom("x")
            .atom("=")
            .atom("aaaa")
            .atom("+")
            .atom("bbbb")
            .atom("+")
            .atom("cccc")
            .newline();
        assert_eq!(w.finish(), "let x = aaaa + bbbb\n    + cccc\n");
    }

    #[test]
    fn wrapping_accounts_for_indent_depth() {
        let config = WriterConfig {
            max_width: Some(20),
            ..WriterConfig::default()
        };
        let mut w = StyledWriter::new(config);
        w.block("fn f()", |w| -> Result<(), ()> {
            w.atom("let")
                .atom("x")
                .atom("=")
                .atom("aaaa")
                .atom("+")
                .atom("bbbb")
                .atom("+")
                .atom("cccc")
                .newline();
            Ok(())
        })
        .unwrap();
        // Body is at depth 1 (4 spaces); continuation goes to depth 2 (8 spaces),
        // both narrowing the usable width compared to the top-level case above.
        assert_eq!(
            w.finish(),
            "fn f() {\n    let x = aaaa +\n        bbbb + cccc\n}\n"
        );
    }

    #[test]
    fn unbreakable_token_overflows_rather_than_splitting_mid_token() {
        let config = WriterConfig {
            max_width: Some(10),
            ..WriterConfig::default()
        };
        let mut w = StyledWriter::new(config);
        w.write_line("aaaaaaaaaaaaaaaaaaaa");
        assert_eq!(w.finish(), "aaaaaaaaaaaaaaaaaaaa\n");
    }

    #[test]
    fn no_wrapping_when_max_width_unset() {
        let mut w = StyledWriter::new(WriterConfig::default());
        w.write_line(
            "a very long line that would exceed any reasonable width if wrapping were enabled here",
        );
        assert_eq!(
            w.finish(),
            "a very long line that would exceed any reasonable width if wrapping were enabled here\n"
        );
    }

    #[test]
    fn short_line_is_unaffected_by_max_width() {
        let config = WriterConfig {
            max_width: Some(80),
            ..WriterConfig::default()
        };
        let mut w = StyledWriter::new(config);
        w.write_line("short");
        assert_eq!(w.finish(), "short\n");
    }

    #[test]
    fn ensure_blank_line_is_idempotent_and_skips_when_empty() {
        let mut w = StyledWriter::new(WriterConfig::default());
        w.ensure_blank_line(); // no-op: buffer is empty
        w.write_line("a");
        w.ensure_blank_line();
        w.ensure_blank_line(); // already blank: no-op
        w.write_line("b");
        assert_eq!(w.finish(), "a\n\nb\n");
    }

    #[test]
    fn write_verbatim_ignores_current_indent_and_normalizes_trailing_newline() {
        let mut w = StyledWriter::new(WriterConfig::default());
        w.increase_indent();
        w.write_verbatim("interface Foo {\n  x: number;\n}\n\n\n");
        w.write_line("after");
        assert_eq!(w.finish(), "interface Foo {\n  x: number;\n}\n    after\n");
    }

    #[test]
    fn write_lines_indents_every_line_not_just_the_first() {
        let mut w = StyledWriter::new(WriterConfig::default());
        w.increase_indent();
        // A pre-rendered snippet with its own baked-in *relative* indent,
        // as if produced standalone at depth 0 (e.g. by an expression
        // renderer building a nested `run { ... }` block as a string).
        w.write_lines("run {\n    val x = 1\n}");
        assert_eq!(w.finish(), "    run {\n        val x = 1\n    }\n");
    }

    #[test]
    fn write_lines_matches_write_line_for_single_line_input() {
        let mut w = StyledWriter::new(WriterConfig::default());
        w.increase_indent();
        w.write_lines("val x = 1");
        assert_eq!(w.finish(), "    val x = 1\n");
    }

    #[test]
    fn raw_indent_primitives_compose_like_block() {
        let mut w = StyledWriter::new(WriterConfig::default());
        w.write_line("outer {");
        w.increase_indent();
        w.write_line("inner");
        w.decrease_indent();
        w.write_line("}");
        assert_eq!(w.finish(), "outer {\n    inner\n}\n");
    }

    #[test]
    fn swap_buffer_redirects_a_scratch_render_without_losing_indent() {
        let mut w = StyledWriter::new(WriterConfig::default());
        w.write_line("outer");
        w.increase_indent();
        let saved = w.swap_buffer(String::new());
        w.write_line("scratch");
        let scratch = w.swap_buffer(saved);
        w.write_line("resumed");
        assert_eq!(scratch, "    scratch\n");
        assert_eq!(w.finish(), "outer\n    resumed\n");
    }

    #[test]
    fn indent_component_tracks_depth_and_renders_prefixes_independent_of_any_buffer() {
        let mut indent = Indent::new(IndentStyle::Spaces(2));
        assert_eq!(indent.depth(), 0);
        assert_eq!(indent.prefix(), "");
        indent.increase().increase();
        assert_eq!(indent.depth(), 2);
        assert_eq!(indent.prefix(), "    ");
        assert_eq!(indent.prefix_at(3), "      ");
        indent.decrease();
        assert_eq!(indent.prefix(), "  ");
    }

    #[test]
    fn indented_buffer_works_standalone_without_a_styled_file_writer() {
        let mut buf = IndentedBuffer::new(BufferConfig::default());
        buf.write_line("outer {");
        buf.indented(|b| -> Result<(), ()> {
            b.write_line("inner");
            Ok(())
        })
        .unwrap();
        buf.write_line("}");
        assert_eq!(buf.raw_contents(), "outer {\n    inner\n}\n");
    }

    /// The whole point of layering `StyledWriter` on top of a reusable
    /// `IndentedBuffer`: a snippet can be built self-consistently at depth 0
    /// in one buffer, then embedded via `write_lines` into another buffer
    /// sitting at some real, unrelated depth — the two compose additively
    /// (real prefix + the snippet's own already-consistent relative prefix)
    /// instead of requiring the snippet builder to know its eventual
    /// embedding depth, or to hardcode per-level literal spacing.
    fn render_nested_snippet() -> String {
        let mut scratch = IndentedBuffer::new(BufferConfig::default());
        scratch.write_line("run {");
        scratch
            .indented(|b| -> Result<(), ()> {
                b.write_line("val x = 1");
                Ok(())
            })
            .unwrap();
        scratch.write_line("}");
        scratch.raw_contents().trim_end_matches('\n').to_string()
    }

    #[test]
    fn snippet_built_standalone_embeds_correctly_at_any_real_depth() {
        let snippet = render_nested_snippet();
        assert_eq!(snippet, "run {\n    val x = 1\n}");

        let mut w = StyledWriter::new(WriterConfig::default());
        w.block("fun outer()", |w| -> Result<(), ()> {
            w.block("fun inner()", |w| {
                w.write_lines(&snippet);
                Ok(())
            })
        })
        .unwrap();
        assert_eq!(
            w.finish(),
            "fun outer() {\n    fun inner() {\n        run {\n            val x = 1\n        }\n    }\n}\n"
        );
    }
}
