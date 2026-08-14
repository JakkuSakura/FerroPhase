use std::fmt::{self, Formatter};

use crate::writer::{IndentStyle, StyledFileWriter, WriterConfig};

/// Configuration for pretty-printing intermediate representations.
#[derive(Debug, Clone)]
pub struct PrettyOptions {
    /// Number of spaces to indent per nesting level.
    pub indent_size: usize,
    /// Include span metadata when available.
    pub show_spans: bool,
    /// Include inferred/static type annotations when available.
    pub show_types: bool,
}

impl Default for PrettyOptions {
    fn default() -> Self {
        Self {
            indent_size: 4,
            show_spans: false,
            show_types: true,
        }
    }
}

/// Formatting context shared across pretty printers.
///
/// Backed by a [`StyledFileWriter`] for indentation/line bookkeeping: output
/// is buffered as `fmt_pretty` walks the tree and flushed to the real
/// `Formatter` once, by [`PrettyDisplay::fmt`], rather than written
/// line-by-line as each node renders.
pub struct PrettyCtx<'a> {
    pub options: &'a PrettyOptions,
    writer: StyledFileWriter,
}

impl<'a> PrettyCtx<'a> {
    pub fn new(options: &'a PrettyOptions) -> Self {
        let config = WriterConfig {
            indent_style: IndentStyle::Spaces(options.indent_size.max(1)),
            ..WriterConfig::default()
        };
        Self {
            options,
            writer: StyledFileWriter::new(config),
        }
    }

    /// Write one line at the current indent depth.
    pub fn write_line(&mut self, line: impl AsRef<str>) -> fmt::Result {
        self.writer.write_line(line);
        Ok(())
    }

    /// Same as [`Self::write_line`], kept for pretty-printers written
    /// against the older Formatter-threading API. `f` is unused now that
    /// output is buffered — it's `f`'s caller, [`PrettyDisplay::fmt`], that
    /// flushes the buffer once the whole tree has been walked.
    pub fn writeln(&mut self, _f: &mut Formatter<'_>, line: impl AsRef<str>) -> fmt::Result {
        self.write_line(line)
    }

    pub fn current_indent(&self) -> usize {
        self.writer.indent_depth() * self.options.indent_size
    }

    pub fn increase_indent(&mut self) {
        self.writer.increase_indent();
    }

    pub fn decrease_indent(&mut self) {
        self.writer.decrease_indent();
    }

    pub fn with_indent<F>(&mut self, mut f_closure: F) -> fmt::Result
    where
        F: FnMut(&mut Self) -> fmt::Result,
    {
        self.increase_indent();
        let result = f_closure(self);
        self.decrease_indent();
        result
    }

    /// Flush the buffered output. Called once, by [`PrettyDisplay::fmt`],
    /// after the whole tree has been walked.
    fn finish(&self) -> String {
        self.writer.finish()
    }
}

/// Trait implemented by IR nodes that support pretty-printing.
pub trait PrettyPrintable {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result;
}

/// Helper wrapper implementing `Display` by delegating to `PrettyPrintable`.
pub struct PrettyDisplay<'a, T> {
    value: &'a T,
    options: PrettyOptions,
}

impl<'a, T> PrettyDisplay<'a, T> {
    pub fn new(value: &'a T, options: PrettyOptions) -> Self {
        Self { value, options }
    }
}

impl<'a, T> fmt::Display for PrettyDisplay<'a, T>
where
    T: PrettyPrintable,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut ctx = PrettyCtx::new(&self.options);
        self.value.fmt_pretty(f, &mut ctx)?;
        f.write_str(&ctx.finish())
    }
}

/// Convenience helper to build a `PrettyDisplay` wrapper.
pub fn pretty<'a, T>(value: &'a T, options: PrettyOptions) -> PrettyDisplay<'a, T>
where
    T: PrettyPrintable,
{
    PrettyDisplay::new(value, options)
}

pub fn escape_string(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    for ch in input.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ch if ch.is_control() => {
                use std::fmt::Write as _;
                let _ = write!(out, "\\u{{{:x}}}", ch as u32);
            }
            _ => out.push(ch),
        }
    }
    out
}

pub fn escape_char(ch: char) -> String {
    match ch {
        '\'' => "\\'".to_string(),
        '\\' => "\\\\".to_string(),
        '\n' => "\\n".to_string(),
        '\r' => "\\r".to_string(),
        '\t' => "\\t".to_string(),
        ch if ch.is_control() => format!("\\u{{{:x}}}", ch as u32),
        _ => ch.to_string(),
    }
}
