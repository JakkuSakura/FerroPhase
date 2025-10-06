//! Common printer configuration and utilities

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndentationConfig {
    /// Type of indentation to use
    pub style: IndentationStyle,
    /// Number of spaces or tabs per indentation level
    pub size: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IndentationStyle {
    /// Use spaces for indentation
    Spaces,
    /// Use tabs for indentation
    Tabs,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormattingConfig {
    /// Indentation configuration
    pub indentation: IndentationConfig,
    /// Maximum line length before wrapping (0 = no limit)
    pub max_line_length: usize,
    /// Whether to add trailing newlines
    pub trailing_newline: bool,
    /// Whether to insert blank lines between declarations
    pub blank_lines_between_declarations: bool,
    /// Line ending style
    pub line_ending: LineEnding,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LineEnding {
    /// Unix-style line endings (\n)
    Unix,
    /// Windows-style line endings (\r\n)
    Windows,
    /// Mac classic line endings (\r)
    Mac,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstSerializerConfig {
    /// Formatting configuration
    pub formatting: FormattingConfig,
    /// Whether to minify output (ignore formatting preferences)
    pub minify: bool,
    /// Whether to include comments in output
    pub include_comments: bool,
    /// Whether to include source maps
    pub source_maps: bool,
    /// Whether to print types as comments for typed AST nodes
    pub print_types: bool,
}

impl Default for IndentationConfig {
    fn default() -> Self {
        Self {
            style: IndentationStyle::Spaces,
            size: 2,
        }
    }
}

impl Default for FormattingConfig {
    fn default() -> Self {
        Self {
            indentation: IndentationConfig::default(),
            max_line_length: 100,
            trailing_newline: true,
            blank_lines_between_declarations: true,
            line_ending: LineEnding::Unix,
        }
    }
}

impl Default for AstSerializerConfig {
    fn default() -> Self {
        Self {
            formatting: FormattingConfig::default(),
            minify: false,
            include_comments: true,
            source_maps: false,
            print_types: false,
        }
    }
}

impl IndentationConfig {
    /// Create a new indentation config with spaces
    pub fn spaces(size: usize) -> Self {
        Self {
            style: IndentationStyle::Spaces,
            size,
        }
    }

    /// Create a new indentation config with tabs
    pub fn tabs(size: usize) -> Self {
        Self {
            style: IndentationStyle::Tabs,
            size,
        }
    }

    /// Generate an indentation string for the given level
    pub fn indent_string(&self, level: usize) -> String {
        let unit = match self.style {
            IndentationStyle::Spaces => " ".repeat(self.size),
            IndentationStyle::Tabs => "\t".repeat(self.size),
        };
        unit.repeat(level)
    }
}

impl LineEnding {
    /// Get the string representation of the line ending
    pub fn as_str(&self) -> &'static str {
        match self {
            LineEnding::Unix => "\n",
            LineEnding::Windows => "\r\n",
            LineEnding::Mac => "\r",
        }
    }
}

impl AstSerializerConfig {
    /// Create a new config with 2-space indentation (common for web languages)
    pub fn compact() -> Self {
        Self {
            formatting: FormattingConfig {
                indentation: IndentationConfig::spaces(2),
                max_line_length: 120,
                trailing_newline: true,
                blank_lines_between_declarations: true,
                line_ending: LineEnding::Unix,
            },
            minify: false,
            include_comments: true,
            source_maps: false,
            print_types: false,
        }
    }

    /// Create a new config with 4-space indentation (common for systems languages)
    pub fn standard() -> Self {
        Self {
            formatting: FormattingConfig {
                indentation: IndentationConfig::spaces(4),
                max_line_length: 100,
                trailing_newline: true,
                blank_lines_between_declarations: false,
                line_ending: LineEnding::Unix,
            },
            minify: false,
            include_comments: true,
            source_maps: false,
            print_types: false,
        }
    }

    /// Create a minified config
    pub fn minified() -> Self {
        Self {
            formatting: FormattingConfig {
                indentation: IndentationConfig::spaces(0),
                max_line_length: 0,
                trailing_newline: false,
                blank_lines_between_declarations: false,
                line_ending: LineEnding::Unix,
            },
            minify: true,
            include_comments: false,
            source_maps: false,
            print_types: false,
        }
    }

    /// Enable type printing in comments
    pub fn with_types(mut self) -> Self {
        self.print_types = true;
        self
    }
}
