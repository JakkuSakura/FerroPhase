use crate::ast::{Expr, Ident, Item, Path, Ty};
use crate::common_enum;
use crate::common_struct;
use crate::error::Result;
use crate::span::Span;

common_enum! {
    /// Delimiter used for a macro invocation.
    pub enum MacroDelimiter {
        Parenthesis,
        Bracket,
        Brace,
    }
}

common_struct! {
    /// Single token inside a macro token tree.
    pub struct MacroToken {
        pub text: String,
        #[serde(default)]
        pub span: Span,
    }
}

common_struct! {
    /// A delimited group inside a macro token tree.
    pub struct MacroGroup {
        pub delimiter: MacroDelimiter,
        pub tokens: Vec<MacroTokenTree>,
        #[serde(default)]
        pub span: Span,
    }
}

common_enum! {
    /// Token tree representation used by Rust-like macros.
    pub enum MacroTokenTree {
        Token(MacroToken),
        Group(MacroGroup),
    }
}

common_struct! {
    /// Source-level representation of a macro invocation captured in the AST.
    pub struct MacroInvocation {
        pub path: Path,
        pub delimiter: MacroDelimiter,
        /// Raw token stream inside the macro invocation, stringified for portability.
        pub tokens: String,
        /// Structured token tree for macro expansion.
        #[serde(default)]
        pub token_trees: Vec<MacroTokenTree>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub span: Option<Span>,
    }
}

impl MacroInvocation {
    pub fn new(path: Path, delimiter: MacroDelimiter, tokens: impl Into<String>) -> Self {
        Self {
            path,
            delimiter,
            tokens: tokens.into(),
            token_trees: Vec::new(),
            span: None,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_token_trees(mut self, token_trees: Vec<MacroTokenTree>) -> Self {
        self.token_trees = token_trees;
        self
    }

    pub fn span(&self) -> Span {
        self.span.unwrap_or_else(Span::null)
    }
}

common_struct! {
    /// Expression node representing a macro invocation that will be lowered later.
    pub struct ExprMacro {
        pub invocation: MacroInvocation,
    }
}

impl ExprMacro {
    pub fn new(invocation: MacroInvocation) -> Self {
        Self { invocation }
    }

    pub fn span(&self) -> Span {
        self.invocation.span()
    }
}

common_struct! {
    /// Item-level macro (e.g., macro_rules!, module attributes as macros, or item macros).
    /// This preserves the source-level macro for a later dedicated lowering pass.
    pub struct ItemMacro {
        pub invocation: MacroInvocation,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub declared_name: Option<Ident>,
    }
}

impl ItemMacro {
    pub fn new(invocation: MacroInvocation) -> Self {
        Self {
            invocation,
            declared_name: None,
        }
    }

    pub fn span(&self) -> Span {
        self.invocation.span()
    }
}

/// Hook for parsing expanded macro token trees back into AST nodes.
pub trait MacroExpansionParser: Send + Sync {
    fn parse_items(&self, tokens: &[MacroTokenTree]) -> Result<Vec<Item>>;
    fn parse_expr(&self, tokens: &[MacroTokenTree]) -> Result<Expr>;
    fn parse_type(&self, tokens: &[MacroTokenTree]) -> Result<Ty>;
}

common_enum! {
    /// Repetition operator on a `$(...)` group inside a `macro_rules!` matcher
    /// or transcriber: `*` (zero or more), `+` (one or more), `?` (zero or one).
    pub enum MacroRepetitionOp {
        Star,
        Plus,
        Question,
    }
}

common_struct! {
    /// A `$name:fragment` metavariable inside a `macro_rules!` matcher, e.g.
    /// `$future:expr`.
    pub struct MacroMetavar {
        pub name: String,
        /// The fragment specifier text (`expr`, `ident`, `ty`, `pat`, `tt`,
        /// `literal`, `block`, `path`, ...), unvalidated at parse time.
        pub fragment: String,
    }
}

common_struct! {
    /// A `$(...)sep? op` repetition group inside a `macro_rules!` matcher,
    /// e.g. `$($future:expr),+`.
    pub struct MacroRepetition {
        pub inner: Vec<MacroMatcherToken>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub separator: Option<MacroToken>,
        pub op: MacroRepetitionOp,
    }
}

common_struct! {
    /// A literal delimited group inside a matcher (e.g. matching a literal
    /// `(a, b)` shape in the invocation) — distinct from a `$(...)`
    /// repetition group, which is `MacroMatcherToken::Repetition` instead.
    pub struct MacroMatcherGroup {
        pub delimiter: MacroDelimiter,
        pub tokens: Vec<MacroMatcherToken>,
    }
}

common_enum! {
    /// One node in a `macro_rules!` matcher (the left side of `=>`) — the
    /// structured counterpart to `MacroTokenTree`, distinguishing literal
    /// tokens from metavariables and repetition groups.
    pub enum MacroMatcherToken {
        Token(MacroToken),
        Metavar(MacroMetavar),
        Repetition(MacroRepetition),
        Group(MacroMatcherGroup),
    }
}

common_struct! {
    /// A single `(matcher) => { transcriber };` rule inside a `macro_rules!`
    /// definition.
    pub struct MacroRule {
        pub matcher: Vec<MacroMatcherToken>,
        pub transcriber: Vec<MacroTokenTree>,
    }
}

common_struct! {
    /// A parsed `macro_rules! name { rule; rule; ... }` definition, structured
    /// enough to match invocations against and substitute their bindings into
    /// the winning rule's transcriber. Derived on demand from an `ItemMacro`'s
    /// raw `token_trees` (see `parse_macro_rules_def`), not stored on
    /// `ItemMacro` itself.
    pub struct MacroRulesDef {
        pub name: String,
        pub rules: Vec<MacroRule>,
    }
}
