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
