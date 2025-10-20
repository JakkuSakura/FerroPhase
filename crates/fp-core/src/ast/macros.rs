use crate::ast::Path;
use crate::common_enum;
use crate::common_struct;

common_enum! {
    /// Delimiter used for a macro invocation.
    pub enum MacroDelimiter {
        Parenthesis,
        Bracket,
        Brace,
    }
}

common_struct! {
    /// Source-level representation of a macro invocation captured in the AST.
    pub struct MacroInvocation {
        pub path: Path,
        pub delimiter: MacroDelimiter,
        /// Raw token stream inside the macro invocation, stringified for portability.
        pub tokens: String,
    }
}

impl MacroInvocation {
    pub fn new(path: Path, delimiter: MacroDelimiter, tokens: impl Into<String>) -> Self {
        Self {
            path,
            delimiter,
            tokens: tokens.into(),
        }
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
}

common_struct! {
    /// Item-level macro (e.g., macro_rules!, module attributes as macros, or item macros).
    /// This preserves the source-level macro for a later dedicated lowering pass.
    pub struct ItemMacro {
        pub invocation: MacroInvocation,
    }
}

impl ItemMacro {
    pub fn new(invocation: MacroInvocation) -> Self {
        Self { invocation }
    }
}
