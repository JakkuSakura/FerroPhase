//! FerroPhase frontend for a basic Lean 4 subset: `def` declarations,
//! `Nat`/`Int`/`Bool`/`String`, `let`/`if-then-else`, arithmetic and
//! comparison operators, function application, literals, comments, and
//! `{binder : base // predicate}` refinement/subtype types. Deliberately
//! not real Lean 4 — no theorem/tactic syntax, no `match`, no inductive
//! types, no typeclasses, no Pi/arrow dependent function types, no
//! whitespace/indentation sensitivity (real Lean relies on layout for
//! `let`-chains without `;`; this subset bounds a `let`'s value expression
//! by the grammar instead, so newlines are insignificant).

pub mod error;
pub mod frontend;
pub mod lexer;
pub mod package;
pub mod parser;
pub mod serializer;

pub use frontend::{LEAN, LeanFrontend};
