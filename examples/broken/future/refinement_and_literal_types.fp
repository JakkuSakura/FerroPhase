//! PROPOSED SYNTAX -- NOT IMPLEMENTED. DOES NOT PARSE OR COMPILE TODAY.
//!
//! This file is a design illustration for two potential features:
//!   1. Value-dependent / refinement types: a base type narrowed by a
//!      predicate over a bound variable, e.g. `x: ((x: Int) >= 0)`
//!      (compare Lean 4's `{x : Int // x >= 0}`, F*'s refinement types,
//!      Liquid Haskell).
//!   2. Literal value types: a type inhabited by exactly one compile-time
//!      value (compare TypeScript literal types, Rust's unstable
//!      `#[feature(adt_const_params)]`).
//!
//! It lives under `examples/future/` -- a directory intentionally excluded
//! from `examples/*.fp` globs used by `scripts/run_examples_exec.sh`,
//! `scripts/run_examples_bytecode.sh`, and `scripts/run_example_snapshots.sh`
//! -- so it is never parsed, compiled, or executed by CI.
//!
//! Every construct below that goes beyond current FerroPhase syntax is
//! flagged with a `// NEW:` comment explaining what it means and, where
//! relevant, what compiler machinery it would require. See the feasibility
//! discussion referenced at the bottom of this file for the full gap
//! analysis (unifier, comptime-during-typing, closed TyKind enums, etc.).

// =====================================================================
// 1. Refinement types
// =====================================================================

// NEW: `x: (<predicate over x>)` is an anonymous refinement type: the set
// of `Int` values `x` for which the predicate holds. Structurally this
// reuses the same "type carries an expression" shape that already exists
// today in `Ty::TypeBounds` and `TypeArray { len: Expr }` -- it would be
// promoted from "bounds we mostly ignore" to "bounds we prove or check."
type NonNegative = x: ((x: Int) >= 0);

// A refinement can name multiple constraints; today's `Ty::TypeBounds`
// already stores `Vec<Expr>`, so conjunction is the natural extension.
type Percentage = x: ((x: Int) >= 0 && (x: Int) <= 100);

// Refinements aren't limited to scalars. `xs.len()` reads as an ordinary
// method call the checker would need to const-eval or reason about.
type NonEmpty<T> = xs: ((xs: Vec<T>) .len() > 0);

struct Account {
    // A refinement type used as a field type: the compiler is responsible
    // for proving every write to `balance` upholds `>= 0`, not just the
    // constructor.
    balance: NonNegative,
}

impl Account {
    fn new(balance: NonNegative) -> Self {
        Self { balance }
    }

    // NEW: refinement types in a function signature act as a contract.
    // `amount: x: ((x: Int) >= 0 && (x: Int) <= balance)` refers to another
    // parameter (`balance`) inside its own predicate -- a genuinely
    // *dependent* type, since the type of `amount` depends on the value of
    // `balance`. Discharging this needs real value-level reasoning during
    // typing, which today's `TypingContext::request_comptime` protocol
    // resolves to a placeholder value rather than a real one.
    fn withdraw(
        balance: NonNegative,
        amount: x: ((x: Int) >= 0 && (x: Int) <= balance),
    ) -> NonNegative {
        // Because `amount <= balance` is already proven by the type of
        // `amount`, the subtraction below can never underflow -- the
        // refinement discharges what would otherwise be a runtime check.
        balance - amount
    }
}

// NEW: refinements checked against *literal* arguments should discharge
// entirely at compile time via const-eval -- no runtime check emitted.
fn deposit_fixed_fee() -> NonNegative {
    // 5 is a literal the checker can prove satisfies `x >= 0` without
    // asking the interpreter for anything: pure syntactic const-eval.
    5
}

// NEW: refinements checked against a *dynamic* (non-const) value cannot be
// proven at compile time, so the compiler falls back to an inserted
// runtime check that panics with the refinement's predicate as context --
// analogous to how `array[i]` bounds-checks fall back to a runtime trap
// when `i` isn't known statically.
fn deposit_user_amount(raw: Int) -> NonNegative {
    // Desugars to: assert!(raw >= 0, "refinement violated: x >= 0"); raw
    raw
}

// =====================================================================
// 2. Literal value types
// =====================================================================

// NEW: a literal value type is inhabited by exactly one value. This is
// refinement types' degenerate case (`x: ((x: Int) == 0)`) given its own
// lightweight syntax, the way singleton types are sugar for refinements
// in most systems that have both.
type Zero = 0;

// NEW: a union of literal types models a closed set of exact values --
// the literal-type analogue of an enum, useful where transpile targets
// (TypeScript, Python `Literal[...]`) already have a native equivalent.
type ConnectionState = "pending" | "active" | "closed";

struct Connection {
    state: ConnectionState,
}

impl Connection {
    // NEW: literal types narrow return values the same way pattern
    // matching narrows scrutinees -- each match arm's literal return type
    // must be a member of the declared union.
    fn describe(state: ConnectionState) -> &str {
        match state {
            "pending" => "waiting to connect",
            "active" => "connected",
            "closed" => "connection closed",
        }
    }
}

// NEW: combining literal types with const generics -- `N` is required to
// be a literal drawn from a refined range, giving buffer capacities that
// are checked at the type level instead of via a runtime-sized allocation
// check. This depends on const generics being fully supported first (today
// MIR lowering warns "const generics are ignored during MIR lowering" and
// typing rejects const generic arguments outright).
struct Buffer<const N: usize: (n: ((n: usize) > 0 && (n: usize) <= 4096))> {
    data: [u8; N],
}

fn main() {
    println!("Refinement & literal type sketches (illustrative only)");

    let acct = Account::new(100);
    let acct = Account::new(Account::withdraw(acct.balance, 40));
    println!("balance after withdraw = {}", acct.balance);

    println!("fixed fee = {}", deposit_fixed_fee());
    println!("user deposit = {}", deposit_user_amount(25));

    let conn = Connection { state: "active" };
    println!("connection: {}", Connection::describe(conn.state));
}

// ---------------------------------------------------------------------
// Prerequisites before any of this is real (see the feasibility notes
// from studying crates/fp-typing and crates/fp-core's Ty/TyKind types):
//
//   1. Finish const generics end-to-end (currently stubbed/rejected in
//      crates/fp-typing/src/hir_typeck.rs and
//      crates/fp-backend/src/transforms/hir_to_mir/expr.rs).
//   2. Make `TypingContext::request_comptime` resolve real values during
//      typing instead of the current placeholder `Value::Undefined`
//      (crates/fp-compiler/src/driver.rs).
//   3. Add a predicate-discharge layer: at minimum const-eval-based proof
//      for literal cases, ideally an SMT bridge for the general case --
//      `require_same`/`unify_call_types` in hir_typeck.rs only do
//      structural equality today, with no notion of logical implication.
//   4. Add a `Refinement`/`Literal` variant to the closed HIR/MIR `TyKind`
//      enums and update every exhaustive match across ast_to_hir,
//      hir_to_mir, hir_to_ast, and mir_to_lir.
//   5. Define a degradation rule per transpile target (TypeScript, Python,
//      Go, Rust, ...) per docs/Language.md's semantic contract, since none
//      of them natively enforce refinement predicates.
// ---------------------------------------------------------------------
