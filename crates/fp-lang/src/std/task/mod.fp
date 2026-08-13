// Future placeholder helpers for the runtime scheduler.

pub struct Future<T> {
    pub handle: any,
}

// Accept any Future-like value (async block, ::std::future::sleep, etc.).
//
// `#[unimplemented]`: `fut: any`/`Future<T>.handle: any` lower to
// `TyKind::Infer` (an unresolved inference variable), and this checker has
// no mechanism to actually solve `Infer` variables (unlike `TyKind::Param`,
// which `unify_call_types`'s substitution map handles correctly) — a
// genuine, deeper type-inference gap, not a bug fixable by adding another
// coercion rule. In practice this body is never reached anyway: any real
// call to `spawn(...)` is intercepted by name at normalization time
// (`OpKind::Spawn` in `fp-lang/src/normalization.rs`) before it would ever
// resolve to this same-named `.fp`-level function.
#[unimplemented]
fn spawn<T>(fut: any) -> Future<T> {
    Future { handle: ::std::task::spawn(fut) }
}

macro_rules! join {
    ($($future:expr),+ $(,)?) => { ::std::task::join($($future),+) };
}

macro_rules! select {
    ($($future:expr),+ $(,)?) => { ::std::task::select($($future),+) };
}
