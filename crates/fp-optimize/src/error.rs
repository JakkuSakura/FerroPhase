use fp_core::diagnostics::report_error;
use fp_core::error::Error;
use fp_core::span::Span;

/// Create a simple optimization error with default span
pub fn optimization_error(message: impl Into<String>) -> Error {
    report_error(message)
}

/// Create an optimization error with a specific error code
pub fn optimization_error_with_code(message: impl Into<String>, code: impl Into<String>) -> Error {
    let message = format!("{} ({})", message.into(), code.into());
    report_error(message)
}

/// Create an optimization error with a specific span
pub fn optimization_error_with_span(message: impl Into<String>, span: Span) -> Error {
    let msg = format!("{} [span {}:{}]", message.into(), span.lo, span.hi);
    report_error(msg)
}

/// Create a generic error (when we don't have specific error information)
pub fn generic_error(message: impl Into<eyre::Error>) -> Error {
    Error::Generic(message.into())
}

// Convenience macros for generating optimization errors

/// Macro to return early with an optimization error
#[macro_export]
macro_rules! opt_bail {
    ($message:expr) => {
        return Err($crate::error::optimization_error($message))
    };
    ($message:expr, $code:expr) => {
        return Err($crate::error::optimization_error_with_code($message, $code))
    };
    ($message:expr, $span:expr) => {
        return Err($crate::error::optimization_error_with_span($message, $span))
    };
}

/// Macro to ensure a condition is true, or return an optimization error
#[macro_export]
macro_rules! opt_ensure {
    ($cond:expr, $message:expr) => {
        if !($cond) {
            $crate::opt_bail!($message);
        }
    };
    ($cond:expr, $message:expr, $code:expr) => {
        if !($cond) {
            $crate::opt_bail!($message, $code);
        }
    };
    ($cond:expr, $message:expr, $span:expr) => {
        if !($cond) {
            $crate::opt_bail!($message, $span);
        }
    };
}
