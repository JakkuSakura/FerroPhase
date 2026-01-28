/// Macro to return early with an error
#[macro_export]
macro_rules! bail {
    ($($arg:tt)*) => {
        return Err($crate::error::Error::from(format!($($arg)*)))
    };
}

/// Log a warning message
#[macro_export]
macro_rules! warn {
    ($($arg:tt)*) => {
        eprintln!("[warn] {}", format!($($arg)*))
    };
}

/// Log a debug message
#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {
        eprintln!("[debug] {}", format!($($arg)*))
    };
}

/// Log an info message
#[macro_export]
macro_rules! info {
    ($($arg:tt)*) => {
        eprintln!("[info] {}", format!($($arg)*))
    };
}

/// Log an error message
#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => {
        eprintln!("[error] {}", format!($($arg)*))
    };
}

/// Log a trace message
#[macro_export]
macro_rules! trace {
    ($($arg:tt)*) => {
        eprintln!("[trace] {}", format!($($arg)*))
    };
}

/// Assert expression is true at runtime, with formatted message
#[macro_export]
macro_rules! assert_expr {
    ($cond:expr, $($arg:tt)*) => {
        if !$cond {
            panic!("assertion failed: {}", format_args!($($arg)*));
        }
    };
}
