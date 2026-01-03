use std::borrow::Cow;

/// Maps std-facing function names to glibc runtime calls.
///
/// This is backend-specific: it encodes the glibc ABI surface that the LLVM
/// backend expects, and should not be used for other runtimes.
pub(crate) fn map_std_function_to_runtime(fn_name: &str) -> Cow<'_, str> {
    let mapped = match fn_name {
        // I/O helpers map to C stdio calls.
        "printf" => "printf",
        "println!" => "printf",
        "print!" => "printf",
        "eprint" | "eprint!" | "std::io::eprint" => "fprintf",
        "eprintln" | "eprintln!" | "std::io::eprintln" => "fprintf",

        // Memory management wrappers.
        "std::alloc::alloc" => "malloc",
        "std::alloc::dealloc" => "free",
        "std::alloc::realloc" => "realloc",

        // Basic math (libm).
        "std::f64::sin" => "sin",
        "std::f64::cos" => "cos",
        "std::f64::tan" => "tan",
        "std::f64::sqrt" => "sqrt",
        "std::f64::pow" => "pow",
        "std::f32::sin" => "sinf",
        "std::f32::cos" => "cosf",
        "std::f32::tan" => "tanf",
        "std::f32::sqrt" => "sqrtf",
        "std::f32::pow" => "powf",

        // String utilities.
        "std::str::len" => "strlen",
        "std::str::cmp" => "strcmp",
        _ => return Cow::Borrowed(fn_name),
    };

    Cow::Borrowed(mapped)
}
