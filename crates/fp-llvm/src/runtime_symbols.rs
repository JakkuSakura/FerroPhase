/// Map a symbol name to its glibc runtime equivalent.
///
/// This is LLVM backend-specific and should not live in generic MIR→LIR lowering.
pub fn map_runtime_symbol_glibc(name: &str) -> Option<&'static str> {
    let mapped = match name {
        // I/O helpers map to C stdio calls.
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

        // System helpers.
        "std::process::exit" => "exit",
        _ => return None,
    };
    Some(mapped)
}
