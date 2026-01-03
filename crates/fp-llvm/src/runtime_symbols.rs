use fp_core::lir::RuntimeSymbol;

/// Map a symbol name to its glibc runtime equivalent.
///
/// This is LLVM backend-specific and should not live in generic MIR→LIR lowering.
pub fn map_runtime_symbol_glibc(name: &str) -> Option<RuntimeSymbol> {
    let mapped = match name {
        // I/O helpers map to C stdio calls.
        "println!" => RuntimeSymbol::Printf,
        "print!" => RuntimeSymbol::Printf,
        "eprint" | "eprint!" | "std::io::eprint" => RuntimeSymbol::Fprintf,
        "eprintln" | "eprintln!" | "std::io::eprintln" => RuntimeSymbol::Fprintf,

        // Memory management wrappers.
        "std::alloc::alloc" => RuntimeSymbol::Malloc,
        "std::alloc::dealloc" => RuntimeSymbol::Free,
        "std::alloc::realloc" => RuntimeSymbol::Realloc,

        // Basic math (libm).
        "std::f64::sin" => RuntimeSymbol::Sin,
        "std::f64::cos" => RuntimeSymbol::Cos,
        "std::f64::tan" => RuntimeSymbol::Tan,
        "std::f64::sqrt" => RuntimeSymbol::Sqrt,
        "std::f64::pow" => RuntimeSymbol::Pow,
        "std::f32::sin" => RuntimeSymbol::Sinf,
        "std::f32::cos" => RuntimeSymbol::Cosf,
        "std::f32::tan" => RuntimeSymbol::Tanf,
        "std::f32::sqrt" => RuntimeSymbol::Sqrtf,
        "std::f32::pow" => RuntimeSymbol::Powf,

        // String utilities.
        "std::str::len" => RuntimeSymbol::Strlen,
        "std::str::cmp" => RuntimeSymbol::Strcmp,

        // System helpers.
        "std::process::exit" => RuntimeSymbol::Exit,
        _ => return None,
    };
    Some(mapped)
}
