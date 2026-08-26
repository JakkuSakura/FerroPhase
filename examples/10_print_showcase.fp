#!/usr/bin/env fp interpret
//! Comprehensive println!/print showcase covering variadic arguments and runtime formatting

const fn main() {
    println!("📘 Tutorial: 10_print_showcase.fp");
    println!("🧭 Focus: Comprehensive println!/print showcase covering variadic arguments and runtime formatting");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
    // Basic printing
    std::intrinsics::print("Hello");
    std::intrinsics::print("World with newlines");
    println!("");

    // Variadic arguments with mixed types
    std::intrinsics::print("Number:", 42);
    std::intrinsics::print("Boolean:", true, false);
    std::intrinsics::print("Mixed:", 1, 2.5, "text", true);
    println!();

    // Namespace variant
    std::intrinsics::print("Namespace test", "still works");
    println!();

    // Placeholder style formatting using println!
    let value = 7;
    println!("value = {}", value);
    println!("math: {} + {} = {}", 2, 3, 5);
    println!("float: {}", 3.14159);
    println!("chars: {} {}", 'a', 'Z');
    println!("tuple: ({}, {})", 1, 2);
    println!("bools: {} {}", true, false);

    // Regression checks for runtime printf bridge
    std::intrinsics::print("This", "stays", "on", "one", "line");
    println!();
    std::intrinsics::print("Continuing without newline");
    std::intrinsics::print(" - appended content");
    println!();

    // Special values
    std::intrinsics::print("Unit:", ());
    std::intrinsics::print("Null:", "null");
    println!("");

    // Strings with escapes
    println!("escaped: {} {}", "line1\nline2", "tab\tend");
}
