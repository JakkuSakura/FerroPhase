#!/usr/bin/env fp run
//! Comprehensive println!/print showcase covering variadic arguments and runtime formatting

const fn main() {
    // Basic printing
    print("Hello");
    print("World with newlines");
    println!();

    // Variadic arguments with mixed types
    print("Number:", 42);
    print("Boolean:", true, false);
    print("Mixed:", 1, 2.5, "text", true);
    println!();

    // Namespace variant
    std::io::print("Namespace test", "still works");
    println!();

    // Placeholder style formatting using println!
    let value = 7;
    println!("value = {}", value);
    println!("math: {} + {} = {}", 2, 3, 5);

    // Regression checks for runtime printf bridge
    print("This", "stays", "on", "one", "line");
    println!();
    print("Continuing without newline");
    print(" - appended content");
    println!();

    // Special values
    print("Unit:", ());
    print("Null:", null);
    println!();
}
