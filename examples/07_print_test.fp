// Test std::io::print function (Python-like print without newline)

const fn main() {
    // Test basic print functionality
    print("Hello");
    print(" ");
    print("World");
    println!();  // Add a newline
    
    // Test print with multiple arguments (should join with spaces)
    print("Testing", "multiple", "arguments");
    println!();
    
    // Test print with numbers and strings
    print("Count:", 42, "items");
    println!();
    
    // Test std::io::print explicitly
    std::io::print("Explicit namespace call");
    println!();
    
    // Comparison with println!
    println!("This has a newline");
    print("This does not");
    print(" - continued on same line");
    println!();
}