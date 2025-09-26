// Test print() function with all types (Python-like)

const fn main() {
    // Test print with basic types
    print("String:", "Hello World");
    print("Integer:", 42);
    print("Boolean:", true, false);
    print("Decimal:", 3.14159);
    print("Unit:", ());
    
    // Test print with multiple arguments (space-separated)
    print("Multiple", "arguments", "with", "spaces");
    print("Mixed types:", 1, 2.5, "text", true);
    
    // Test empty print (should just print newline)
    print();
    
    // Test namespace variant
    std::io::print("Explicit namespace:", "works too");
    
    // Compare with println! 
    println!("This is println!");
    print("This is print with newline");
    
    // Test special values
    print("None value:", null);
    print("Unit value:", unit);
    
    // Test different data types together
    print("All together:", "text", 123, 45.67, true, false, ());
}