// Test simple print() functionality

const fn main() {
    // Test basic print functionality
    print("Hello");
    print("World");
    
    // Test print with multiple arguments
    print("Multiple", "arguments");
    
    // Test print with numbers
    print("Number:", 42);
    
    // Test print with booleans
    print("Boolean:", true);
    
    // Test empty print
    print();
    
    // Test namespace usage
    std::io::print("Namespace test");
    
    // Compare with println!
    println!("This is println!");
    print("This is print");
}