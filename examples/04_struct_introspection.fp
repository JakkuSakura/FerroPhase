#!/usr/bin/env fp run
//! Struct introspection demonstration

fn main() {
    // Define basic structs
    struct Point {
        x: f64,
        y: f64,
    }
    
    struct Color {
        r: u8,
        g: u8,
        b: u8,
    }
    
    // Introspection capabilities
    const POINT_SIZE: usize = sizeof!(Point);
    const COLOR_SIZE: usize = sizeof!(Color);
    const POINT_FIELDS: usize = field_count!(Point);
    const COLOR_FIELDS: usize = field_count!(Color);
    const POINT_HAS_X: bool = hasfield!(Point, "x");
    const POINT_HAS_Z: bool = hasfield!(Point, "z");
    const POINT_METHODS: usize = method_count!(Point);
    const COLOR_METHODS: usize = method_count!(Color);
    
    println!("=== Struct Introspection ===");
    
    // Direct introspection calls to test evaluation
    println!("Point size: {} bytes", sizeof!(Point));
    println!("Color size: {} bytes", sizeof!(Color));
    println!("Point fields: {}", field_count!(Point));
    println!("Color fields: {}", field_count!(Color));
    println!("Point has x: {}", hasfield!(Point, "x"));
    println!("Point has z: {}", hasfield!(Point, "z"));
    println!("Point methods: {}", method_count!(Point));
    println!("Color methods: {}", method_count!(Color));
    
    println!("\n✓ Introspection completed!");

    // Transpilation example - using introspection for code generation
    println!("\n=== Transpilation Demo ===");

    // Const evaluation for transpilation targets
    const POINT_SIZE_CONST: usize = sizeof!(Point);
    const COLOR_SIZE_CONST: usize = sizeof!(Color);
    const TOTAL_SIZE: usize = POINT_SIZE_CONST + COLOR_SIZE_CONST;

    // Runtime values that could be used for external code generation
    let origin = Point { x: 0.0, y: 0.0 };
    let red = Color { r: 255, g: 0, b: 0 };

    println!("Transpilation target sizes:");
    println!("  Point: {} bytes (const)", POINT_SIZE_CONST);
    println!("  Color: {} bytes (const)", COLOR_SIZE_CONST);
    println!("  Combined: {} bytes", TOTAL_SIZE);

    println!("Runtime instances:");
    println!("  Origin: ({}, {})", origin.x, origin.y);
    println!("  Red: rgb({}, {}, {})", red.r, red.g, red.b);

    println!("\n✓ Introspection enables external code generation!");
}