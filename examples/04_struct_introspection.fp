#!/usr/bin/env fp run
//! Struct introspection demonstration

fn main() {
    println!("📘 Tutorial: 04_struct_introspection.fp");
    println!("🧭 Focus: Struct introspection demonstration");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
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
    const POINT_FIELDS: i64 = type(Point).fields.len();
    const COLOR_FIELDS: i64 = type(Color).fields.len();
    const POINT_HAS_X: bool = type(Point).fields.contains("x");
    const POINT_HAS_Z: bool = type(Point).fields.contains("z");
    const POINT_METHODS: i64 = type(Point).methods.len();
    const COLOR_METHODS: i64 = type(Color).methods.len();
    
    println!("=== Struct Introspection ===");
    
    // Direct introspection calls to test evaluation
    println!("Point size: {} bytes", sizeof!(Point));
    println!("Color size: {} bytes", sizeof!(Color));
    println!("Point fields: {}", type(Point).fields.len());
    println!("Color fields: {}", type(Color).fields.len());
    println!("Point has x: {}", type(Point).fields.contains("x"));
    println!("Point has z: {}", type(Point).fields.contains("z"));
    println!("Point methods: {}", type(Point).methods.len());
    println!("Color methods: {}", type(Color).methods.len());
    
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
