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
}