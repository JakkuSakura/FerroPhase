#!/usr/bin/env fp run
//! Struct introspection and reflection using compile-time macros

fn main() {
    struct Point { x: f64, y: f64 }
    struct Color { r: u8, g: u8, b: u8, a: u8 }
    struct Drawable { position: Point, color: Color, visible: bool }
    
    // Complete struct introspection using macros
    const POINT_SIZE: usize = sizeof!(Point);
    const COLOR_SIZE: usize = sizeof!(Color);
    const DRAWABLE_SIZE: usize = sizeof!(Drawable);
    
    const POINT_FIELDS: usize = field_count!(Point);
    const COLOR_FIELDS: usize = field_count!(Color);
    const DRAWABLE_FIELDS: usize = field_count!(Drawable);
    
    // Field existence checks
    const POINT_HAS_X: bool = hasfield!(Point, "x");
    const POINT_HAS_Z: bool = hasfield!(Point, "z");
    const DRAWABLE_HAS_VISIBLE: bool = hasfield!(Drawable, "visible");
    
    // Field type introspection
    const X_FIELD_TYPE: Type = field_type!(Point, "x");
    const COLOR_R_TYPE: Type = field_type!(Color, "r");
    
    // Alignment and padding analysis
    const POINT_ALIGNMENT: usize = alignof!(Point);
    const DRAWABLE_ALIGNMENT: usize = alignof!(Drawable);
    
    // Get complete field information
    const POINT_FIELDS_INFO: FieldDescriptor[] = reflect_fields!(Point);
    const DRAWABLE_FIELDS_INFO: FieldDescriptor[] = reflect_fields!(Drawable);
    
    // Validation at compile time
    const SIZE_CALCULATION_OK: bool = DRAWABLE_SIZE >= (POINT_SIZE + COLOR_SIZE + 1);
    
    if DRAWABLE_SIZE > 1024 {
        compile_warning!("Drawable struct is quite large");
    }
    
    // Memory layout analysis
    const MEMORY_EFFICIENCY: f64 = (POINT_SIZE + COLOR_SIZE + 1) as f64 / DRAWABLE_SIZE as f64 * 100.0;
    
    let drawable = Drawable {
        position: Point { x: 1.0, y: 2.0 },
        color: Color { r: 255, g: 128, b: 64, a: 255 },
        visible: true,
    };
    
    println!("Struct sizes: Point={}B, Color={}B, Drawable={}B", 
             POINT_SIZE, COLOR_SIZE, DRAWABLE_SIZE);
    println!("Field counts: Point={}, Color={}, Drawable={}", 
             POINT_FIELDS, COLOR_FIELDS, DRAWABLE_FIELDS);
    println!("Point has x: {}, has z: {}, Drawable has visible: {}", 
             POINT_HAS_X, POINT_HAS_Z, DRAWABLE_HAS_VISIBLE);
    println!("Alignments: Point={}B, Drawable={}B", POINT_ALIGNMENT, DRAWABLE_ALIGNMENT);
    println!("Memory efficiency: {:.1}%", MEMORY_EFFICIENCY);
    
    println!("Instance: pos=({}, {}), color=rgba({},{},{},{}), visible={}", 
             drawable.position.x, drawable.position.y,
             drawable.color.r, drawable.color.g, drawable.color.b, drawable.color.a,
             drawable.visible);
}