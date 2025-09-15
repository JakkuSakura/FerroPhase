#!/usr/bin/env fp run
//! Dynamic struct creation using conditional fields and type composition

fn main() {
    // Configuration flags
    const ENABLE_3D: bool = true;
    const ENABLE_COLOR: bool = true;
    const ENABLE_TEXTURE: bool = false;
    const ENABLE_LIGHTING: bool = true;
    
    // Dynamic vertex type creation using declarative syntax with @ macros
    type Vertex = {
        // Base position fields (always present)
        x: f32,
        y: f32,
        
        // Optional 3D coordinate
        if ENABLE_3D {
            z: f32,
        }
        
        // Optional color components
        if ENABLE_COLOR {
            r: f32,
            g: f32,
            b: f32,
            a: f32,
        }
        
        // Optional texture coordinates
        if ENABLE_TEXTURE {
            u: f32,
            v: f32,
        }
        
        // Optional lighting normal
        if ENABLE_LIGHTING {
            nx: f32,
            ny: f32,
            nz: f32,
        }
    };
    
    // Alternative performance-optimized version
    const PERFORMANCE_MODE: &str = "high";
    
    type OptimizedVertex = {
        ...Vertex,  // Inherit all fields from Vertex
        
        if PERFORMANCE_MODE == "high" && sizeof!(Vertex) <= 64 {
            __inline_flag: bool,
        }
        
        if PERFORMANCE_MODE == "memory" {
            packed,  // Attribute for tight packing
        }
    };
    
    // Analyze the generated types
    const VERTEX_SIZE: usize = sizeof!(Vertex);
    const VERTEX_FIELDS: usize = field_count!(Vertex);
    const HAS_Z_COORD: bool = hasfield!(Vertex, "z");
    const HAS_TEXTURE: bool = hasfield!(Vertex, "u");
    
    const OPTIMIZED_SIZE: usize = sizeof!(OptimizedVertex);
    const HAS_INLINE_FLAG: bool = hasfield!(OptimizedVertex, "__inline_flag");
    
    // Size breakdown analysis
    const BASE_SIZE: usize = 2 * 4; // x, y
    const Z_SIZE: usize = if ENABLE_3D { 4 } else { 0 };
    const COLOR_SIZE: usize = if ENABLE_COLOR { 4 * 4 } else { 0 };
    const TEXTURE_SIZE: usize = if ENABLE_TEXTURE { 2 * 4 } else { 0 };
    const LIGHTING_SIZE: usize = if ENABLE_LIGHTING { 3 * 4 } else { 0 };
    const EXPECTED_SIZE: usize = BASE_SIZE + Z_SIZE + COLOR_SIZE + TEXTURE_SIZE + LIGHTING_SIZE;
    
    // Compile-time validation
    if VERTEX_SIZE != EXPECTED_SIZE {
        compile_error!("Vertex size calculation mismatch!");
    }
    
    if VERTEX_SIZE > 256 {
        compile_warning!("Vertex struct is getting quite large");
    }
    
    // Generate constructor macro
    macro_rules! new_vertex {
        ($x:expr, $y:expr $(, $z:expr)? $(, r: $r:expr, g: $g:expr, b: $b:expr, a: $a:expr)? $(, nx: $nx:expr, ny: $ny:expr, nz: $nz:expr)?) => {
            construct_vertex!($x, $y $(, $z)? $(, $r, $g, $b, $a)? $(, $nx, $ny, $nz)?)
        };
    }
    
    println!("Dynamic vertex configuration:");
    println!("  3D: {}, Color: {}, Texture: {}, Lighting: {}", 
             ENABLE_3D, ENABLE_COLOR, ENABLE_TEXTURE, ENABLE_LIGHTING);
    println!("Generated vertex: {} bytes, {} fields", VERTEX_SIZE, VERTEX_FIELDS);
    println!("Features: has_z={}, has_texture={}", HAS_Z_COORD, HAS_TEXTURE);
    println!("Size breakdown: base={}, 3d={}, color={}, texture={}, lighting={}",
             BASE_SIZE, Z_SIZE, COLOR_SIZE, TEXTURE_SIZE, LIGHTING_SIZE);
    println!("Optimized vertex: {} bytes, has_inline={}", OPTIMIZED_SIZE, HAS_INLINE_FLAG);
}