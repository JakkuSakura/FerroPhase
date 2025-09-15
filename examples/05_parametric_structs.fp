#!/usr/bin/env fp run
//! Parametric struct generation with @ macros and declarative syntax

fn main() {
    // Parametric vector creation using const fn with @ macros
    const fn create_vector_type<const DIM: usize, T>() -> Type {
        type Vector = {
            // Create fields based on dimension parameter
            match DIM {
                1 => { x: T },
                2 => { x: T, y: T },
                3 => { x: T, y: T, z: T },
                4 => { x: T, y: T, z: T, w: T },
                _ => {
                    // Dynamic field generation for higher dimensions
                    for i in 0..DIM {
                        field!(format!("dim_{}", i)): T,
                    }
                }
            }
            
            // Add computed methods based on dimension using @ macros
            if DIM >= 2 {
                fn dot(&self, other: &Self) -> T {
                    generate_dot_product!(DIM, T)
                }
            }
            
            if DIM == 3 {
                fn cross(&self, other: &Self) -> Self {
                    generate_cross_product!(T)
                }
            }
            
            if DIM >= 4 {
                fn magnitude(&self) -> f64 {
                    generate_magnitude!(DIM, T)
                }
            }
        };
        
        Vector
    }
    
    // Create specific vector types using @ macros
    type Vector2D = create_vector_type<2, f32>();
    type Vector3D = create_vector_type<3, f64>();
    type Vector4D = create_vector_type<4, i32>();
    type Vector8D = create_vector_type<8, f64>();
    
    // Even cleaner inline type definitions
    type Point2D = { x: f64, y: f64 };
    type Point3D = { x: f64, y: f64, z: f64 };
    
    type ColoredPoint3D = {
        ...Point3D,  // Inherit position fields
        color: u32,
        alpha: f32,
    };
    
    // Conditional type composition
    const ENABLE_ANIMATION: bool = true;
    
    type AnimatedPoint = {
        ...ColoredPoint3D,
        
        if ENABLE_ANIMATION {
            velocity: Point3D,
            acceleration: Point3D,
            time_scale: f32,
        }
    };
    
    // Analyze generated types using introspection
    const VEC2_SIZE: usize = sizeof!(Vector2D);
    const VEC3_SIZE: usize = sizeof!(Vector3D);
    const VEC4_SIZE: usize = sizeof!(Vector4D);
    const VEC8_SIZE: usize = sizeof!(Vector8D);
    
    const VEC2_FIELDS: usize = field_count!(Vector2D);
    const VEC3_FIELDS: usize = field_count!(Vector3D);
    const VEC8_FIELDS: usize = field_count!(Vector8D);
    
    // Field existence checks
    const VEC3_HAS_Z: bool = hasfield!(Vector3D, "z");
    const VEC4_HAS_W: bool = hasfield!(Vector4D, "w");
    const VEC8_HAS_DIM7: bool = hasfield!(Vector8D, "dim_7");
    
    // Method availability checks
    const VEC2_HAS_DOT: bool = hasmethod!(Vector2D, "dot");
    const VEC3_HAS_CROSS: bool = hasmethod!(Vector3D, "cross");
    const VEC4_HAS_MAGNITUDE: bool = hasmethod!(Vector4D, "magnitude");
    
    const ANIMATED_SIZE: usize = sizeof!(AnimatedPoint);
    const HAS_VELOCITY: bool = hasfield!(AnimatedPoint, "velocity");
    
    // Compile-time validation
    if VEC2_SIZE != 8 { // 2 * f32
        compile_error!("Vector2D size incorrect");
    }
    
    if VEC3_SIZE != 24 { // 3 * f64
        compile_error!("Vector3D size incorrect");
    }
    
    // Performance analysis
    const REGISTER_FRIENDLY_TYPES: Type[] = [Vector2D, Vector4D];
    
    for vec_type in REGISTER_FRIENDLY_TYPES {
        if sizeof!(vec_type) > 32 {
            compile_warning!(format!("Type {} might be too large for registers", type_name!(vec_type)));
        }
    }
    
    // Generate specialized constructors
    macro_rules! vec2d {
        ($x:expr, $y:expr) => {
            construct_vector!(Vector2D, x: $x, y: $y)
        };
    }
    
    macro_rules! vec3d {
        ($x:expr, $y:expr, $z:expr) => {
            construct_vector!(Vector3D, x: $x, y: $y, z: $z)
        };
    }
    
    println!("Generated vector types:");
    println!("  Vector2D<f32>: {} bytes, {} fields, has_dot={}", 
             VEC2_SIZE, VEC2_FIELDS, VEC2_HAS_DOT);
    println!("  Vector3D<f64>: {} bytes, {} fields, has_z={}, has_cross={}", 
             VEC3_SIZE, VEC3_FIELDS, VEC3_HAS_Z, VEC3_HAS_CROSS);
    println!("  Vector4D<i32>: {} bytes, has_w={}, has_magnitude={}", 
             VEC4_SIZE, VEC4_HAS_W, VEC4_HAS_MAGNITUDE);
    println!("  Vector8D<f64>: {} bytes, {} fields, has_dim7={}", 
             VEC8_SIZE, VEC8_FIELDS, VEC8_HAS_DIM7);
    println!("  AnimatedPoint: {} bytes, has_velocity={}", 
             ANIMATED_SIZE, HAS_VELOCITY);
}