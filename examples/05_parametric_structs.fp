#!/usr/bin/env fp run
//! Parametric struct generation using t! macro

fn main() {
    // Parametric vector creation with dimension-based specialization
    const fn create_vector_2d<T>(x: T, y: T) -> Vector2D 
    where T: Copy + Into<f32> {
        Vector2D {
            x: x.into(),
            y: y.into(),
        }
    }
    
    const fn create_vector_3d<T>(x: T, y: T, z: T) -> Vector3D 
    where T: Copy + Into<f64> {
        Vector3D {
            x: x.into(),
            y: y.into(), 
            z: z.into(),
        }
    }
    
    // Generic vector operations using const evaluation
    const fn dot_product_2d(a: &Vector2D, b: &Vector2D) -> f32 {
        a.x * b.x + a.y * b.y
    }
    
    const fn dot_product_3d(a: &Vector3D, b: &Vector3D) -> f64 {
        a.x * b.x + a.y * b.y + a.z * b.z
    }
    
    const fn cross_product_3d(a: &Vector3D, b: &Vector3D) -> Vector3D {
        Vector3D {
            x: a.y * b.z - a.z * b.y,
            y: a.z * b.x - a.x * b.z,
            z: a.x * b.y - a.y * b.x,
        }
    }
    
    // Create specialized vector types using t! macro
    t! {
        struct Vector2D {
            x: f32,
            y: f32,
        
        fn dot(&self, other: &Self) -> f32 {
            self.x * other.x + self.y * other.y
        }
        
        fn magnitude(&self) -> f32 {
            (self.x * self.x + self.y * self.y).sqrt()
        }
    };
    
    
    t! {
        struct Vector3D {
            x: f64,
            y: f64,
            z: f64,
        
        fn dot(&self, other: &Self) -> f64 {
            self.x * other.x + self.y * other.y + self.z * other.z
        }
        
        fn cross(&self, other: &Self) -> Self {
            Self {
                x: self.y * other.z - self.z * other.y,
                y: self.z * other.x - self.x * other.z,
                z: self.x * other.y - self.y * other.x,
            }
        }
        
        fn magnitude(&self) -> f64 {
            (self.x * self.x + self.y * self.y + self.z * self.z).sqrt()
        }
    };
    
    
    // Advanced type composition with inheritance
    t! {
        struct Point3D {
            x: f64,
            y: f64, 
            z: f64,
        }
    }
    
    
    t! {
        struct ColoredPoint3D {
            // Inherit position fields (manual for now)
            x: f64,
            y: f64,
            z: f64,
            
            // Color fields
            color: u32,
            alpha: f32,
        
        fn to_rgba(&self) -> (u8, u8, u8, u8) {
            let r = ((self.color >> 24) & 0xFF) as u8;
            let g = ((self.color >> 16) & 0xFF) as u8;
            let b = ((self.color >> 8) & 0xFF) as u8;
            let a = (self.alpha * 255.0) as u8;
            (r, g, b, a)
        }
    };
    
    // Conditional type extensions using constants
    const ENABLE_ANIMATION: bool = true;
    const ENABLE_PHYSICS: bool = false;
    
    
    // Entity type with conditional features
    t! {
        struct AnimatedEntity {
            // Base colored point
            x: f64,
            y: f64,
            z: f64,
            color: u32,
            alpha: f32,
            
            // Animation fields (enabled by ENABLE_ANIMATION)
            velocity_x: f64,
            velocity_y: f64,
            velocity_z: f64,
            time_scale: f32,
        
        fn update_position(&mut self, dt: f32) {
            let scaled_dt = dt * self.time_scale;
            self.x += self.velocity_x * scaled_dt as f64;
            self.y += self.velocity_y * scaled_dt as f64;
            self.z += self.velocity_z * scaled_dt as f64;
        }
        
        fn set_velocity(&mut self, vx: f64, vy: f64, vz: f64) {
            self.velocity_x = vx;
            self.velocity_y = vy;
            self.velocity_z = vz;
        }
    };
    
    type PhysicsEntity = t! {
        // Base entity
        x: f64,
        y: f64,
        z: f64,
        color: u32,
        alpha: f32,
        velocity_x: f64,
        velocity_y: f64,
        velocity_z: f64,
        time_scale: f32,
        
        // Physics fields (enabled by ENABLE_PHYSICS)
        mass: f32,
        friction: f32,
        
        fn apply_force(&mut self, fx: f64, fy: f64, fz: f64) {
            let acceleration_x = fx / self.mass as f64;
            let acceleration_y = fy / self.mass as f64;
            let acceleration_z = fz / self.mass as f64;
            
            self.velocity_x += acceleration_x;
            self.velocity_y += acceleration_y;
            self.velocity_z += acceleration_z;
            
            // Apply friction
            let friction_factor = 1.0 - self.friction as f64;
            self.velocity_x *= friction_factor;
            self.velocity_y *= friction_factor;
            self.velocity_z *= friction_factor;
        }
    };
    
    // Compile-time analysis using introspection
    const VEC2_SIZE: usize = sizeof!(Vector2D);
    const VEC3_SIZE: usize = sizeof!(Vector3D);
    const ENTITY_SIZE: usize = sizeof!(AnimatedEntity);
    const PHYSICS_SIZE: usize = sizeof!(PhysicsEntity);
    
    const VEC2_FIELDS: usize = field_count!(Vector2D);
    const VEC3_FIELDS: usize = field_count!(Vector3D);
    const ENTITY_FIELDS: usize = field_count!(AnimatedEntity);
    
    // Feature detection
    const VEC2_HAS_DOT: bool = hasmethod!(Vector2D, "dot");
    const VEC3_HAS_CROSS: bool = hasmethod!(Vector3D, "cross");
    const ENTITY_HAS_PHYSICS: bool = hasmethod!(PhysicsEntity, "apply_force");
    
    // Performance analysis
    if VEC3_SIZE > 32 {
        compile_warning!("Vector3D might be larger than expected");
    }
    
    if ENTITY_SIZE > 128 {
        compile_warning!("AnimatedEntity is quite large");
    }
    
    // Additional const constructor functions for convenience
    const fn create_colored_point(x: f64, y: f64, z: f64, color: u32) -> ColoredPoint3D {
        ColoredPoint3D {
            x, y, z,
            color,
            alpha: 1.0,
        }
    }
    
    const fn create_colored_point_with_alpha(x: f64, y: f64, z: f64, color: u32, alpha: f32) -> ColoredPoint3D {
        ColoredPoint3D {
            x, y, z,
            color,
            alpha,
        }
    }
    
    const fn create_animated_entity(x: f64, y: f64, z: f64, color: u32) -> AnimatedEntity {
        AnimatedEntity {
            x, y, z,
            color,
            alpha: 1.0,
            velocity_x: if ENABLE_ANIMATION { 0.0 } else { 0.0 },
            velocity_y: if ENABLE_ANIMATION { 0.0 } else { 0.0 },
            velocity_z: if ENABLE_ANIMATION { 0.0 } else { 0.0 },
            time_scale: if ENABLE_ANIMATION { 1.0 } else { 1.0 },
        }
    }
    
    // Compile-time calculations using const evaluation
    const EXAMPLE_V3A: Vector3D = create_vector_3d(1.0, 2.0, 3.0);
    const EXAMPLE_V3B: Vector3D = create_vector_3d(4.0, 5.0, 6.0);
    const CONST_DOT_PRODUCT: f64 = dot_product_3d(&EXAMPLE_V3A, &EXAMPLE_V3B);
    const CONST_CROSS_PRODUCT: Vector3D = cross_product_3d(&EXAMPLE_V3A, &EXAMPLE_V3B);
    
    // Runtime usage with const constructors
    let v2 = create_vector_2d(1.0, 2.0);
    let v3 = create_vector_3d(1.0, 2.0, 3.0);
    let v3_other = create_vector_3d(4.0, 5.0, 6.0);
    
    let dot_product = v3.dot(&v3_other);
    let cross_product = v3.cross(&v3_other);
    let magnitude = v3.magnitude();
    
    // Verify const calculations match runtime
    println!("Const dot product: {:.1}, Runtime: {:.1}", CONST_DOT_PRODUCT, dot_product);
    println!("Const cross product: ({:.1}, {:.1}, {:.1})", 
             CONST_CROSS_PRODUCT.x, CONST_CROSS_PRODUCT.y, CONST_CROSS_PRODUCT.z);
    println!("Runtime cross product: ({:.1}, {:.1}, {:.1})", 
             cross_product.x, cross_product.y, cross_product.z);
    
    let colored = create_colored_point(10.0, 20.0, 30.0, 0xFF5500AA);
    let (r, g, b, a) = colored.to_rgba();
    
    let mut entity = create_animated_entity(0.0, 0.0, 0.0, 0xFF0000FF);
    if ENABLE_ANIMATION {
        entity.velocity_x = 1.0;
        entity.velocity_y = 0.5;
        entity.velocity_z = -0.2;
    };
    
    entity.update_position(0.016); // 60 FPS
    
    println!("Vector types generated using t! macro:");
    println!("  Vector2D: {} bytes, {} fields, has_dot={}", 
             VEC2_SIZE, VEC2_FIELDS, VEC2_HAS_DOT);
    println!("  Vector3D: {} bytes, {} fields, has_cross={}", 
             VEC3_SIZE, VEC3_FIELDS, VEC3_HAS_CROSS);
    println!("  AnimatedEntity: {} bytes, {} fields", 
             ENTITY_SIZE, ENTITY_FIELDS);
    println!("  PhysicsEntity: {} bytes, has_physics={}", 
             PHYSICS_SIZE, ENTITY_HAS_PHYSICS);
    
    println!("Sample computations:");
    println!("  v3 dot v3_other = {}", dot_product);
    println!("  v3 magnitude = {}", magnitude);
    println!("  cross product = ({}, {}, {})", 
             cross_product.x, cross_product.y, cross_product.z);
    println!("  colored point RGBA = ({}, {}, {}, {})", r, g, b, a);
    println!("  entity position after update = ({:.3}, {:.3}, {:.3})", 
             entity.x, entity.y, entity.z);
}