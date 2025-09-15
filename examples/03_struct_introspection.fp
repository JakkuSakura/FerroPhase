#!/usr/bin/env fp run
//! Advanced struct introspection and reflection using t! macro

fn main() {
    // Basic data structures for introspection
    t! {
        struct Point {
            x: f64,
            y: f64,
        
        fn distance_to(&self, other: &Point) -> f64 {
            let dx = self.x - other.x;
            let dy = self.y - other.y;
            (dx * dx + dy * dy).sqrt()
        }
        
        fn move_by(&mut self, dx: f64, dy: f64) {
            self.x += dx;
            self.y += dy;
        }
        
        fn origin() -> Self {
            Self { x: 0.0, y: 0.0 }
        }
    };
    
    
    t! {
        struct Color {
            r: u8,
            g: u8, 
            b: u8,
            a: u8,
        
        fn from_rgb(r: u8, g: u8, b: u8) -> Self {
            Self { r, g, b, a: 255 }
        }
        
        fn from_rgba(r: u8, g: u8, b: u8, a: u8) -> Self {
            Self { r, g, b, a }
        }
        
        fn to_hex(&self) -> String {
            format!("#{:02X}{:02X}{:02X}{:02X}", self.r, self.g, self.b, self.a)
        }
        
        fn blend(&self, other: &Color, factor: f32) -> Self {
            let factor = factor.clamp(0.0, 1.0);
            let inv_factor = 1.0 - factor;
            
            Self {
                r: ((self.r as f32 * inv_factor) + (other.r as f32 * factor)) as u8,
                g: ((self.g as f32 * inv_factor) + (other.g as f32 * factor)) as u8,
                b: ((self.b as f32 * inv_factor) + (other.b as f32 * factor)) as u8,
                a: ((self.a as f32 * inv_factor) + (other.a as f32 * factor)) as u8,
            }
        }
        
        fn brightness(&self) -> f32 {
            // Luminance calculation
            (0.299 * self.r as f32 + 0.587 * self.g as f32 + 0.114 * self.b as f32) / 255.0
        }
    };
    
    
    t! {
        struct Transform {
            position: Point,
            rotation: f64,
            scale_x: f64,
            scale_y: f64,
        
        fn identity() -> Self {
            Self {
                position: Point::origin(),
                rotation: 0.0,
                scale_x: 1.0,
                scale_y: 1.0,
            }
        }
        
        fn translate(&mut self, dx: f64, dy: f64) {
            self.position.move_by(dx, dy);
        }
        
        fn rotate(&mut self, angle: f64) {
            self.rotation += angle;
            // Normalize to [0, 2π)
            while self.rotation >= 2.0 * std::f64::consts::PI {
                self.rotation -= 2.0 * std::f64::consts::PI;
            }
            while self.rotation < 0.0 {
                self.rotation += 2.0 * std::f64::consts::PI;
            }
        }
        
        fn scale(&mut self, sx: f64, sy: f64) {
            self.scale_x *= sx;
            self.scale_y *= sy;
        }
        
        fn apply_to_point(&self, point: &Point) -> Point {
            // Apply scale, rotation, then translation
            let cos_r = self.rotation.cos();
            let sin_r = self.rotation.sin();
            
            let scaled_x = point.x * self.scale_x;
            let scaled_y = point.y * self.scale_y;
            
            let rotated_x = scaled_x * cos_r - scaled_y * sin_r;
            let rotated_y = scaled_x * sin_r + scaled_y * cos_r;
            
            Point {
                x: rotated_x + self.position.x,
                y: rotated_y + self.position.y,
            }
        }
    };
    
    
    t! {
        struct Drawable {
            position: Point,
            color: Color,
            transform: Transform,
            visible: bool,
            layer: i32,
        
        fn new(x: f64, y: f64, color: Color) -> Self {
            Self {
                position: Point { x, y },
                color,
                transform: Transform::identity(),
                visible: true,
                layer: 0,
            }
        }
        
        fn move_to(&mut self, x: f64, y: f64) {
            self.position.x = x;
            self.position.y = y;
        }
        
        fn set_color(&mut self, color: Color) {
            self.color = color;
        }
        
        fn hide(&mut self) {
            self.visible = false;
        }
        
        fn show(&mut self) {
            self.visible = true;
        }
        
        fn set_layer(&mut self, layer: i32) {
            self.layer = layer;
        }
        
        fn get_world_position(&self) -> Point {
            self.transform.apply_to_point(&self.position)
        }
        
        fn distance_to_drawable(&self, other: &Drawable) -> f64 {
            let world_pos = self.get_world_position();
            let other_world_pos = other.get_world_position();
            world_pos.distance_to(&other_world_pos)
        }
        
        fn is_in_bounds(&self, min_x: f64, min_y: f64, max_x: f64, max_y: f64) -> bool {
            let world_pos = self.get_world_position();
            world_pos.x >= min_x && world_pos.x <= max_x && 
            world_pos.y >= min_y && world_pos.y <= max_y
        }
    };
    
    
    // Advanced nested structure for complex introspection
    t! {
        struct Scene {
            objects: Vec<Drawable>,
            background_color: Color,
            ambient_light: f32,
            camera_transform: Transform,
            bounds: (f64, f64, f64, f64), // min_x, min_y, max_x, max_y
        
        fn new() -> Self {
            Self {
                objects: Vec::new(),
                background_color: Color::from_rgb(0, 0, 0),
                ambient_light: 0.1,
                camera_transform: Transform::identity(),
                bounds: (-1000.0, -1000.0, 1000.0, 1000.0),
            }
        }
        
        fn add_object(&mut self, object: Drawable) {
            self.objects.push(object);
        }
        
        fn remove_object(&mut self, index: usize) -> Option<Drawable> {
            if index < self.objects.len() {
                Some(self.objects.remove(index))
            } else {
                None
            }
        }
        
        fn count_visible_objects(&self) -> usize {
            self.objects.iter().filter(|obj| obj.visible).count()
        }
        
        fn objects_in_layer(&self, layer: i32) -> Vec<&Drawable> {
            self.objects.iter().filter(|obj| obj.layer == layer).collect()
        }
        
        fn objects_in_bounds(&self) -> Vec<&Drawable> {
            let (min_x, min_y, max_x, max_y) = self.bounds;
            self.objects.iter()
                .filter(|obj| obj.is_in_bounds(min_x, min_y, max_x, max_y))
                .collect()
        }
        
        fn set_background(&mut self, color: Color) {
            self.background_color = color;
        }
        
        fn move_camera(&mut self, dx: f64, dy: f64) {
            self.camera_transform.translate(dx, dy);
        }
        
        fn zoom_camera(&mut self, factor: f64) {
            self.camera_transform.scale(factor, factor);
        }
        
        fn get_statistics(&self) -> SceneStats {
            let visible_count = self.count_visible_objects();
            let total_count = self.objects.len();
            let in_bounds_count = self.objects_in_bounds().len();
            
            let mut layer_counts = std::collections::HashMap::new();
            for obj in &self.objects {
                *layer_counts.entry(obj.layer).or_insert(0) += 1;
            }
            
            SceneStats {
                total_objects: total_count,
                visible_objects: visible_count,
                objects_in_bounds: in_bounds_count,
                layer_distribution: layer_counts,
                background_brightness: self.background_color.brightness(),
                camera_position: self.camera_transform.position,
            }
        }
    };
    
    // Complete struct introspection using FerroPhase capabilities
    const POINT_SIZE: usize = sizeof!(Point);
    const COLOR_SIZE: usize = sizeof!(Color);
    const TRANSFORM_SIZE: usize = sizeof!(Transform);
    const DRAWABLE_SIZE: usize = sizeof!(Drawable);
    const SCENE_SIZE: usize = sizeof!(Scene);
    
    const POINT_FIELDS: usize = field_count!(Point);
    const COLOR_FIELDS: usize = field_count!(Color);
    const TRANSFORM_FIELDS: usize = field_count!(Transform);
    const DRAWABLE_FIELDS: usize = field_count!(Drawable);
    const SCENE_FIELDS: usize = field_count!(Scene);
    
    // Field existence checks
    const POINT_HAS_X: bool = hasfield!(Point, "x");
    const POINT_HAS_Z: bool = hasfield!(Point, "z");
    const COLOR_HAS_ALPHA: bool = hasfield!(Color, "a");
    const DRAWABLE_HAS_VISIBLE: bool = hasfield!(Drawable, "visible");
    const SCENE_HAS_CAMERA: bool = hasfield!(Scene, "camera_transform");
    
    // Method introspection
    const POINT_METHODS: usize = method_count!(Point);
    const COLOR_METHODS: usize = method_count!(Color);
    const TRANSFORM_METHODS: usize = method_count!(Transform);
    const DRAWABLE_METHODS: usize = method_count!(Drawable);
    const SCENE_METHODS: usize = method_count!(Scene);
    
    const POINT_HAS_DISTANCE: bool = hasmethod!(Point, "distance_to");
    const COLOR_HAS_BLEND: bool = hasmethod!(Color, "blend");
    const TRANSFORM_HAS_ROTATE: bool = hasmethod!(Transform, "rotate");
    const DRAWABLE_HAS_WORLD_POS: bool = hasmethod!(Drawable, "get_world_position");
    const SCENE_HAS_STATS: bool = hasmethod!(Scene, "get_statistics");
    
    // Nested field analysis
    const DRAWABLE_HAS_POSITION: bool = hasfield!(Drawable, "position");
    const DRAWABLE_HAS_COLOR: bool = hasfield!(Drawable, "color");
    const SCENE_HAS_OBJECTS: bool = hasfield!(Scene, "objects");
    
    // Validation at compile time
    const SIZE_CALCULATION_OK: bool = DRAWABLE_SIZE >= (POINT_SIZE + COLOR_SIZE + TRANSFORM_SIZE);
    const REASONABLE_SIZES: bool = POINT_SIZE <= 32 && COLOR_SIZE <= 16 && TRANSFORM_SIZE <= 64;
    
    if DRAWABLE_SIZE > 512 {
        compile_warning!("Drawable struct is quite large");
    }
    
    if SCENE_SIZE > 2048 {
        compile_warning!("Scene struct is very large");
    }
    
    if !SIZE_CALCULATION_OK {
        compile_warning!("Drawable size calculation seems incorrect");
    }
    
    if !REASONABLE_SIZES {
        compile_warning!("Some basic structs are unexpectedly large");
    }
    
    // Memory layout and efficiency analysis
    const TOTAL_BASIC_SIZE: usize = POINT_SIZE + COLOR_SIZE + TRANSFORM_SIZE;
    const MEMORY_EFFICIENCY: f64 = TOTAL_BASIC_SIZE as f64 / DRAWABLE_SIZE as f64 * 100.0;
    const TOTAL_METHODS: usize = POINT_METHODS + COLOR_METHODS + TRANSFORM_METHODS + 
                                DRAWABLE_METHODS + SCENE_METHODS;
    
    // Runtime demonstration
    let mut scene = Scene::new();
    scene.set_background(Color::from_rgb(25, 25, 50));
    
    // Create some drawable objects
    let mut red_circle = Drawable::new(100.0, 100.0, Color::from_rgb(255, 0, 0));
    red_circle.set_layer(1);
    
    let mut blue_square = Drawable::new(200.0, 150.0, Color::from_rgb(0, 0, 255));
    blue_square.set_layer(2);
    blue_square.transform.rotate(std::f64::consts::PI / 4.0); // 45 degrees
    
    let mut green_triangle = Drawable::new(-50.0, 300.0, Color::from_rgb(0, 255, 0));
    green_triangle.set_layer(1);
    green_triangle.hide(); // Not visible
    
    scene.add_object(red_circle);
    scene.add_object(blue_square);
    scene.add_object(green_triangle);
    
    // Move camera
    scene.move_camera(-10.0, -5.0);
    scene.zoom_camera(1.2);
    
    // Color operations
    let purple = Color::from_rgb(255, 0, 0).blend(&Color::from_rgb(0, 0, 255), 0.5);
    let yellow = Color::from_rgb(255, 255, 0);
    
    // Transform operations
    let mut transform = Transform::identity();
    transform.translate(50.0, 25.0);
    transform.rotate(std::f64::consts::PI / 6.0); // 30 degrees
    transform.scale(1.5, 1.5);
    
    let test_point = Point { x: 10.0, y: 20.0 };
    let transformed_point = transform.apply_to_point(&test_point);
    
    // Scene analysis
    let stats = scene.get_statistics();
    
    println!("=== Struct Introspection Analysis ===\n");
    
    println!("Structure Sizes:");
    println!("  Point: {} bytes, {} fields, {} methods", 
             POINT_SIZE, POINT_FIELDS, POINT_METHODS);
    println!("  Color: {} bytes, {} fields, {} methods", 
             COLOR_SIZE, COLOR_FIELDS, COLOR_METHODS);
    println!("  Transform: {} bytes, {} fields, {} methods", 
             TRANSFORM_SIZE, TRANSFORM_FIELDS, TRANSFORM_METHODS);
    println!("  Drawable: {} bytes, {} fields, {} methods", 
             DRAWABLE_SIZE, DRAWABLE_FIELDS, DRAWABLE_METHODS);
    println!("  Scene: {} bytes, {} fields, {} methods", 
             SCENE_SIZE, SCENE_FIELDS, SCENE_METHODS);
    
    println!("\nField Existence Analysis:");
    println!("  Point has x: {}, has z: {}", POINT_HAS_X, POINT_HAS_Z);
    println!("  Color has alpha: {}", COLOR_HAS_ALPHA);
    println!("  Drawable has visible: {}, has position: {}, has color: {}", 
             DRAWABLE_HAS_VISIBLE, DRAWABLE_HAS_POSITION, DRAWABLE_HAS_COLOR);
    println!("  Scene has camera: {}, has objects: {}", SCENE_HAS_CAMERA, SCENE_HAS_OBJECTS);
    
    println!("\nMethod Capability Analysis:");
    println!("  Point can calculate distance: {}", POINT_HAS_DISTANCE);
    println!("  Color can blend: {}", COLOR_HAS_BLEND);
    println!("  Transform can rotate: {}", TRANSFORM_HAS_ROTATE);
    println!("  Drawable can get world position: {}", DRAWABLE_HAS_WORLD_POS);
    println!("  Scene can generate statistics: {}", SCENE_HAS_STATS);
    
    println!("\nMemory Layout Analysis:");
    println!("  Size calculation valid: {}", SIZE_CALCULATION_OK);
    println!("  Reasonable sizes: {}", REASONABLE_SIZES);
    println!("  Memory efficiency: {:.1}%", MEMORY_EFFICIENCY);
    println!("  Total methods across all types: {}", TOTAL_METHODS);
    
    println!("\nRuntime Demonstration:");
    println!("  Scene objects: {}", stats.total_objects);
    println!("  Visible objects: {}", stats.visible_objects);
    println!("  Objects in bounds: {}", stats.objects_in_bounds);
    println!("  Background brightness: {:.2}", stats.background_brightness);
    println!("  Camera position: ({:.1}, {:.1})", 
             stats.camera_position.x, stats.camera_position.y);
    
    println!("  Purple color: {}", purple.to_hex());
    println!("  Yellow brightness: {:.2}", yellow.brightness());
    println!("  Transformed point: ({:.2}, {:.2})", 
             transformed_point.x, transformed_point.y);
    
    // Layer analysis
    println!("  Layer distribution:");
    for (layer, count) in &stats.layer_distribution {
        println!("    Layer {}: {} objects", layer, count);
    }
    
    println!("\n✓ Struct introspection completed successfully!");
}

// Helper struct for scene statistics
struct SceneStats {
    total_objects: usize,
    visible_objects: usize,
    objects_in_bounds: usize,
    layer_distribution: std::collections::HashMap<i32, usize>,
    background_brightness: f32,
    camera_position: Point,
}