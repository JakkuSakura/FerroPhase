#!/usr/bin/env fp run
//! Generic container specialization using t! macro

fn main() {
    // Smart container for i32 with inline optimizations
    t! {
        struct I32Container {
            data: Vec<i32>,
            len: usize,
            capacity: usize,
        
        // Small type optimization for i32
        inline_buffer: [i32; 16],
        small_item_flag: bool,
        
        // Hash-based optimizations (i32 implements Hash)
        hash_cache: u64,
        dedup_mode: bool,
        
        // Ordering optimizations (i32 implements Ord)
        is_sorted: bool,
        
        // SIMD optimizations for 32-bit numeric types
        simd_enabled: bool,
        
        fn try_inline_push(&mut self, item: i32) -> bool {
            if self.len < 16 && self.small_item_flag {
                self.inline_buffer[self.len] = item;
                self.len += 1;
                true
            } else {
                false
            }
        }
        
        fn hash_all(&self) -> u64 {
            use std::collections::hash_map::DefaultHasher;
            use std::hash::{Hash, Hasher};
            let mut hasher = DefaultHasher::new();
            for item in &self.data {
                item.hash(&mut hasher);
            }
            hasher.finish()
        }
        
        fn binary_search(&self, item: &i32) -> Option<usize> {
            if self.is_sorted {
                self.data.binary_search(item).ok()
            } else {
                None
            }
        }
        
        fn sum_simd(&self) -> i32 {
            if self.simd_enabled {
                // Simulated SIMD sum
                self.data.iter().sum()
            } else {
                self.data.iter().sum()
            }
        }
        
        fn deduplicate(&mut self) {
            if self.dedup_mode {
                self.data.sort();
                self.data.dedup();
                self.len = self.data.len();
                self.is_sorted = true;
            }
        }
        
        fn sort_optimized(&mut self) {
            self.data.sort();
            self.is_sorted = true;
        }
    };
    
    
    // String container with different optimizations
    t! {
        struct StringContainer {
            data: Vec<String>,
            len: usize,
            capacity: usize,
        
        // No inline buffer for String (too large)
        // Hash-based optimizations (String implements Hash)
        hash_cache: u64,
        dedup_mode: bool,
        
        // String-specific optimizations
        total_capacity: usize,
        intern_table: std::collections::HashMap<String, u32>,
        
        // Clone optimizations (String implements Clone but not Copy)
        clone_strategy: CloneStrategy,
        
        fn hash_all(&self) -> u64 {
            use std::collections::hash_map::DefaultHasher;
            use std::hash::{Hash, Hasher};
            let mut hasher = DefaultHasher::new();
            for item in &self.data {
                item.hash(&mut hasher);
            }
            hasher.finish()
        }
        
        fn deep_clone(&self) -> Self {
            Self {
                data: self.data.clone(),
                len: self.len,
                capacity: self.capacity,
                hash_cache: 0,
                dedup_mode: self.dedup_mode,
                total_capacity: self.total_capacity,
                intern_table: self.intern_table.clone(),
                clone_strategy: self.clone_strategy,
            }
        }
        
        fn intern_strings(&mut self) {
            let mut next_id = 0u32;
            for s in &self.data {
                if !self.intern_table.contains_key(s) {
                    self.intern_table.insert(s.clone(), next_id);
                    next_id += 1;
                }
            }
        }
        
        fn total_string_size(&self) -> usize {
            self.data.iter().map(|s| s.len()).sum()
        }
        
        fn clone_range(&self, start: usize, end: usize) -> Vec<String> {
            self.data[start..end].to_vec()
        }
    };
    
    
    // f32 container with SIMD optimizations
    t! {
        struct F32Container {
            data: Vec<f32>,
            len: usize,
            capacity: usize,
        
        // SIMD optimizations for 32-bit numeric types
        simd_enabled: bool,
        
        // Hash optimizations (f32 doesn't implement Hash by default)
        // No hash_cache field
        
        fn sum_simd(&self) -> f32 {
            if self.simd_enabled {
                // Simulated SIMD sum for f32
                self.data.iter().sum()
            } else {
                self.data.iter().sum()
            }
        }
        
        fn map_simd<F>(&self, f: F) -> Vec<f32> 
        where F: Fn(f32) -> f32 {
            if self.simd_enabled {
                self.data.iter().map(|&x| f(x)).collect()
            } else {
                self.data.iter().map(|&x| f(x)).collect()
            }
        }
        
        fn parallel_reduce<F>(&self, init: f32, f: F) -> f32
        where F: Fn(f32, f32) -> f32 {
            self.data.iter().fold(init, |acc, &x| f(acc, x))
        }
    };
    
    
    // Large struct container (minimal optimizations)
    t! {
        struct LargeStructContainer {
            data: Vec<LargeStruct>,
            len: usize,
            capacity: usize,
        
        // No inline buffer (struct too large)
        // No SIMD optimizations
        // Basic functionality only
        
        fn optimize_layout(&mut self) {
            // Placeholder for layout optimization
            self.capacity = self.data.capacity();
        }
    };
    
    
    // Cached container variant using composition
    t! {
        struct CachedI32Container {
            // Base container fields
            data: Vec<i32>,
            len: usize,
            capacity: usize,
            inline_buffer: [i32; 16],
            small_item_flag: bool,
            hash_cache: u64,
            dedup_mode: bool,
            is_sorted: bool,
        simd_enabled: bool,
        
        // Caching extensions
        cache: std::collections::HashMap<usize, i32>,
        cache_hits: u64,
        cache_misses: u64,
        
        fn get_cached(&mut self, index: usize) -> Option<&i32> {
            if let Some(value) = self.cache.get(&index) {
                self.cache_hits += 1;
                Some(value)
            } else if let Some(value) = self.data.get(index) {
                self.cache.insert(index, *value);
                self.cache_misses += 1;
                self.data.get(index)
            } else {
                None
            }
        }
        
        fn cache_hit_rate(&self) -> f64 {
            let total = self.cache_hits + self.cache_misses;
            if total > 0 {
                self.cache_hits as f64 / total as f64
            } else {
                0.0
            }
        }
    };
    
    // Compile-time analysis
    const I32_SIZE: usize = sizeof!(I32Container);
    const STRING_SIZE: usize = sizeof!(StringContainer);
    const F32_SIZE: usize = sizeof!(F32Container);
    const LARGE_SIZE: usize = sizeof!(LargeStructContainer);
    const CACHED_SIZE: usize = sizeof!(CachedI32Container);
    
    // Feature detection
    const I32_HAS_INLINE: bool = hasfield!(I32Container, "inline_buffer");
    const I32_HAS_SIMD: bool = hasmethod!(I32Container, "sum_simd");
    const I32_HAS_HASH: bool = hasfield!(I32Container, "hash_cache");
    
    const STRING_HAS_INTERN: bool = hasmethod!(StringContainer, "intern_strings");
    const STRING_HAS_CLONE: bool = hasmethod!(StringContainer, "deep_clone");
    const STRING_HAS_INLINE: bool = hasfield!(StringContainer, "inline_buffer");
    
    const F32_HAS_SIMD: bool = hasmethod!(F32Container, "sum_simd");
    const F32_HAS_PARALLEL: bool = hasmethod!(F32Container, "parallel_reduce");
    
    const CACHED_HAS_CACHE: bool = hasfield!(CachedI32Container, "cache");
    
    // Method counting
    const I32_METHODS: usize = method_count!(I32Container);
    const STRING_METHODS: usize = method_count!(StringContainer);
    const F32_METHODS: usize = method_count!(F32Container);
    
    // Performance warnings
    if I32_SIZE > 512 {
        compile_warning!("I32Container is quite large");
    }
    
    if STRING_SIZE > 1024 {
        compile_warning!("StringContainer may have poor cache performance");
    }
    
    const TOTAL_METHODS: usize = I32_METHODS + STRING_METHODS + F32_METHODS;
    if TOTAL_METHODS > 50 {
        compile_warning!("High total method count across containers");
    }
    
    // Runtime demonstration
    let mut int_container = I32Container {
        data: vec![1, 2, 3, 4, 5],
        len: 5,
        capacity: 10,
        inline_buffer: [0; 16],
        small_item_flag: true,
        hash_cache: 0,
        dedup_mode: false,
        is_sorted: false,
        simd_enabled: true,
    };
    
    let mut string_container = StringContainer {
        data: vec!["hello".to_string(), "world".to_string(), "ferrophase".to_string()],
        len: 3,
        capacity: 10,
        hash_cache: 0,
        dedup_mode: false,
        total_capacity: 0,
        intern_table: std::collections::HashMap::new(),
        clone_strategy: CloneStrategy::Deep,
    };
    
    let f32_container = F32Container {
        data: vec![1.0, 2.5, 3.7, 4.2],
        len: 4,
        capacity: 10,
        simd_enabled: true,
    };
    
    // Test operations
    let _ = int_container.try_inline_push(42);
    let hash_value = int_container.hash_all();
    let sum_result = int_container.sum_simd();
    
    string_container.intern_strings();
    let string_size = string_container.total_string_size();
    let cloned_strings = string_container.deep_clone();
    
    let f32_sum = f32_container.sum_simd();
    let doubled = f32_container.map_simd(|x| x * 2.0);
    
    println!("Smart Container Analysis using t! macro:");
    println!("  I32Container: {} bytes, {} methods", I32_SIZE, I32_METHODS);
    println!("    Features: inline={}, simd={}, hash={}", 
             I32_HAS_INLINE, I32_HAS_SIMD, I32_HAS_HASH);
    println!("    Runtime: hash={:X}, sum={}", hash_value, sum_result);
    
    println!("  StringContainer: {} bytes, {} methods", STRING_SIZE, STRING_METHODS);
    println!("    Features: intern={}, clone={}, inline={}", 
             STRING_HAS_INTERN, STRING_HAS_CLONE, STRING_HAS_INLINE);
    println!("    Runtime: total_size={}, interned={}", 
             string_size, string_container.intern_table.len());
    
    println!("  F32Container: {} bytes, {} methods", F32_SIZE, F32_METHODS);
    println!("    Features: simd={}, parallel={}", F32_HAS_SIMD, F32_HAS_PARALLEL);
    println!("    Runtime: sum={}, doubled={:?}", f32_sum, doubled);
    
    println!("  CachedI32Container: {} bytes, has_cache={}", 
             CACHED_SIZE, CACHED_HAS_CACHE);
    
    println!("Performance analysis:");
    println!("  Total methods across all containers: {}", TOTAL_METHODS);
    println!("  Large struct container: {} bytes", LARGE_SIZE);
    
    // Demonstrate type-based optimizations
    println!("Automatic optimizations applied:");
    if I32_HAS_INLINE {
        println!("✓ i32: Small type → inline buffer optimization");
    }
    if I32_HAS_SIMD {
        println!("✓ i32: 32-bit numeric → SIMD acceleration");
    }
    if STRING_HAS_INTERN {
        println!("✓ String: String type → interning optimization");
    }
    if !STRING_HAS_INLINE {
        println!("✓ String: Large type → no inline buffer (memory efficient)");
    }
    if F32_HAS_PARALLEL {
        println!("✓ f32: Numeric type → parallel operations");
    }
}

// Helper types
struct LargeStruct {
    data: [u8; 256],
}

#[derive(Clone, Copy)]
enum CloneStrategy {
    Shallow,
    Deep,
    CopyOnWrite,
}