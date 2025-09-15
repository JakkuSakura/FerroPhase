#!/usr/bin/env fp run
//! Generic container specialization with declarative type syntax

fn main() {
    // Generic container creation with type-based specialization
    const fn create_container<T>() -> Type {
        type Container = {
            // Core container fields
            data: Vec<T>,
            len: usize,
            capacity: usize,
            
            // Type-specific optimizations
            if is_copy!(T) && sizeof!(T) <= 8 {
                inline_buffer: [T; 16],
                small_item_flag: bool,
            }
            
            if sizeof!(T) <= 4 {
                tiny_item_optimization: bool,
            }
            
            if implements!(T, Hash) {
                hash_cache: u64,
                
                fn hash_all(&self) -> u64 {
                    generate_hash_aggregator!(T)
                }
            }
            
            if implements!(T, Ord) {
                is_sorted: bool,
                
                fn binary_search(&self, item: &T) -> Option<usize> {
                    generate_binary_search!(T)
                }
                
                fn sort(&mut self) {
                    generate_sort_optimized!(T)
                }
            }
            
            if implements!(T, Clone) && !implements!(T, Copy) {
                fn deep_clone(&self) -> Self {
                    generate_deep_clone!(T)
                }
            }
            
            // Add SIMD optimizations for numeric types
            if is_numeric!(T) && sizeof!(T) == 4 {
                fn sum_simd(&self) -> T {
                    generate_simd_sum!(T)
                }
            }
        };
        
        Container
    }
    
    // Create specialized containers using the declarative syntax
    type I32Container = create_container<i32>();
    type StringContainer = create_container<String>();
    type U64Container = create_container<u64>();
    type F32Container = create_container<f32>();
    
    // Simple inline type definitions
    type SmallData = { id: u16, value: i32 };
    type LargeData = { header: [u8; 64], payload: Vec<u8>, metadata: String };
    
    // Conditional type composition
    const ENABLE_CACHING: bool = true;
    
    type CachedContainer<T> = {
        ...create_container<T>(),  // Inherit all container features
        
        if ENABLE_CACHING {
            cache: HashMap<String, T>,
            cache_hits: u64,
            cache_misses: u64,
        }
    };
    
    // Analyze specializations
    const I32_SIZE: usize = sizeof!(I32Container);
    const STRING_SIZE: usize = sizeof!(StringContainer);
    const U64_SIZE: usize = sizeof!(U64Container);
    const F32_SIZE: usize = sizeof!(F32Container);
    
    // Check optimizations applied
    const I32_HAS_INLINE: bool = hasfield!(I32Container, "inline_buffer");
    const I32_HAS_HASH: bool = hasfield!(I32Container, "hash_cache");
    const I32_HAS_SORT: bool = hasmethod!(I32Container, "sort");
    const I32_HAS_SIMD: bool = hasmethod!(I32Container, "sum_simd");
    
    const STRING_HAS_INLINE: bool = hasfield!(StringContainer, "inline_buffer");
    const STRING_HAS_HASH: bool = hasfield!(StringContainer, "hash_cache");
    const STRING_HAS_CLONE: bool = hasmethod!(StringContainer, "deep_clone");
    
    const F32_HAS_SIMD: bool = hasmethod!(F32Container, "sum_simd");
    
    // Type characteristic validation
    if !is_copy!(i32) {
        compile_error!("i32 should be Copy!");
    }
    
    if is_copy!(String) {
        compile_error!("String should not be Copy!");
    }
    
    // Performance tier classification
    type CachedI32Container = CachedContainer<i32>;
    const CACHED_SIZE: usize = sizeof!(CachedI32Container);
    const HAS_CACHE: bool = hasfield!(CachedI32Container, "cache");
    
    println!("Container specializations:");
    println!("  i32: {} bytes, inline={}, hash={}, sort={}, simd={}", 
             I32_SIZE, I32_HAS_INLINE, I32_HAS_HASH, I32_HAS_SORT, I32_HAS_SIMD);
    println!("  String: {} bytes, inline={}, hash={}, deep_clone={}", 
             STRING_SIZE, STRING_HAS_INLINE, STRING_HAS_HASH, STRING_HAS_CLONE);
    println!("  f32: {} bytes, simd={}", F32_SIZE, F32_HAS_SIMD);
    println!("  CachedI32Container: {} bytes, has_cache={}", CACHED_SIZE, HAS_CACHE);
}