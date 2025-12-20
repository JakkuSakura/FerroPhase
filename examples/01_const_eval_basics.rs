pub fn main() -> () {
    pub const BUFFER_SIZE: i64 = 4096;
    pub const MAX_CONNECTIONS: i64 = 150;
    pub const FACTORIAL_5: i64 = 120;
    pub const IS_LARGE: bool = true;
    println!(
        "Buffer: {}KB, factorial(5)={}, large={}",
        BUFFER_SIZE / 1024,
        FACTORIAL_5,
        IS_LARGE
    );
    pub struct Config {
        pub buffer_size: i64,
        pub max_connections: i64,
    }
    pub const DEFAULT_CONFIG: Config = Config {
        buffer_size: 4096,
        max_connections: 150,
    };
    println!(
        "Config: {}KB buffer, {} connections",
        DEFAULT_CONFIG.buffer_size / 1024,
        DEFAULT_CONFIG.max_connections
    );
    let runtime_multiplier = 3;
    let optimized_size = intrinsic_const_block({ 4096 * 2 });
    let cache_strategy = intrinsic_const_block({
        if 4096 > 2048 {
            "large".to_string()
        } else {
            "small".to_string()
        }
    });
    let total_memory =
        runtime_multiplier * intrinsic_const_block({ BUFFER_SIZE * MAX_CONNECTIONS });
    println!(
        "Const blocks: size={}, strategy={}, memory={}",
        optimized_size, cache_strategy, total_memory
    );
}
