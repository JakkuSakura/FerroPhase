pub fn main() -> () {
    const FLAG_A: bool = true;
    const FLAG_B: bool = false;
    struct Config {
        pub x: i64,
        pub y: i64,
    }
    const CONFIG: Config = Config { x: 100, y: 20 };
    println!("x={}, y={}", CONFIG.x, CONFIG.y);
    const SIZE: usize = 256;
    const ITEMS: [i64; 256] = [0; 256];
    println!("array size: {}", ITEMS.len());
}
