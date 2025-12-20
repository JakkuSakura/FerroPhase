pub fn main() -> () {
    pub const NAME: &str = "FerroPhase".to_string();
    pub const VERSION: &str = "0.1.0".to_string();
    pub const NAME_LEN: usize = 10;
    pub const VERSION_LEN: usize = 5;
    println!("name='{}' len={}", NAME, NAME_LEN);
    println!("version='{}' len={}", VERSION, VERSION_LEN);
    pub const IS_EMPTY: bool = false;
    pub const IS_LONG: bool = true;
    println!("empty={}, long={}", IS_EMPTY, IS_LONG);
    pub const BANNER: &str = "FerroPhase v0.1.0".to_string();
    println!("banner='{}'", BANNER);
    pub const BUFFER_SIZE: usize = 256;
    println!("buffer_size={}", BUFFER_SIZE);
}
