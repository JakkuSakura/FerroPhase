pub fn main() -> () {
    const NAME: &str = "FerroPhase";
    const VERSION: &str = "0.1.0";
    const NAME_LEN: usize = 10;
    const VERSION_LEN: usize = 5;
    println!("name='{}' len={}", NAME, NAME_LEN);
    println!("version='{}' len={}", VERSION, VERSION_LEN);
    const IS_EMPTY: bool = false;
    const IS_LONG: bool = true;
    println!("empty={}, long={}", IS_EMPTY, IS_LONG);
    const BANNER: &str = "FerroPhase v0.1.0";
    println!("banner='{}'", BANNER);
    const BUFFER_SIZE: usize = 256;
    println!("buffer_size={}", BUFFER_SIZE);
}
