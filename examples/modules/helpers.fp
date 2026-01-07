// External module loaded from examples/modules/helpers.fp
pub const SOURCE: &str = "file module";

pub fn greet_from_file(name: &str) {
    println!("[{}] Hello, {}!", SOURCE, name);
}
