// External module loaded from examples/modules/helpers.fp
mod math;

pub const SOURCE: &str = "file module";

pub fn greet_from_file(name: &str) {
    println!("[{}] Hello, {}!", SOURCE, name);
}
