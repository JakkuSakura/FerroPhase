use rust_macro::specialize_module;

#[specialize_module]
mod s {
    fn print() {
        println!("Hello, world!");
    }
    pub fn main() {
        print()
    }
}
fn main() {
    s::main();
}
