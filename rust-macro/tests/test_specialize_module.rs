#[test]
fn test_specialize_module() {
    #[rust_macro::specialize_module]
    mod s {
        use std::println;

        fn print() {
            println!("Hello, world!");
        }
        pub fn main() {
            print()
        }
    }
    s::main();
}
