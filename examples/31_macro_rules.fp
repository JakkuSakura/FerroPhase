// Demonstrate Rust-like macro_rules! support in FerroPhase.

macro_rules! add {
    ($a:tt, $b:tt) => { $a + $b };
}

macro_rules! make_adder {
    ($name:ident, $value:tt) => {
        fn $name(x: i64) -> i64 {
            x + $value
        }
    };
}

make_adder!(add_two, 2);

fn main() {
    let sum = add!(10, 32);
    let v = add_two(5);
    print("sum =", sum, ", add_two(5) =", v);
}
