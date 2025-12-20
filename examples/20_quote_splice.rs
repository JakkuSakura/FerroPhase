pub fn first_gt(xs: [i32], ys: [i32]) -> i32 {
    for _ in xs.iter().enumerate() {
        {
            if x > ys[i] {
                return x;
            };
        }
    }
    0
}
pub fn main() -> () {
    let _ = first_gt([1, 2, 5], [0, 1, 3]);
}
