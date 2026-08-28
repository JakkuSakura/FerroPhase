#[derive(Host)]
#[repr(C)]
struct Point {
    x: i64,
    y: i64,
}

extern "host" static mut HOST_POINT: Point;
extern "host" fn host_add(a: i64, b: i64) -> i64;

fn main() {
    println!("host point = ({}, {})", HOST_POINT.x, HOST_POINT.y);
    println!("host add = {}", host_add(20, 22));
}
