#[derive(Host)]
#[repr(C)]
struct Point {
    x: i64,
    y: i64,
}

extern "host" static mut HOST_POINT: Point;

fn main() {
    println!("host point = ({}, {})", HOST_POINT.x, HOST_POINT.y);
}
