#[derive(Host)]
#[repr(C)]
struct Point {
    x: i64,
    y: i64,
}

#[host]
static mut HOST_POINT: Point = Point { x: 0, y: 0 };

fn main() {
    println!("host point = ({}, {})", HOST_POINT.x, HOST_POINT.y);
}
