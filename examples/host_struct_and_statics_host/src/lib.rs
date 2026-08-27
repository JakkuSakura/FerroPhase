use fp_core::{Host, HostLayout, HostLayoutRegistry};
use fp_interpret::HostGlobalRegistry;

#[repr(C)]
#[derive(Host)]
pub struct Point {
    pub x: i64,
    pub y: i64,
}

pub static mut HOST_POINT: Point = Point { x: 3, y: 4 };

pub fn host_layouts() -> HostLayoutRegistry {
    let mut layouts = HostLayoutRegistry::new();
    layouts.register::<Point>();
    layouts
}

pub fn host_globals() -> Result<HostGlobalRegistry, fp_interpret::HostGlobalError> {
    let mut globals = HostGlobalRegistry::new();
    let point_type = <Point as HostLayout>::DESCRIPTOR.lir_type();
    let address = std::ptr::addr_of_mut!(HOST_POINT).cast::<u8>();
    globals.register("HOST_POINT", point_type, address, true)?;
    Ok(globals)
}
