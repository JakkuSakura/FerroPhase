use fp_core::{Host, HostFunctionDescriptor, HostLayout, HostLayoutRegistry};
use fp_interpret::{HostFunctionRegistry, HostGlobalRegistry};
use fp_core::lir::{LirFunctionSignature, LirType};

#[repr(C)]
#[derive(Host)]
pub struct Point {
    pub x: i64,
    pub y: i64,
}

pub static mut HOST_POINT: Point = Point { x: 3, y: 4 };

pub extern "C" fn host_add(a: i64, b: i64) -> i64 { a + b }

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

pub fn host_functions() -> Result<HostFunctionRegistry, fp_interpret::HostFunctionError> {
    let mut functions = HostFunctionRegistry::new();
    functions.register(
        HostFunctionDescriptor::new(
            "host_add",
            LirFunctionSignature {
                params: vec![LirType::I64, LirType::I64],
                return_type: LirType::I64,
                is_variadic: false,
            },
        ),
        host_add as *const std::ffi::c_void,
    )?;
    Ok(functions)
}
