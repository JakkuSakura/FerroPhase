use super::context::LlvmContext;
use inkwell::values::{FloatValue, IntValue, PointerValue};
use inkwell::AddressSpace;

impl LlvmContext {
    /// Create constant values.
    pub fn const_i32(&self, value: i32) -> IntValue<'static> {
        self.context.i32_type().const_int(value as u64, true)
    }

    pub fn const_i64(&self, value: i64) -> IntValue<'static> {
        self.context.i64_type().const_int(value as u64, true)
    }

    pub fn const_f32(&self, value: f32) -> FloatValue<'static> {
        self.context.f32_type().const_float(value as f64)
    }

    pub fn const_f64(&self, value: f64) -> FloatValue<'static> {
        self.context.f64_type().const_float(value)
    }

    pub fn const_bool(&self, value: bool) -> IntValue<'static> {
        self.context.bool_type().const_int(if value { 1 } else { 0 }, false)
    }

    pub fn const_null_ptr(&self) -> PointerValue<'static> {
        self.context.ptr_type(AddressSpace::default()).const_null()
    }
}
