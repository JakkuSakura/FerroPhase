use super::context::LlvmContext;
use inkwell::types::{FloatType, IntType, PointerType, VoidType};
use inkwell::AddressSpace;

impl LlvmContext {
    /// Create basic LLVM types.
    pub fn i1_type(&self) -> IntType<'static> {
        self.context.bool_type()
    }

    pub fn i8_type(&self) -> IntType<'static> {
        self.context.i8_type()
    }

    pub fn i16_type(&self) -> IntType<'static> {
        self.context.i16_type()
    }

    pub fn i32_type(&self) -> IntType<'static> {
        self.context.i32_type()
    }

    pub fn i64_type(&self) -> IntType<'static> {
        self.context.i64_type()
    }

    pub fn i128_type(&self) -> IntType<'static> {
        self.context.custom_width_int_type(128)
    }

    pub fn f32_type(&self) -> FloatType<'static> {
        self.context.f32_type()
    }

    pub fn f64_type(&self) -> FloatType<'static> {
        self.context.f64_type()
    }

    pub fn void_type(&self) -> VoidType<'static> {
        self.context.void_type()
    }

    pub fn ptr_type(&self) -> PointerType<'static> {
        self.context.ptr_type(AddressSpace::default())
    }
}
