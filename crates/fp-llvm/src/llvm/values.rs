use super::context::LlvmContext;
use llvm_ir::constant::Float;
use llvm_ir::{Constant, ConstantRef};

impl LlvmContext {
    /// Create constant values
    pub fn const_i32(&self, value: i32) -> ConstantRef {
        ConstantRef::new(Constant::Int {
            bits: 32,
            value: value as u64,
        })
    }
    pub fn const_i64(&self, value: i64) -> ConstantRef {
        ConstantRef::new(Constant::Int {
            bits: 64,
            value: value as u64,
        })
    }
    pub fn const_f32(&self, value: f32) -> ConstantRef {
        ConstantRef::new(Constant::Float(Float::Single(value)))
    }
    pub fn const_f64(&self, value: f64) -> ConstantRef {
        ConstantRef::new(Constant::Float(Float::Double(value)))
    }
    pub fn const_bool(&self, value: bool) -> ConstantRef {
        ConstantRef::new(Constant::Int {
            bits: 1,
            value: if value { 1 } else { 0 },
        })
    }
}
