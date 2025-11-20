use super::context::LlvmContext;
use llvm_ir::types::FPType;
use llvm_ir::Type;

impl LlvmContext {
    /// Create basic LLVM types
    pub fn i1_type(&self) -> Type {
        Type::IntegerType { bits: 1 }
    }
    pub fn i8_type(&self) -> Type {
        Type::IntegerType { bits: 8 }
    }
    pub fn i16_type(&self) -> Type {
        Type::IntegerType { bits: 16 }
    }
    pub fn i32_type(&self) -> Type {
        Type::IntegerType { bits: 32 }
    }
    pub fn i64_type(&self) -> Type {
        Type::IntegerType { bits: 64 }
    }
    pub fn f32_type(&self) -> Type {
        Type::FPType(FPType::Single)
    }
    pub fn f64_type(&self) -> Type {
        Type::FPType(FPType::Double)
    }
    pub fn void_type(&self) -> Type {
        Type::VoidType
    }
}
