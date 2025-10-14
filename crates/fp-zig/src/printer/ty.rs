use fp_core::ast::{Ty, TypeEnum, TypePrimitive, TypeStruct};

use super::ZigEmitter;

impl ZigEmitter {
    pub(super) fn render_type(&self, ty: &Ty) -> String {
        match ty {
            Ty::Primitive(TypePrimitive::Int(int_ty)) => match int_ty {
                fp_core::ast::TypeInt::I64 => "i64".to_string(),
                fp_core::ast::TypeInt::U64 => "u64".to_string(),
                fp_core::ast::TypeInt::I32 => "i32".to_string(),
                fp_core::ast::TypeInt::U32 => "u32".to_string(),
                fp_core::ast::TypeInt::I16 => "i16".to_string(),
                fp_core::ast::TypeInt::U16 => "u16".to_string(),
                fp_core::ast::TypeInt::I8 => "i8".to_string(),
                fp_core::ast::TypeInt::U8 => "u8".to_string(),
                fp_core::ast::TypeInt::BigInt => "i128".to_string(),
            },
            Ty::Primitive(TypePrimitive::Decimal(decimal_ty)) => match decimal_ty {
                fp_core::ast::DecimalType::F64 => "f64".to_string(),
                fp_core::ast::DecimalType::F32 => "f32".to_string(),
                _ => "f64".to_string(),
            },
            Ty::Primitive(TypePrimitive::Bool) => "bool".to_string(),
            Ty::Primitive(TypePrimitive::Char) => "u8".to_string(),
            Ty::Primitive(TypePrimitive::String) => "[]const u8".to_string(),
            Ty::Struct(TypeStruct { name, .. }) => name.name.clone(),
            Ty::Enum(TypeEnum { name, .. }) => name.name.clone(),
            Ty::Reference(reference) => self.render_type(&reference.ty),
            Ty::Vec(vec_ty) => {
                let inner = self.render_type(&vec_ty.ty);
                format!("[]const {}", inner)
            }
            Ty::Slice(slice_ty) => {
                let inner = self.render_type(&slice_ty.elem);
                format!("[]const {}", inner)
            }
            Ty::Unit(_) => "void".to_string(),
            _ => "anytype".to_string(),
        }
    }
}
