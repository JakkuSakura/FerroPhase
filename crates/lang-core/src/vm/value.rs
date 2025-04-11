use crate::ast::{AstValue, ValuePointer};

pub struct VmValue {
    pub value: AstValue,
}
impl VmValue {
    pub fn new(value: AstValue) -> Self {
        Self { value }
    }
    pub fn as_slice(&self) -> Option<&[u8]> {
        match &self.value {
            AstValue::Bytes(bytes) => Some(bytes),
            AstValue::Escaped(escaped) => unsafe { Some(escaped.as_slice()) },
            _ => None,
        }
    }
    pub fn as_slice_mut(&mut self) -> Option<&mut [u8]> {
        match &mut self.value {
            AstValue::Bytes(bytes) => Some(bytes),
            AstValue::Escaped(escaped) => unsafe { Some(escaped.as_slice_mut()) },
            _ => None,
        }
    }

    pub unsafe fn as_object<T>(&self) -> Option<&T> {
        match &self.value {
            AstValue::Bytes(object) => Some(std::mem::transmute(&**object)),
            AstValue::Escaped(object) => Some(std::mem::transmute(object.as_ptr())),
            _ => None,
        }
    }
    pub unsafe fn as_object_mut<T>(&mut self) -> Option<&mut T> {
        match &mut self.value {
            AstValue::Bytes(object) => Some(std::mem::transmute(&mut **object)),
            AstValue::Escaped(object) => Some(std::mem::transmute(object.as_mut_ptr())),
            _ => None,
        }
    }
    pub fn as_ptr(&self) -> Option<ValuePointer> {
        match &self.value {
            AstValue::Pointer(ptr) => Some(*ptr),
            _ => None,
        }
    }
}
