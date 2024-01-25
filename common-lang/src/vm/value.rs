use crate::value::{Value, ValuePointer};

pub struct VmValue {
    pub value: Value,
}
impl VmValue {
    pub fn new(value: Value) -> Self {
        Self { value }
    }
    pub fn as_slice(&self) -> Option<&[u8]> {
        match &self.value {
            Value::Bytes(bytes) => Some(bytes),
            Value::Escaped(escaped) => unsafe { Some(escaped.as_slice()) },
            _ => None,
        }
    }
    pub fn as_slice_mut(&mut self) -> Option<&mut [u8]> {
        match &mut self.value {
            Value::Bytes(bytes) => Some(bytes),
            Value::Escaped(escaped) => unsafe { Some(escaped.as_slice_mut()) },
            _ => None,
        }
    }

    pub unsafe fn as_object<T>(&self) -> Option<&T> {
        match &self.value {
            Value::Bytes(object) => Some(std::mem::transmute(&**object)),
            Value::Escaped(object) => Some(std::mem::transmute(object.as_ptr())),
            _ => None,
        }
    }
    pub unsafe fn as_object_mut<T>(&mut self) -> Option<&mut T> {
        match &mut self.value {
            Value::Bytes(object) => Some(std::mem::transmute(&mut **object)),
            Value::Escaped(object) => Some(std::mem::transmute(object.as_mut_ptr())),
            _ => None,
        }
    }
    pub fn as_ptr(&self) -> Option<ValuePointer> {
        match &self.value {
            Value::Pointer(ptr) => Some(*ptr),
            _ => None,
        }
    }
}
