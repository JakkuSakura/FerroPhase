use crate::value::Value;

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
    pub fn as_ptr(&self) -> Option<*const u8> {
        match &self.value {
            Value::Bytes(bytes) => Some(bytes.as_ptr()),
            _ => None,
        }
    }
}
