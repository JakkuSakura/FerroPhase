use crate::value::{Value, ValueBytes, ValueEscaped, ValuePointer};
use crate::vm::VmValue;
use common::warn;
use std::collections::BTreeMap;

pub type Ptr = ValuePointer;
const DEFAULT_ALIGN: usize = 8;

pub struct VmStorage {
    count: i64,
    memory: BTreeMap<Ptr, VmValue>,
    stack: Vec<Ptr>,
}
impl VmStorage {
    pub fn new() -> Self {
        Self {
            count: DEFAULT_ALIGN as i64,
            memory: BTreeMap::new(),
            stack: vec![],
        }
    }
    pub fn alloc_ptr(&mut self) -> Ptr {
        let ptr = ValuePointer::managed(self.count);
        self.count += DEFAULT_ALIGN as i64;
        ptr
    }
    pub fn alloc(&mut self, value: Value) -> Ptr {
        let ptr = self.alloc_ptr();
        self.memory.insert(ptr, VmValue::new(value));
        ptr
    }
    pub fn alloc_escaped(&mut self, size: usize) -> Ptr {
        // TODO: use proper alignment
        unsafe {
            let layout = std::alloc::Layout::from_size_align(size, DEFAULT_ALIGN).unwrap();
            let ptr = std::alloc::alloc_zeroed(layout);
            let ptr = ValuePointer::escaped(ptr);
            self.memory.insert(
                ptr,
                VmValue::new(Value::Escaped(ValueEscaped {
                    ptr: ptr.value,
                    size: size as _,
                    align: DEFAULT_ALIGN as _,
                })),
            );
            ptr
        }
    }
    pub fn alloc_bytes(&mut self, size: usize) -> Ptr {
        let ptr = self.alloc_ptr();
        self.memory
            .insert(ptr, VmValue::new(Value::Bytes(ValueBytes::zeroed(size))));
        ptr
    }

    pub fn dealloc(&mut self, ptr: Ptr) {
        let old = self.memory.remove(&ptr);
        match old {
            Some(VmValue {
                value: Value::Escaped(size),
                ..
            }) => unsafe {
                let layout =
                    std::alloc::Layout::from_size_align(size.size as _, DEFAULT_ALIGN).unwrap();
                std::alloc::dealloc(ptr.value as *mut u8, layout);
            },
            Some(_) => {}
            None => {
                warn!("dealloc: invalid ptr={}", ptr);
                return;
            }
        }
    }
    pub fn get(&self, ptr: Ptr) -> Option<&VmValue> {
        self.memory.get(&ptr)
    }
    pub fn get_mut(&mut self, ptr: Ptr) -> Option<&mut VmValue> {
        self.memory.get_mut(&ptr)
    }
    pub fn push_stack(&mut self, ptr: Ptr) {
        self.stack.push(ptr);
    }
    pub fn get_stack(&self) -> Option<Ptr> {
        self.stack.last().copied()
    }
    pub fn pop_stack(&mut self) -> Option<Ptr> {
        self.stack.pop()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alloc_dealloc() {
        let mut storage = VmStorage::new();
        let ptr = storage.alloc(Value::Bytes(ValueBytes::zeroed(10)));
        assert_eq!(storage.get(ptr).unwrap().as_slice().unwrap().len(), 10);
        storage.dealloc(ptr);
        assert!(storage.get(ptr).is_none());
    }

    #[test]
    fn test_alloc_dealloc_multiple() {
        let mut storage = VmStorage::new();
        let ptr1 = storage.alloc(Value::Bytes(ValueBytes::zeroed(10)));
        let ptr2 = storage.alloc(Value::Bytes(ValueBytes::zeroed(20)));
        assert_eq!(storage.get(ptr1).unwrap().as_slice().unwrap().len(), 10);
        assert_eq!(storage.get(ptr2).unwrap().as_slice().unwrap().len(), 20);
        storage.dealloc(ptr1);
        assert!(storage.get(ptr1).is_none());
        assert_eq!(storage.get(ptr2).unwrap().as_slice().unwrap().len(), 20);
        storage.dealloc(ptr2);
        assert!(storage.get(ptr2).is_none());
    }
    #[test]
    fn test_escaped_value() {
        let mut storage = VmStorage::new();
        let ptr = storage.alloc_escaped(10);
        assert_eq!(storage.get(ptr).unwrap().as_slice().unwrap().len(), 10);
        storage.dealloc(ptr);
        assert!(storage.get(ptr).is_none());
    }
}
