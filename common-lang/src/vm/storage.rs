use crate::value::{Value, ValueBytes};
use crate::vm::VmValue;
use std::collections::BTreeMap;

pub type Ptr = i64;

pub struct VmStorage {
    count: i64,
    memory: BTreeMap<Ptr, VmValue>,
    stack: Vec<Ptr>,
}
impl VmStorage {
    pub fn new() -> Self {
        Self {
            count: 0,
            memory: BTreeMap::new(),
            stack: vec![],
        }
    }
    pub fn alloc(&mut self, value: Value) -> Ptr {
        let ptr = self.count;
        self.count += 1;
        self.memory.insert(ptr, VmValue::new(value));
        ptr
    }
    pub fn alloc_bytes(&mut self, size: usize) -> Ptr {
        let ptr = self.count;
        self.count += 1;
        self.memory
            .insert(ptr, VmValue::new(Value::Bytes(ValueBytes::zeroed(size))));
        ptr
    }

    pub fn dealloc(&mut self, ptr: Ptr) {
        let duplicate = self.memory.remove(&ptr).is_none();
        if duplicate {
            panic!("duplicate dealloc: ptr={}", ptr);
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
}
