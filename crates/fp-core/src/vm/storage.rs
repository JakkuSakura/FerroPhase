use crate::ast::{AstValue, ValueBytes, ValueEscaped, ValuePointer};
use crate::vm::VmValue;
use common::warn;
use std::collections::HashMap;

pub type Ptr = ValuePointer;
const DEFAULT_ALIGN: usize = 8;

pub struct VmStorage {
    count: i64,
    memory: HashMap<Ptr, VmValue>,
    stack: Vec<Ptr>,
}
impl VmStorage {
    pub fn new() -> Self {
        Self {
            count: DEFAULT_ALIGN as i64,
            memory: HashMap::new(),
            stack: vec![],
        }
    }
    pub fn alloc_ptr(&mut self) -> Ptr {
        let ptr = ValuePointer::managed(self.count);
        self.count += DEFAULT_ALIGN as i64;
        ptr
    }
    pub fn alloc(&mut self, value: AstValue) -> Ptr {
        let ptr = self.alloc_ptr();
        self.memory.insert(ptr, VmValue::new(value));
        ptr
    }
    pub fn alloc_escaped(&mut self, size: usize) -> Ptr {
        let escaped = ValueEscaped::new(size as _, DEFAULT_ALIGN as _);
        let ptr = escaped.ptr;
        self.memory
            .insert(escaped.ptr, VmValue::new(AstValue::Escaped(escaped)));
        ptr
    }
    pub fn alloc_bytes(&mut self, size: usize) -> Ptr {
        let ptr = self.alloc_ptr();
        self.memory
            .insert(ptr, VmValue::new(AstValue::Bytes(ValueBytes::zeroed(size))));
        ptr
    }

    pub fn dealloc(&mut self, ptr: Ptr) {
        let old = self.memory.remove(&ptr);
        match old {
            Some(VmValue {
                value: AstValue::Escaped(escaped),
            }) => {
                warn!("dealloc escaped but did not drop: ptr={}", escaped.ptr);
            }
            Some(_) => {}
            None => {
                warn!("dealloc: invalid ptr={}", ptr);
            }
        }
    }
    pub fn drop<T>(&mut self, ptr: Ptr) {
        let old = self.memory.remove(&ptr);
        match old {
            Some(VmValue {
                value: AstValue::Escaped(mut escaped),
            }) => unsafe {
                escaped.drop_in_place::<T>();
            },
            None => {
                warn!("drop: invalid ptr={}", ptr);
                return;
            }
            _ => {}
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
    use std::mem::size_of;
    use std::sync::atomic::AtomicBool;

    #[test]
    fn test_alloc_dealloc() {
        let mut storage = VmStorage::new();
        let ptr = storage.alloc(AstValue::Bytes(ValueBytes::zeroed(10)));
        assert_eq!(storage.get(ptr).unwrap().as_slice().unwrap().len(), 10);
        storage.dealloc(ptr);
        assert!(storage.get(ptr).is_none());
    }

    #[test]
    fn test_alloc_dealloc_multiple() {
        let mut storage = VmStorage::new();
        let ptr1 = storage.alloc(AstValue::Bytes(ValueBytes::zeroed(10)));
        let ptr2 = storage.alloc(AstValue::Bytes(ValueBytes::zeroed(20)));
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
    #[test]
    fn test_box_with_escaped_slice() {
        let mut storage = VmStorage::new();
        static DROP: AtomicBool = AtomicBool::new(false);
        // Assume an opaque type T, allocate it on heap
        #[derive(Debug)]
        #[repr(C)]
        struct Obj {
            a: i64,
            b: i64,
            c: i64,
            d: i64,
        }
        impl Drop for Obj {
            fn drop(&mut self) {
                DROP.store(true, std::sync::atomic::Ordering::Relaxed);
            }
        }
        // Box::new(t)
        let ptr = storage.alloc_escaped(size_of::<Obj>());
        // write to the object
        unsafe {
            let t = storage
                .get_mut(ptr)
                .unwrap()
                .as_object_mut::<Obj>()
                .unwrap();
            t.a = 1;
            t.b = 2;
            t.c = 3;
            t.d = 4;
        }
        // read from the object
        unsafe {
            let t = storage.get(ptr).unwrap().as_object::<Obj>().unwrap();
            assert_eq!(t.a, 1);
            assert_eq!(t.b, 2);
            assert_eq!(t.c, 3);
            assert_eq!(t.d, 4);
        }
        // drop(t)
        storage.drop::<Obj>(ptr);
        assert!(DROP.load(std::sync::atomic::Ordering::Relaxed));
    }
}
