use std::any::Any;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

pub trait AnyBoxable: Any + Debug + Clone + PartialEq + 'static {}
impl<T: Any + Debug + Clone + PartialEq + 'static> AnyBoxable for T {}
// TODO: make it constant
pub struct AnyBoxVTable {
    pub debug: fn(&dyn Any) -> String,
    pub clone: fn(&dyn Any) -> Box<dyn Any>,
    pub equals: fn(&dyn Any, &dyn Any) -> bool,
    pub hash: fn(&dyn Any) -> u64,
}
impl AnyBoxVTable {
    pub fn new<T: AnyBoxable>() -> Self {
        Self {
            debug: |v| format!("{:?}", v.downcast_ref::<T>().unwrap()),
            clone: |v| Box::new(v.downcast_ref::<T>().unwrap().clone()),
            equals: |a, b| {
                let a = a.downcast_ref::<T>().unwrap();
                let b = b.downcast_ref::<T>().unwrap();
                a == b
            },
            hash: |_| 0,
        }
    }
}

pub struct AnyBox {
    pub value: Box<dyn Any>,
    vtable: Arc<AnyBoxVTable>,
}
impl AnyBox {
    pub fn new<T: AnyBoxable>(t: T) -> Self {
        Self {
            value: Box::new(t),
            vtable: Arc::new(AnyBoxVTable::new::<T>()),
        }
    }

    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.value.downcast_ref()
    }
    pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
        self.value.downcast_mut()
    }

    pub fn downcast<T: Any>(self) -> Result<Box<T>, Self> {
        if self.downcast_ref::<T>().is_some() {
            Ok(self.value.downcast::<T>().unwrap())
        } else {
            Err(self)
        }
    }
}
impl Clone for AnyBox {
    fn clone(&self) -> Self {
        Self {
            value: (self.vtable.clone)(self.value.as_ref()),
            vtable: self.vtable.clone(),
        }
    }
}

impl Debug for AnyBox {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str((self.vtable.debug)(self.value.as_ref()).as_str())
    }
}
impl PartialEq for AnyBox {
    fn eq(&self, other: &Self) -> bool {
        // Compare only when both boxes were constructed for the same concrete type.
        // If vtables differ, treat values as not equal rather than panicking on a bad downcast.
        if Arc::ptr_eq(&self.vtable, &other.vtable) {
            (self.vtable.equals)(self.value.as_ref(), other.value.as_ref())
        } else {
            false
        }
    }
}
impl Eq for AnyBox {}
impl Hash for AnyBox {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.vtable.hash)(self.value.as_ref()).hash(state);
    }
}
