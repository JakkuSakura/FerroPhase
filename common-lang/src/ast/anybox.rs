use serde::{Deserialize, Serialize};
use std::any::Any;
use std::fmt::Debug;
use std::rc::Rc;
pub trait AnyBoxable: Any + Debug + Clone + PartialEq + Eq + 'static {}
impl<T: Any + Debug + Clone + PartialEq + Eq + 'static> AnyBoxable for T {}
pub struct AnyBox {
    pub value: Box<dyn Any>,
    debug: Rc<str>,
    clone: fn(&dyn Any) -> Box<dyn Any>,
    equals: fn(&dyn Any, &dyn Any) -> bool,
}
impl AnyBox {
    pub fn new<T: AnyBoxable>(t: T) -> Self {
        Self {
            debug: Rc::from(format!("{:?}", t)),
            value: Box::new(t),
            clone: |v| Box::new(v.downcast_ref::<T>().unwrap().clone()),
            equals: |a, b| {
                let a = a.downcast_ref::<T>().unwrap();
                let b = b.downcast_ref::<T>().unwrap();
                a == b
            },
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
            debug: self.debug.clone(),
            value: (self.clone)(self.value.as_ref()),
            clone: self.clone,
            equals: self.equals,
        }
    }
}
impl Debug for AnyBox {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.debug)
    }
}
impl Serialize for AnyBox {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.debug.serialize(serializer)
    }
}
impl PartialEq for AnyBox {
    fn eq(&self, other: &Self) -> bool {
        (self.equals)(self.value.as_ref(), other.value.as_ref())
    }
}
impl Eq for AnyBox {}
impl<'de> Deserialize<'de> for AnyBox {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let name = String::deserialize(deserializer)?;
        Err(serde::de::Error::custom(format!(
            "Cannot deserialize AnyBox: {}",
            name
        )))
    }
}
