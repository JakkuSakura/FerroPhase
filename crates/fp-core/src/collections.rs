//! Shared collection abstractions used throughout fp-core.
//!
//! The default build uses `dashmap::DashMap` for concurrency. Bootstrap builds
//! replace it with a single-threaded `HashMap` wrapped in interior mutability
//! so we can shed the heavier dependency surface when compiling the minimal
//! pipeline.

#[cfg(not(feature = "bootstrap"))]
use dashmap::DashMap;
#[cfg(feature = "bootstrap")]
use std::cell::RefCell;
#[cfg(feature = "bootstrap")]
use std::collections::HashMap;
use std::hash::Hash;

pub struct ConcurrentMap<K, V> {
    #[cfg(not(feature = "bootstrap"))]
    inner: DashMap<K, V>,
    #[cfg(feature = "bootstrap")]
    inner: RefCell<HashMap<K, V>>,
}

impl<K, V> Default for ConcurrentMap<K, V>
where
    K: Eq + Hash,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> ConcurrentMap<K, V>
where
    K: Eq + Hash,
{
    pub fn new() -> Self {
        #[cfg(not(feature = "bootstrap"))]
        {
            Self {
                inner: dashmap::DashMap::new(),
            }
        }

        #[cfg(feature = "bootstrap")]
        {
            Self {
                inner: RefCell::new(HashMap::new()),
            }
        }
    }

    pub fn insert(&self, key: K, value: V) {
        #[cfg(not(feature = "bootstrap"))]
        {
            self.inner.insert(key, value);
        }

        #[cfg(feature = "bootstrap")]
        {
            self.inner.borrow_mut().insert(key, value);
        }
    }

    pub fn get_cloned(&self, key: &K) -> Option<V>
    where
        V: Clone,
    {
        #[cfg(not(feature = "bootstrap"))]
        {
            self.inner.get(key).map(|entry| entry.value().clone())
        }

        #[cfg(feature = "bootstrap")]
        {
            self.inner.borrow().get(key).cloned()
        }
    }

    pub fn get_or_insert_default(&self, key: K) -> V
    where
        V: Default + Clone,
    {
        #[cfg(not(feature = "bootstrap"))]
        {
            self.inner.entry(key).or_default().value().clone()
        }

        #[cfg(feature = "bootstrap")]
        {
            let mut map = self.inner.borrow_mut();
            let entry = map.entry(key).or_insert_with(V::default);
            entry.clone()
        }
    }

    pub fn for_each<F>(&self, mut f: F)
    where
        F: FnMut(&K, &V),
    {
        #[cfg(not(feature = "bootstrap"))]
        {
            for entry in self.inner.iter() {
                let (k, v) = entry.pair();
                f(k, v);
            }
        }

        #[cfg(feature = "bootstrap")]
        {
            for (k, v) in self.inner.borrow().iter() {
                f(k, v);
            }
        }
    }
}
