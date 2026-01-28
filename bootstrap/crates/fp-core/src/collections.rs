//! Shared collection abstractions used throughout fp-core.
//!
//! The bootstrap build uses a mutex-protected map to avoid external deps.

use std::collections::HashMap;
use std::hash::Hash;
use std::sync::Mutex;

pub struct ConcurrentMap<K, V> {
    inner: Mutex<HashMap<K, V>>,
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
        Self {
            inner: Mutex::new(HashMap::new()),
        }
    }

    pub fn insert(&self, key: K, value: V) {
        self.inner
            .lock()
            .expect("concurrent map mutex poisoned")
            .insert(key, value);
    }

    pub fn get_cloned(&self, key: &K) -> Option<V>
    where
        V: Clone,
    {
        self.inner
            .lock()
            .expect("concurrent map mutex poisoned")
            .get(key)
            .cloned()
    }

    pub fn get_or_insert_default(&self, key: K) -> V
    where
        V: Default + Clone,
    {
        let mut guard = self.inner.lock().expect("concurrent map mutex poisoned");
        guard.entry(key).or_default().clone()
    }

    pub fn for_each<F>(&self, mut f: F)
    where
        F: FnMut(&K, &V),
    {
        let guard = self.inner.lock().expect("concurrent map mutex poisoned");
        for (k, v) in guard.iter() {
            f(k, v);
        }
    }
}
