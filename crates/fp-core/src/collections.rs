//! Shared collection abstractions used throughout fp-core.
//!
//! The default build uses `dashmap::DashMap` for concurrency.

use dashmap::DashMap;
use std::hash::Hash;

pub struct ConcurrentMap<K, V> {
    inner: DashMap<K, V>,
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
            inner: dashmap::DashMap::new(),
        }
    }

    pub fn insert(&self, key: K, value: V) {
        self.inner.insert(key, value);
    }

    pub fn get_cloned(&self, key: &K) -> Option<V>
    where
        V: Clone,
    {
        self.inner.get(key).map(|entry| entry.value().clone())
    }

    pub fn get_or_insert_default(&self, key: K) -> V
    where
        V: Default + Clone,
    {
        self.inner.entry(key).or_default().value().clone()
    }

    pub fn for_each<F>(&self, mut f: F)
    where
        F: FnMut(&K, &V),
    {
        for entry in self.inner.iter() {
            let (k, v) = entry.pair();
            f(k, v);
        }
    }
}
