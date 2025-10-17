//! Shared collection abstractions used throughout fp-core.
//!
//! In regular builds we re-export `dashmap::DashMap` so existing code keeps its
//! concurrency guarantees. Bootstrap builds can later swap this alias to a
//! single-threaded implementation without touching callers.

#[cfg_attr(feature = "bootstrap", allow(dead_code))]
pub type ConcurrentMap<K, V> = dashmap::DashMap<K, V>;
