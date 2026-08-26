pub struct HashMapEntry<K, V> {
    key: K,
    value: V,
}


pub struct HashMap<K, V> {
    len: usize,
    keys: ::std::alloc::Vec<K>,
    values: ::std::alloc::Vec<V>,
}

impl<K, V> HashMap<K, V> {
    fn new() -> HashMap<K, V> {
        HashMap {
            len: 0,
            keys: ::std::alloc::Vec::new(),
            values: ::std::alloc::Vec::new(),
        }
    }

    // Blocked on a pre-existing type-checker gap: `ParamTy` has no
    // scope/binder id (just `{index, name}`), so identically-named/
    // positioned generics from different scopes (this impl's own K,V vs.
    // `HashMapEntry<K,V>`'s own K,V) collide during unification. Not new —
    // this method never worked before `is_std_module()` stopped hiding
    // std bodies from type-checking; leave stubbed until ParamTy gets
    // real scope tracking.
    #[unimplemented]
    fn from(entries: ::std::alloc::Vec<HashMapEntry<K, V>>) -> HashMap<K, V> {
        let mut map: HashMap<K, V> = HashMap {
            len: 0,
            keys: ::std::alloc::Vec::new(),
            values: ::std::alloc::Vec::new(),
        };
        let mut idx: usize = 0;
        let entries_len = entries.len();
        while idx < entries_len {
            let entry = entries[idx];
            map.insert(entry.key, entry.value);
            idx = idx + 1;
        }
        map
    }

    fn len(&self) -> usize {
        self.len
    }

    fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn clear(&mut self) {
        self.len = 0;
        self.keys = ::std::alloc::Vec::new();
        self.values = ::std::alloc::Vec::new();
    }

    fn contains_key(&self, key: K) -> bool {
        self.find_node_idx(key).is_some()
    }

    fn insert(&mut self, key: K, value: V) {
        let mut keys = self.keys;
        let mut values = self.values;
        let mut idx: usize = 0;
        let keys_len = keys.len();
        while idx < keys_len {
            if keys[idx] == key {
                values[idx] = value;
                self.keys = keys;
                self.values = values;
                return;
            }
            idx = idx + 1;
        }

        keys.push(key);
        values.push(value);
        self.keys = keys;
        self.values = values;
        self.len = self.len + 1;
    }

    fn get_unchecked(&self, key: K) -> V {
        let idx = self.find_node_idx(key);
        match idx {
            ::std::option::Option::Some(idx) => return self.values[idx],
            ::std::option::Option::None => {}
        }
        loop {}
    }

    fn find_node_idx(&self, key: K) -> ::std::option::Option<usize> {
        let mut idx: usize = 0;
        let keys_len = self.keys.len();
        while idx < keys_len {
            if self.keys[idx] == key {
                return ::std::option::Option::Some(idx);
            }
            idx = idx + 1;
        }
        ::std::option::Option::None
    }
}
