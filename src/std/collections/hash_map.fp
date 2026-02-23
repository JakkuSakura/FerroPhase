pub struct HashMapEntry<K, V> {
    key: K,
    value: V,
}

pub struct HashMap<K, V> {
    len: i64,
    keys: Vec<K>,
    values: Vec<V>,
}

impl<K, V> HashMap<K, V> {
    fn new() -> HashMap<K, V> {
        HashMap {
            len: 0,
            keys: Vec::new(),
            values: Vec::new(),
        }
    }

    fn from(entries: Vec<HashMapEntry<K, V>>) -> HashMap<K, V> {
        let mut map = HashMap::new();
        let mut idx = 0;
        let entries_len = entries.len();
        while idx < entries_len {
            let entry = entries[idx];
            map.insert(entry.key, entry.value);
            idx = idx + 1;
        }
        map
    }

    fn len(&self) -> i64 {
        self.len
    }

    fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn clear(&mut self) {
        self.len = 0;
        self.keys = Vec::new();
        self.values = Vec::new();
    }

    fn contains_key(&self, key: K) -> bool {
        self.find_node_idx(key) >= 0
    }

    fn insert(&mut self, key: K, value: V) {
        let mut keys = self.keys;
        let mut values = self.values;
        let mut idx = 0;
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
        if idx >= 0 {
            return self.values[idx as usize];
        }
        loop {}
    }

    fn find_node_idx(&self, key: K) -> i64 {
        let mut idx = 0;
        let keys_len = self.keys.len();
        while idx < keys_len {
            if self.keys[idx] == key {
                return idx as i64;
            }
            idx = idx + 1;
        }
        -1
    }
}
