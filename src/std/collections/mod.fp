struct HashMapEntry<K, V> {
    key: K,
    value: V,
}

struct HashMap<K, V> {
    entries: Vec<HashMapEntry<K, V>>,
}

impl HashMap<K, V> {
    fn from(entries: Vec<HashMapEntry<K, V>>) -> HashMap<K, V> {
        HashMap { entries }
    }

    fn len(&self) -> i64 {
        self.entries.len()
    }

    fn get_unchecked(&self, key: K) -> V {
        let mut idx = 0;
        while idx < self.entries.len() {
            let entry = self.entries[idx];
            if entry.key == key {
                return entry.value;
            }
            idx = idx + 1;
        }
        loop {}
    }
}
