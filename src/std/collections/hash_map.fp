pub struct HashMapEntry<K, V> {
    key: K,
    value: V,
}

struct HashNode<K, V> {
    key: K,
    value: V,
    left: i64,
    right: i64,
}

pub struct HashMap<K, V> {
    root: i64,
    len: i64,
    nodes: Vec<HashNode<K, V>>,
}

impl HashMap<K, V> {
    fn new() -> HashMap<K, V> {
        HashMap {
            root: -1,
            len: 0,
            nodes: Vec::new(),
        }
    }

    fn from(entries: Vec<HashMapEntry<K, V>>) -> HashMap<K, V> {
        let mut map = HashMap {
            root: -1,
            len: 0,
            nodes: Vec::new(),
        };
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
        self.root = -1;
        self.len = 0;
        self.nodes = Vec::new();
    }

    fn contains_key(&self, key: K) -> bool {
        self.find_node_idx(key) >= 0
    }

    fn insert(&mut self, key: K, value: V) {
        if self.root < 0 {
            self.nodes.push(HashNode {
                key,
                value,
                left: -1,
                right: -1,
            });
            self.root = 0;
            self.len = 1;
            return;
        }

        let mut current = self.root;
        loop {
            let current_idx = current as usize;
            let node = self.nodes[current_idx];

            if key == node.key {
                self.nodes[current_idx] = HashNode {
                    key,
                    value,
                    left: node.left,
                    right: node.right,
                };
                return;
            }

            if key < node.key {
                if node.left < 0 {
                    let child_idx = self.nodes.len() as i64;
                    self.nodes.push(HashNode {
                        key,
                        value,
                        left: -1,
                        right: -1,
                    });
                    self.nodes[current_idx] = HashNode {
                        key: node.key,
                        value: node.value,
                        left: child_idx,
                        right: node.right,
                    };
                    self.len = self.len + 1;
                    return;
                }
                current = node.left;
                continue;
            }

            if node.right < 0 {
                let child_idx = self.nodes.len() as i64;
                self.nodes.push(HashNode {
                    key,
                    value,
                    left: -1,
                    right: -1,
                });
                self.nodes[current_idx] = HashNode {
                    key: node.key,
                    value: node.value,
                    left: node.left,
                    right: child_idx,
                };
                self.len = self.len + 1;
                return;
            }

            current = node.right;
        }
    }

    fn get_unchecked(&self, key: K) -> V {
        let mut current = self.root;
        while current >= 0 {
            let node = self.nodes[current as usize];
            if key == node.key {
                return node.value;
            }
            if key < node.key {
                current = node.left;
            } else {
                current = node.right;
            }
        }
        loop {}
    }

    fn find_node_idx(&self, key: K) -> i64 {
        let mut current = self.root;
        while current >= 0 {
            let node = self.nodes[current as usize];
            if key == node.key {
                return current;
            }
            if key < node.key {
                current = node.left;
            } else {
                current = node.right;
            }
        }
        -1
    }
}
