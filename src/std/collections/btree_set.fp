struct BTreeSetNode<T> {
    key: T,
    left: i64,
    right: i64,
}

pub struct BTreeSet<T> {
    root: i64,
    len: i64,
    nodes: Vec<BTreeSetNode<T>>,
}

impl<T> BTreeSet<T> {
    fn new() -> BTreeSet<T> {
        BTreeSet {
            root: -1,
            len: 0,
            nodes: Vec::new(),
        }
    }

    fn from(values: Vec<T>) -> BTreeSet<T> {
        let mut set = BTreeSet::new();
        let mut idx = 0;
        let values_len = values.len();
        while idx < values_len {
            set.insert(values[idx]);
            idx = idx + 1;
        }
        set
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

    fn contains(&self, value: T) -> bool {
        self.find_node_idx(value) >= 0
    }

    fn insert(&mut self, value: T) {
        if self.root < 0 {
            self.nodes.push(BTreeSetNode {
                key: value,
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

            if value == node.key {
                return;
            }

            if value < node.key {
                if node.left < 0 {
                    let child_idx = self.nodes.len() as i64;
                    self.nodes.push(BTreeSetNode {
                        key: value,
                        left: -1,
                        right: -1,
                    });
                    self.nodes[current_idx] = BTreeSetNode {
                        key: node.key,
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
                self.nodes.push(BTreeSetNode {
                    key: value,
                    left: -1,
                    right: -1,
                });
                self.nodes[current_idx] = BTreeSetNode {
                    key: node.key,
                    left: node.left,
                    right: child_idx,
                };
                self.len = self.len + 1;
                return;
            }

            current = node.right;
        }
    }

    fn find_node_idx(&self, value: T) -> i64 {
        let mut current = self.root;
        while current >= 0 {
            let node = self.nodes[current as usize];
            if value == node.key {
                return current;
            }
            if value < node.key {
                current = node.left;
            } else {
                current = node.right;
            }
        }
        -1
    }
}
