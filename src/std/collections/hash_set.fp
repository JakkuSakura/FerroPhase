pub struct FpHashSet<T> {
    len: i64,
    values: Vec<T>,
}

impl FpHashSet<T> {
    fn new() -> FpHashSet<T> {
        FpHashSet {
            len: 0,
            values: Vec::new(),
        }
    }

    fn from(values: Vec<T>) -> FpHashSet<T> {
        let mut set = FpHashSet::new();
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
        self.len = 0;
        self.values = Vec::new();
    }

    fn contains_value(&self, value: T) -> bool {
        self.find_node_idx(value) >= 0
    }

    fn contains(&self, value: T) -> bool {
        self.contains_value(value)
    }

    fn insert(&mut self, value: T) {
        let mut values = self.values;
        let mut idx = 0;
        let values_len = values.len();
        while idx < values_len {
            if values[idx] == value {
                self.values = values;
                return;
            }
            idx = idx + 1;
        }

        values.push(value);
        self.values = values;
        self.len = self.len + 1;
    }

    fn find_node_idx(&self, value: T) -> i64 {
        let mut idx = 0;
        let values_len = self.values.len();
        while idx < values_len {
            if self.values[idx] == value {
                return idx as i64;
            }
            idx = idx + 1;
        }
        -1
    }
}
