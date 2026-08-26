
pub struct HashSet<T> {
    len: usize,
    values: ::std::alloc::Vec<T>,
}

impl<T> HashSet<T> {
    fn new() -> HashSet<T> {
        HashSet {
            len: 0,
            values: ::std::alloc::Vec::new(),
        }
    }

    fn from(values: ::std::alloc::Vec<T>) -> HashSet<T> {
        let mut set: HashSet<T> = HashSet::new();
        let mut idx: usize = 0;
        let values_len = values.len();
        while idx < values_len {
            set.insert(values[idx]);
            idx = idx + 1;
        }
        set
    }

    fn len(&self) -> usize {
        self.len
    }

    fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn clear(&mut self) {
        self.len = 0;
        self.values = ::std::alloc::Vec::new();
    }

    fn contains(&self, value: T) -> bool {
        self.find_node_idx(value).is_some()
    }

    fn insert(&mut self, value: T) {
        let mut values = self.values;
        let mut idx: usize = 0;
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

    fn find_node_idx(&self, value: T) -> ::std::option::Option<usize> {
        let mut idx: usize = 0;
        let values_len = self.values.len();
        while idx < values_len {
            if self.values[idx] == value {
                return ::std::option::Option::Some(idx);
            }
            idx = idx + 1;
        }
        ::std::option::Option::None
    }
}
