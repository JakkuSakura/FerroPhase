pub struct BinaryHeap<T> {
    values: Vec<T>,
    len: i64,
}

impl BinaryHeap<T> {
    fn new() -> BinaryHeap<T> {
        BinaryHeap {
            values: Vec::new(),
            len: 0,
        }
    }

    fn from(items: Vec<T>) -> BinaryHeap<T> {
        let mut heap = BinaryHeap::new();
        let mut idx = 0;
        let items_len = items.len();
        while idx < items_len {
            heap.insert(items[idx]);
            idx = idx + 1;
        }
        heap
    }

    fn len(&self) -> i64 {
        self.len
    }

    fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn clear(&mut self) {
        self.values = Vec::new();
        self.len = 0;
    }

    fn insert(&mut self, value: T) {
        let mut values = self.values;

        if self.len < values.len() as i64 {
            values[self.len as usize] = value;
        } else {
            values.push(value);
        }

        self.len = self.len + 1;
        let mut idx = self.len - 1;
        while idx > 0 {
            let parent = idx - 1;
            if values[parent as usize] >= values[idx as usize] {
                break;
            }

            let parent_value = values[parent as usize];
            values[parent as usize] = values[idx as usize];
            values[idx as usize] = parent_value;
            idx = parent;
        }

        self.values = values;
    }

    fn push(&mut self, value: T) {
        self.insert(value);
    }

    fn peek_unchecked(&self) -> T {
        self.values[0]
    }

    fn pop_unchecked(&mut self) -> T {
        let mut values = self.values;
        let head = values[0];

        let mut idx = 1;
        while idx < self.len {
            values[(idx - 1) as usize] = values[idx as usize];
            idx = idx + 1;
        }

        self.values = values;
        self.len = self.len - 1;
        head
    }
}
