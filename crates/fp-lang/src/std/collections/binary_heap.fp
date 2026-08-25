
pub struct BinaryHeap<T> {
    values: ::std::alloc::Vec<T>,
    len: usize,
}

impl<T> BinaryHeap<T> {
    fn new() -> BinaryHeap<T> {
        BinaryHeap {
            values: ::std::alloc::Vec::new(),
            len: 0,
        }
    }

    fn from(items: ::std::alloc::Vec<T>) -> BinaryHeap<T> {
        let mut heap: BinaryHeap<T> = BinaryHeap::new();
        let mut idx: usize = 0;
        let items_len = items.len();
        while idx < items_len {
            heap.insert(items[idx]);
            idx = idx + 1;
        }
        heap
    }

    fn len(&self) -> usize {
        self.len
    }

    fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn clear(&mut self) {
        self.values = ::std::alloc::Vec::new();
        self.len = 0;
    }

    fn insert(&mut self, value: T) {
        let mut values = self.values;

        if self.len < values.len() {
            values[self.len] = value;
        } else {
            values.push(value);
        }

        self.len = self.len + 1;
        let mut idx: usize = self.len - 1;
        while idx > 0 {
            let parent = idx - 1;
            if values[parent] >= values[idx] {
                break;
            }

            let parent_value = values[parent];
            values[parent] = values[idx];
            values[idx] = parent_value;
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

        let mut idx: usize = 1;
        while idx < self.len {
            values[(idx - 1)] = values[idx];
            idx = idx + 1;
        }

        self.values = values;
        self.len = self.len - 1;
        head
    }
}
