pub struct BinaryHeap<T> {
    values: Vec<T>,
    len: i64,
}

impl<T> BinaryHeap<T> {
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
            heap.push(items[idx]);
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

    fn push(&mut self, value: T) {
        if self.len < self.values.len() as i64 {
            self.values[self.len as usize] = value;
        } else {
            self.values.push(value);
        }
        self.len = self.len + 1;

        let mut idx = self.len - 1;
        while idx > 0 {
            let parent = (idx - 1) / 2;
            if self.values[parent as usize] >= self.values[idx as usize] {
                break;
            }

            let parent_value = self.values[parent as usize];
            self.values[parent as usize] = self.values[idx as usize];
            self.values[idx as usize] = parent_value;
            idx = parent;
        }
    }

    fn peek_unchecked(&self) -> T {
        self.values[0]
    }

    fn pop_unchecked(&mut self) -> T {
        let head = self.values[0];

        if self.len == 1 {
            self.len = 0;
            return head;
        }

        self.len = self.len - 1;
        self.values[0] = self.values[self.len as usize];

        let mut idx = 0;
        loop {
            let left = idx * 2 + 1;
            let right = idx * 2 + 2;
            let mut largest = idx;

            if left < self.len && self.values[left as usize] > self.values[largest as usize] {
                largest = left;
            }

            if right < self.len && self.values[right as usize] > self.values[largest as usize] {
                largest = right;
            }

            if largest == idx {
                break;
            }

            let value = self.values[idx as usize];
            self.values[idx as usize] = self.values[largest as usize];
            self.values[largest as usize] = value;
            idx = largest;
        }

        head
    }
}
