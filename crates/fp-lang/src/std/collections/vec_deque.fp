pub struct VecDeque<T> {
    values: Vec<T>,
    len: i64,
}

impl VecDeque<T> {
    fn new() -> VecDeque<T> {
        VecDeque {
            values: Vec::new(),
            len: 0,
        }
    }

    fn from(items: Vec<T>) -> VecDeque<T> {
        let mut deque = VecDeque::new();
        let mut idx = 0;
        let items_len = items.len();
        while idx < items_len {
            deque.push_back(items[idx]);
            idx = idx + 1;
        }
        deque
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

    fn push_back(&mut self, value: T) {
        let mut values = self.values;
        if self.len < values.len() as i64 {
            values[self.len as usize] = value;
        } else {
            values.push(value);
        }
        self.values = values;
        self.len = self.len + 1;
    }

    fn push_front(&mut self, value: T) {
        let mut values = self.values;
        if self.len < values.len() as i64 {
            let mut idx = self.len;
            while idx > 0 {
                values[idx as usize] = values[(idx - 1) as usize];
                idx = idx - 1;
            }
            values[0] = value;
            self.values = values;
            self.len = self.len + 1;
            return;
        }

        let mut shifted: Vec<T> = Vec::new();
        shifted.push(value);
        let mut idx = 0;
        while idx < self.len {
            shifted.push(values[idx as usize]);
            idx = idx + 1;
        }

        self.values = shifted;
        self.len = self.len + 1;
    }

    fn pop_back_unchecked(&mut self) -> T {
        self.len = self.len - 1;
        self.values[self.len as usize]
    }

    fn pop_front_unchecked(&mut self) -> T {
        let mut values = self.values;
        let front = values[0];

        let mut idx = 1;
        while idx < self.len {
            values[(idx - 1) as usize] = values[idx as usize];
            idx = idx + 1;
        }

        self.values = values;
        self.len = self.len - 1;
        front
    }

    fn front_unchecked(&self) -> T {
        self.values[0]
    }

    fn back_unchecked(&self) -> T {
        self.values[(self.len - 1) as usize]
    }
}
