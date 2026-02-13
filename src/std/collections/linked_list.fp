pub struct LinkedNode<T> {
    value: T,
    prev: Option<Box<LinkedNode<T>>>,
    next: Option<Box<LinkedNode<T>>>,
}

pub struct LinkedList<T> {
    values: Vec<T>,
    prev: Vec<i64>,
    next: Vec<i64>,
    head: i64,
    tail: i64,
    len: i64,
}

impl<T> LinkedList<T> {
    fn new() -> LinkedList<T> {
        LinkedList {
            values: Vec::new(),
            prev: Vec::new(),
            next: Vec::new(),
            head: -1,
            tail: -1,
            len: 0,
        }
    }

    fn from(items: Vec<T>) -> LinkedList<T> {
        let mut list = LinkedList::new();
        let mut idx = 0;
        let items_len = items.len();
        while idx < items_len {
            list.push_back(items[idx]);
            idx = idx + 1;
        }
        list
    }

    fn len(&self) -> i64 {
        self.len
    }

    fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn clear(&mut self) {
        self.values = Vec::new();
        self.prev = Vec::new();
        self.next = Vec::new();
        self.head = -1;
        self.tail = -1;
        self.len = 0;
    }

    fn push_back(&mut self, value: T) {
        let idx = self.values.len() as i64;
        self.values.push(value);
        self.prev.push(self.tail);
        self.next.push(-1);

        if self.tail >= 0 {
            self.next[self.tail as usize] = idx;
        } else {
            self.head = idx;
        }

        self.tail = idx;
        self.len = self.len + 1;
    }

    fn push_front(&mut self, value: T) {
        let idx = self.values.len() as i64;
        self.values.push(value);
        self.prev.push(-1);
        self.next.push(self.head);

        if self.head >= 0 {
            self.prev[self.head as usize] = idx;
        } else {
            self.tail = idx;
        }

        self.head = idx;
        self.len = self.len + 1;
    }

    fn pop_back_unchecked(&mut self) -> T {
        let idx = self.tail;
        let prev_idx = self.prev[idx as usize];

        if prev_idx >= 0 {
            self.next[prev_idx as usize] = -1;
        } else {
            self.head = -1;
        }

        self.tail = prev_idx;
        self.len = self.len - 1;
        self.values[idx as usize]
    }

    fn pop_front_unchecked(&mut self) -> T {
        let idx = self.head;
        let next_idx = self.next[idx as usize];

        if next_idx >= 0 {
            self.prev[next_idx as usize] = -1;
        } else {
            self.tail = -1;
        }

        self.head = next_idx;
        self.len = self.len - 1;
        self.values[idx as usize]
    }

    fn front_unchecked(&self) -> T {
        self.values[self.head as usize]
    }

    fn back_unchecked(&self) -> T {
        self.values[self.tail as usize]
    }
}
