pub enum Option<T> {
    Some(T),
    None,
}

pub struct Box<T> {
    value: T,
}

pub struct LinkedNode<T> {
    value: T,
    prev: Option<Box<LinkedNode<T> > >,
    next: Option<Box<LinkedNode<T> > >,
}


pub struct LinkedList<T> {
    values: ::std::alloc::Vec<T>,
    len: usize,
}

impl<T> LinkedList<T> {
    fn new() -> LinkedList<T> {
        LinkedList {
            values: ::std::alloc::Vec::new(),
            len: 0,
        }
    }

    fn from(items: ::std::alloc::Vec<T>) -> LinkedList<T> {
        let mut list: LinkedList<T> = LinkedList::new();
        let mut idx = 0;
        let items_len = items.len();
        while idx < items_len {
            list.push_back(items[idx]);
            idx = idx + 1;
        }
        list
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

    fn push_back(&mut self, value: T) {
        let mut values = self.values;
        if self.len < values.len() {
            values[self.len] = value;
        } else {
            values.push(value);
        }
        self.values = values;
        self.len = self.len + 1;
    }

    fn push_front(&mut self, value: T) {
        let mut values = self.values;
        if self.len < values.len() {
            let mut idx = self.len;
            while idx > 0 {
                values[idx] = values[(idx - 1)];
                idx = idx - 1;
            }
            values[0] = value;
            self.values = values;
            self.len = self.len + 1;
            return;
        }

        let mut shifted: ::std::alloc::Vec<T> = ::std::alloc::Vec::new();
        shifted.push(value);
        let mut idx = 0;
        while idx < self.len {
            shifted.push(values[idx]);
            idx = idx + 1;
        }

        self.values = shifted;
        self.len = self.len + 1;
    }

    fn pop_back_unchecked(&mut self) -> T {
        self.len = self.len - 1;
        self.values[self.len]
    }

    fn pop_front_unchecked(&mut self) -> T {
        let mut values = self.values;
        let front = values[0];

        let mut idx = 1;
        while idx < self.len {
            values[(idx - 1)] = values[idx];
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
        self.values[(self.len - 1)]
    }
}
