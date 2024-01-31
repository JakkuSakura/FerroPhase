use std::cell::RefCell;

pub struct Arena<T> {
    inner: RefCell<Vec<T>>,
}
impl<T: Clone> Arena<T> {
    pub fn new() -> Self {
        Self {
            inner: RefCell::new(Vec::new()),
        }
    }
    pub fn alloc(&mut self, val: T) -> u64 {
        let mut inner = self.inner.borrow_mut();
        let id = inner.len();
        inner.push(val);
        id as u64
    }
    pub fn get(&self, id: u64) -> Option<T> {
        self.inner.borrow().get(id as usize).clone()
    }
}
