use std::cell::RefCell;

pub trait ArenaMeta {
    type Item: Clone;
    type Id: Copy;
    fn id_to_usize(id: Self::Id) -> usize;
    fn usize_to_id(id: usize) -> Self::Id;
}
pub struct Arena<M: ArenaMeta> {
    inner: RefCell<Vec<M::Item>>,
}
impl<M: ArenaMeta> Arena<M> {
    pub fn new() -> Self {
        Self {
            inner: RefCell::new(Vec::new()),
        }
    }
    pub fn alloc(&self, val: M::Item) -> M::Id {
        let mut inner = self.inner.borrow_mut();
        let id = inner.len();
        inner.push(val);
        M::usize_to_id(id)
    }
    pub fn get(&self, id: M::Id) -> Option<M::Item> {
        let internal = M::id_to_usize(id);
        self.inner.borrow().get(internal).cloned()
    }
}
