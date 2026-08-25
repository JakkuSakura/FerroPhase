pub trait Index<Idx> {
    type Output;
    fn index(&self, idx: Idx) -> Self::Output;
    fn index_set(&mut self, idx: Idx, value: Self::Output);
}

pub struct Vec<T> {
    ptr: *mut T,
    len: usize,
    capacity: usize,
}

impl<T> [T] {
    pub fn len(&self) -> usize { compile_error!("slice length is compiler-provided") }

    pub fn is_empty(&self) -> bool { self.len() == 0 }
}

impl<T> Vec<T> {
    pub const fn new() -> Vec<T> {
        Vec { ptr: 0 as *mut T, len: 0, capacity: 0 }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn capacity(&self) -> usize {
        self.capacity
    }

    pub fn from(arr: [T]) -> Vec<T> {
        let len = arr.len();
        let elem_size = sizeof!(T) as usize;
        let new_ptr = ::libc::malloc((len * elem_size) as u64) as *mut T;
        let mut idx: usize = 0;
        while idx < len {
            let dest = (new_ptr as usize + idx * elem_size) as *mut T;
            *dest = arr[idx];
            idx = idx + 1;
        }
        Vec { ptr: new_ptr, len, capacity: len }
    }

    pub fn push(&mut self, value: T) {
        if self.len == self.capacity {
            let new_capacity = if self.capacity == 0 { 4 } else { self.capacity * 2 };
            let elem_size = sizeof!(T) as usize;
            let new_ptr = if self.capacity == 0 {
                ::libc::malloc((new_capacity * elem_size) as u64) as *mut T
            } else {
                ::libc::realloc(self.ptr as *mut void, (new_capacity * elem_size) as u64) as *mut T
            };
            self.ptr = new_ptr;
            self.capacity = new_capacity;
        }
        let dest = (self.ptr as usize + self.len * (sizeof!(T) as usize)) as *mut T;
        *dest = value;
        self.len = self.len + 1;
    }

    pub fn pop(&mut self) -> ::std::option::Option<T> {
        if self.len == 0 {
            ::std::option::Option::None
        } else {
            self.len = self.len - 1;
            ::std::option::Option::Some(self[self.len])
        }
    }

    pub fn get(&self, index: usize) -> ::std::option::Option<T> {
        if index < self.len {
            ::std::option::Option::Some(self[index])
        } else {
            ::std::option::Option::None
        }
    }

    pub fn clear(&mut self) {
        self.len = 0;
    }
}

impl<T> Index<usize> for Vec<T> {
    type Output = T;

    fn index(&self, idx: usize) -> Self::Output {
        let addr = (self.ptr as usize + idx * (sizeof!(T) as usize)) as *mut T;
        *addr
    }

    fn index_set(&mut self, idx: usize, value: Self::Output) {
        let addr = (self.ptr as usize + idx * (sizeof!(T) as usize)) as *mut T;
        *addr = value;
    }
}

impl Vec<&str> {
    pub fn join(&self, sep: &str) -> ::std::string::String {
        let len = self.len();
        let mut result: ::std::string::String = ::std::string::String::new();
        let mut idx: usize = 0;
        while idx < len {
            if idx > 0 {
                result.extend(sep);
            }
            result.extend(self[idx]);
            idx = idx + 1;
        }
        result
    }
}

impl Vec<::std::string::String> {
    pub fn join(&self, sep: &str) -> ::std::string::String {
        let len = self.len();
        let mut result: ::std::string::String = ::std::string::String::new();
        let mut idx: usize = 0;
        while idx < len {
            if idx > 0 {
                result.extend(sep);
            }
            result.extend(self[idx].as_str());
            idx = idx + 1;
        }
        result
    }
}
