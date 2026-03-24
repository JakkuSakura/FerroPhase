pub enum Option<T> {
    Some(T),
    None,
}

impl<T> Option<T> {
    pub fn is_some(&self) -> bool {
        match self {
            Option::Some(_) => true,
            Option::None => false,
        }
    }

    pub fn is_none(&self) -> bool {
        !self.is_some()
    }

    pub fn unwrap(self) -> T {
        match self {
            Option::Some(value) => value,
            Option::None => panic("called Option::unwrap() on a None value"),
        }
    }

    pub fn expect(self, message: &str) -> T {
        match self {
            Option::Some(value) => value,
            Option::None => panic(message),
        }
    }

    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Option::Some(value) => value,
            Option::None => default,
        }
    }

    pub fn map<U>(self, f: fn(T) -> U) -> Option<U> {
        match self {
            Option::Some(value) => Option::Some(f(value)),
            Option::None => Option::None,
        }
    }
}
