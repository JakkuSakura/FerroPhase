pub enum Result<T, E> {
    Ok(T),
    Err(E),
}

pub fn Ok<T, E>(value: T) -> Result<T, E> {
    Result::Ok(value)
}

pub fn Err<T, E>(error: E) -> Result<T, E> {
    Result::Err(error)
}

impl<T, E> Result<T, E> {
    pub fn is_ok(&self) -> bool {
        match self {
            Result::Ok(_) => true,
            Result::Err(_) => false,
        }
    }

    pub fn is_err(&self) -> bool {
        !self.is_ok()
    }

    pub fn unwrap(self) -> T {
        match self {
            Result::Ok(value) => value,
            Result::Err(_) => panic!("called Result::unwrap() on an Err value"),
        }
    }

    pub fn expect(self, message: &str) -> T {
        match self {
            Result::Ok(value) => value,
            Result::Err(_) => panic!(message),
        }
    }

    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Result::Ok(value) => value,
            Result::Err(_) => default,
        }
    }

    pub fn unwrap_or_else(self, f: fn(E) -> T) -> T {
        match self {
            Result::Ok(value) => value,
            Result::Err(error) => f(error),
        }
    }

    pub fn map<U>(self, f: fn(T) -> U) -> Result<U, E> {
        match self {
            Result::Ok(value) => Result::Ok(f(value)),
            Result::Err(error) => Result::Err(error),
        }
    }

    pub fn map_or<U>(self, default: U, f: fn(T) -> U) -> U {
        match self {
            Result::Ok(value) => f(value),
            Result::Err(_) => default,
        }
    }

    pub fn map_err<F>(self, f: fn(E) -> F) -> Result<T, F> {
        match self {
            Result::Ok(value) => Result::Ok(value),
            Result::Err(error) => Result::Err(f(error)),
        }
    }

    pub fn ok(self) -> Option<T> {
        match self {
            Result::Ok(value) => Option::Some(value),
            Result::Err(_) => Option::None,
        }
    }

    pub fn err(self) -> Option<E> {
        match self {
            Result::Ok(_) => Option::None,
            Result::Err(error) => Option::Some(error),
        }
    }
}
