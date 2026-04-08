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
            Result::Err(_) => panic("called Result::unwrap() on an Err value"),
        }
    }
}
