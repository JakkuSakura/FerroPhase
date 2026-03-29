pub struct Error {
    message: str,
}

impl Error {
    pub fn new(message: &str) -> Error {
        Error {
            message,
        }
    }

    pub fn message(&self) -> str {
        self.message
    }
}
