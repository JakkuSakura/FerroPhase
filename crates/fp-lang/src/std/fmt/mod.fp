pub struct Error {}

pub trait Debug {}

pub trait Write {
    fn write_str(&mut self, s: &str) -> std::result::Result<(), Error>;
}

impl Write for str {
    fn write_str(&mut self, s: &str) -> std::result::Result<(), Error> {
        let _ = self;
        let _ = s;
        std::result::Result::Ok(())
    }
}
