pub struct Error {}

pub trait Debug {}

pub trait Write {
    fn write_str(&mut self, s: &str) -> ::core::result::Result<(), Error>;
}
