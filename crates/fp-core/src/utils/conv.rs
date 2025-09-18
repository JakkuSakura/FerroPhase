use std::fmt::Display;

pub trait TryConv<T> {
    type Error;
    fn try_conv(self) -> Result<T, Self::Error>;
}

impl<F, T> TryConv<T> for F
where
    F: TryInto<T>,
    F::Error: Display,
{
    type Error = eyre::Error;

    fn try_conv(self) -> Result<T, Self::Error> {
        self.try_into().map_err(|e| eyre::eyre!("{}", e))
    }
}
