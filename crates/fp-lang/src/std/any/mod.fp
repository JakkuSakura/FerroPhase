use std::option::Option;

pub trait Any {}

pub fn try_as_dyn<T, U>(value: &T) -> Option<&U> {
    let _ = value;
    Option::None
}

pub fn try_as_dyn_mut<T, U>(value: &mut T) -> Option<&mut U> {
    let _ = value;
    Option::None
}
