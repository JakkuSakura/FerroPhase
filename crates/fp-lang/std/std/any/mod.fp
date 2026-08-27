
pub trait Any {}

pub fn try_as_dyn<T, U>(value: &T) -> ::core::option::Option<&U> {
    let _ = value;
    ::core::option::Option::None
}

pub fn try_as_dyn_mut<T, U>(value: &mut T) -> ::core::option::Option<&mut U> {
    let _ = value;
    ::core::option::Option::None
}
