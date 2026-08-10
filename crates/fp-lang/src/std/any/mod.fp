
pub trait Any {}

pub fn try_as_dyn<T, U>(value: &T) -> ::std::option::Option<&U> {
    let _ = value;
    ::std::option::Option::None
}

pub fn try_as_dyn_mut<T, U>(value: &mut T) -> ::std::option::Option<&mut U> {
    let _ = value;
    ::std::option::Option::None
}
