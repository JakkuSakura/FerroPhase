use std::borrow::Cow;
use std::path::PathBuf;

/// Recursively load source files beneath an embedded-stdlib root.
pub trait SourceBundle {
    fn paths() -> &'static [&'static str];
    fn get(path: &str) -> Option<Cow<'static, [u8]>>;
}

pub fn load_sources<E: SourceBundle>(extension: &str) -> Option<Vec<(PathBuf, String)>> {
    E::paths()
        .iter()
        .filter(|path| path.ends_with(extension))
        .map(|path| {
            let bytes = E::get(path)?;
            Some((
                PathBuf::from(*path),
                String::from_utf8(bytes.into_owned()).ok()?,
            ))
        })
        .collect()
}

pub fn read_source<E: SourceBundle>(path: &str) -> Option<&'static str> {
    let bytes = E::get(path)?;
    let text = String::from_utf8(bytes.into_owned()).ok()?;
    Some(Box::leak(text.into_boxed_str()))
}
