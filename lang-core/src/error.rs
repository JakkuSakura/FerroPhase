use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
#[error("oops!")]
#[diagnostic(
    code(oops::my::bad),
    url(docsrs),
    help("try doing it better next time?")
)]
struct MyBad {
    // The Source that we're gonna be printing snippets out of.
    // This can be a String if you don't have or care about file names.
    #[source_code]
    src: NamedSource<String>,
    // Snippets and highlights can be included in the diagnostic!
    #[label("This bit here")]
    bad_bit: SourceSpan,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic]
    fn test_my_bad() {
        let src = "source\n  text\n    here".to_string();

        Err::<(), miette::Report>(
            MyBad {
                src: NamedSource::new("bad_file.rs", src),
                bad_bit: (9, 4).into(),
            }
            .into(),
        )
        .unwrap();
    }
}
