use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
#[error("oops!")]
#[diagnostic(
    code(oops::my::bad),
    url(docsrs),
    help("try doing it better next time?")
)]
pub struct ErrorSpanned {
    // The Source that we're gonna be printing snippets out of.
    // This can be a String if you don't have or care about file names.
    #[source_code]
    src: NamedSource<String>,
    // Snippets and highlights can be included in the diagnostic!
    #[label("This bit here")]
    span: SourceSpan,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic]
    fn test_error_span() {
        let src = "source\n  text\n    here".to_string();

        Err::<(), miette::Report>(
            ErrorSpanned {
                src: NamedSource::new("bad_file.rs", src),
                span: (9, 4).into(),
            }
            .into(),
        )
        .unwrap();
    }
}
