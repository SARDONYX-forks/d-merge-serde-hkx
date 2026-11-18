use core::{fmt, ops::Range};

use winnow::error::{ContextError, ErrMode, ParseError, StrContext};

/// Error struct to represent parsing errors in a more user-friendly way.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ReadableError {
    title: String,
    message: String,
    span: Range<usize>,
    input: String,
}

impl ReadableError {
    /// Constructs [`Self`] from parse error & input.
    #[inline]
    pub fn from_parse(error: ParseError<&str, ContextError>) -> Self {
        let message = error.inner().to_string();
        let input = (*error.input()).to_string();
        let span = error.char_span();
        Self {
            title: "Parse error".to_string(),
            message,
            span,
            input,
        }
    }

    /// Constructs [`Self`] from parse error & input.
    #[inline]
    pub fn from_context<T>(error: ErrMode<ContextError>, input: T, err_pos: usize) -> Self
    where
        T: core::fmt::Display,
    {
        let (labels, message) = error
            .map(|ctx_err| {
                ctx_err.cause().map_or_else(
                    || {
                        let mut labels = String::new();
                        let mut msg = "expected ".to_string();

                        for ctx in ctx_err.context() {
                            match ctx {
                                StrContext::Label(label) => {
                                    labels += " <- ";
                                    labels += label;
                                }
                                StrContext::Expected(expected) => {
                                    msg += &expected.to_string();
                                }
                                _ => (),
                            }
                        }
                        (labels, msg)
                    },
                    |cause| (String::new(), cause.to_string()),
                )
            })
            .into_inner()
            .unwrap_or_default();

        let input = input.to_string();
        let span = char_boundary(input.as_bytes(), err_pos);

        Self {
            title: labels,
            message,
            span,
            input,
        }
    }

    #[inline]
    pub fn from_display<T, U>(message: T, input: U, err_pos: usize) -> Self
    where
        T: fmt::Display,
        U: fmt::Display,
    {
        let input = input.to_string();
        let span = char_boundary(input.as_bytes(), err_pos);

        Self {
            title: "Validation Error".to_string(),
            message: message.to_string(),
            span,
            input,
        }
    }
}

/// What the error says, for a tool that shows it in its own way (e.g. an editor's underline) instead
/// of the rendered report of [`fmt::Display`].
///
/// e.g. for input `"a\nbcd"` with an error at `c`:
/// - `Self::span`: `3..4` (bytes)
/// - `Self::line_column`: `(1, 1)` (0-based line, column in chars)
impl ReadableError {
    /// The heading of the report. e.g. `Parse error`, ` <- Field <- Object` (may be empty)
    #[inline]
    pub fn title(&self) -> &str {
        &self.title
    }

    /// What is wrong at [`Self::span`]. e.g. `expected </hkparam>`
    #[inline]
    pub fn message(&self) -> &str {
        &self.message
    }

    /// Where the error is in [`Self::input`], in bytes. Always on char boundaries: one char, or empty
    /// at the end of the input.
    #[inline]
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    /// The text that was parsed.
    #[inline]
    pub fn input(&self) -> &str {
        &self.input
    }

    /// The start of [`Self::span`] as `(line, column)`: both 0-based, the column in chars. A `\r\n`
    /// ends a line as `\n` does.
    pub fn line_column(&self) -> (usize, usize) {
        let before = self.input.get(..self.span.start).unwrap_or(&self.input);
        let line = before.matches('\n').count();
        let line_start = before.rfind('\n').map_or(0, |at| at + 1);
        let column = before[line_start..].chars().count();
        (line, column)
    }
}

impl fmt::Display for ReadableError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let report = &[annotate_snippets::Level::ERROR
            .primary_title(&self.title)
            .element(
                annotate_snippets::Snippet::source(&self.input)
                    .fold(true)
                    .annotation(
                        annotate_snippets::AnnotationKind::Context
                            .span(self.span.clone())
                            .label(&self.message),
                    ),
            )];

        let rendered = annotate_snippets::Renderer::plain().render(report);
        rendered.fmt(f)
    }
}

impl std::error::Error for ReadableError {}

/// winnow method
fn char_boundary(input: &[u8], offset: usize) -> core::ops::Range<usize> {
    let len = input.len();
    if offset == len {
        return offset..offset;
    }

    /// Taken from `core::num`
    const fn is_utf8_char_boundary(b: u8) -> bool {
        // This is bit magic equivalent to: b < 128 || b >= 192
        (b as i8) >= -0x40
    }

    let start = (0..(offset + 1).min(len))
        .rev()
        .find(|i| input.get(*i).copied().is_some_and(is_utf8_char_boundary))
        .unwrap_or(0);
    let end = (offset + 1..len)
        .find(|i| input.get(*i).copied().is_some_and(is_utf8_char_boundary))
        .unwrap_or(len);
    start..end
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The example of the accessors' docs: an error at `c` of `"a\nbcd"`.
    #[test]
    fn the_place_of_an_error_is_given_as_is() {
        let error = ReadableError::from_display("bad", "a\nbcd", 3);
        assert_eq!(
            (error.title(), error.message(), error.input()),
            ("Validation Error", "bad", "a\nbcd")
        );
        assert_eq!(error.span(), 3..4);
        assert_eq!(error.line_column(), (1, 1));
    }

    /// The column counts chars, not bytes: `あ` is 3 bytes.
    #[test]
    fn the_column_is_in_chars() {
        let error = ReadableError::from_display("bad", "x\nあいz", 8);
        assert_eq!(error.span(), 8..9);
        assert_eq!(error.line_column(), (1, 2));
    }

    #[test]
    fn an_error_at_the_end_is_after_the_last_char() {
        let error = ReadableError::from_display("bad", "ab", 2);
        assert_eq!(error.span(), 2..2);
        assert_eq!(error.line_column(), (0, 2));
    }
}
