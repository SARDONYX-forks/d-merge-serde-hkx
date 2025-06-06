mod color_builder;

use imara_diff::{Algorithm, UnifiedDiffBuilder, intern::InternedInput};

/// Computes a line-based diff between `before` and `after` strings,
/// returning a string that includes all lines, marking changed lines with `-` or `+`,
/// and printing git-style hunk headers.
///
/// # Returns
///
/// A string representing the diff with context lines included.
///
/// # Example
///
/// ```
/// let before = "foo\nbar\nbaz";
/// let after = "foo\nqux\nbaz";
///
/// let diff = diff::diff(before, after);
/// let expected = "@@ -1,3 +1,3 @@
///  foo
/// -bar
/// +qux
///  baz
/// ";
/// assert_eq!(diff, expected);
/// ```
pub fn diff(before: &str, after: &str) -> String {
    let input = InternedInput::new(before, after);
    imara_diff::diff(
        Algorithm::Histogram,
        &input,
        UnifiedDiffBuilder::new(&input),
    )
}

/// Computes a line-based diff between `before` and `after` strings,
/// returning a string that includes all lines, marking changed lines with `-` or `+`,
/// and printing git-style hunk headers.
///
/// Unchanged lines are prefixed with a space.
/// Changed lines from `before` are prefixed with `-` (in red).
/// Changed lines from `after` are prefixed with `+` (in green).
///
/// # Returns
///
/// A string representing the diff with context lines included.
///
/// # Example
///
/// ```
/// use diff::colored_diff;
///
/// let before = "hello\nworld";
/// let after = "hello\nrust";
///
/// let diff = colored_diff(before, after);
/// let expected = "@@ -1,2 +1,2 @@
///  hello
/// \u{1b}[31m-world
/// \u{1b}[0m\u{1b}[32m+rust
/// \u{1b}[0m";
///
/// assert_eq!(diff, expected);
/// ```
pub fn colored_diff(before: &str, after: &str) -> String {
    let input = InternedInput::new(before, after);
    imara_diff::diff(
        Algorithm::Histogram,
        &input,
        color_builder::UnifiedDiffBuilder::new(&input, true),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_diff() {
        let before = "foo\nbar\nbaz";
        let after = "foo\nqux\nbaz";

        let diff = diff(before, after);
        let expected = "@@ -1,3 +1,3 @@
 foo
-bar
+qux
 baz
";
        assert_eq!(diff, expected);
    }

    #[test]
    fn test_colored_diff_with_color() {
        let before = "hello\nworld";
        let after = "hello\nrust";

        let diff = colored_diff(before, after);
        let expected = "@@ -1,2 +1,2 @@
 hello
\u{1b}[31m-world
\u{1b}[0m\u{1b}[32m+rust
\u{1b}[0m";

        assert_eq!(diff, expected);
    }
}
