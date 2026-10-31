mod color_builder;

use imara_diff::{Algorithm, BasicLineDiffPrinter, Diff, InternedInput, UnifiedDiffConfig};

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
    let diff = line_diff(&input);
    diff.unified_diff(&BasicLineDiffPrinter(&input.interner), UnifiedDiffConfig::default(), &input)
        .to_string()
}

/// Histogram diff of the lines of `input`, with the hunks moved to natural line boundaries.
fn line_diff(input: &InternedInput<&str>) -> Diff {
    let mut diff = Diff::compute(Algorithm::Histogram, input);
    diff.postprocess_lines(input);
    diff
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
    let diff = line_diff(&input);
    let printer = color_builder::ColorLinePrinter(&input.interner);
    diff.unified_diff(&printer, UnifiedDiffConfig::default(), &input).to_string()
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
