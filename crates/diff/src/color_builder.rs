// SPDX-FileCopyrightText: (C) 2025 pascalkuthe <pascalkuthe@pm.me>
// SPDX-License-Identifier: Apache-2.0
//
// Adapted from `imara_diff::BasicLineDiffPrinter` (imara-diff 0.2) to color the changed lines.
use std::fmt::{self, Write};

use imara_diff::{Interner, Token, UnifiedDiffPrinter};

/// Like [`imara_diff::BasicLineDiffPrinter`], but removed lines are red and added lines green
/// (ANSI escapes). Each line is printed with exactly one `\n`.
pub struct ColorLinePrinter<'a>(pub &'a Interner<&'a str>);

impl ColorLinePrinter<'_> {
    /// The line of `token` without its line break.
    fn line(&self, token: Token) -> &str {
        let line = self.0[token];
        line.strip_suffix('\n').unwrap_or(line)
    }
}

impl UnifiedDiffPrinter for ColorLinePrinter<'_> {
    fn display_header(
        &self,
        mut f: impl Write,
        start_before: u32,
        start_after: u32,
        len_before: u32,
        len_after: u32,
    ) -> fmt::Result {
        writeln!(
            f,
            "@@ -{},{} +{},{} @@",
            start_before + 1,
            len_before,
            start_after + 1,
            len_after
        )
    }

    fn display_context_token(&self, mut f: impl Write, token: Token) -> fmt::Result {
        writeln!(f, " {}", self.line(token))
    }

    fn display_hunk(&self, mut f: impl Write, before: &[Token], after: &[Token]) -> fmt::Result {
        for &token in before {
            write!(f, "\x1b[31m-{}\n\x1b[0m", self.line(token))?;
        }
        for &token in after {
            write!(f, "\x1b[32m+{}\n\x1b[0m", self.line(token))?;
        }
        Ok(())
    }
}
