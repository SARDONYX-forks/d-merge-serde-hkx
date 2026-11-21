use clap::builder::styling::{AnsiColor, Color, Style};

/// See: [`clap.rs not printing colors during --help`](https://stackoverflow.com/questions/74068168/clap-rs-not-printing-colors-during-help)
pub const fn get_styles() -> clap::builder::Styles {
    clap::builder::Styles::styled()
        .usage(
            Style::new()
                .bold()
                .underline()
                .fg_color(Some(Color::Ansi(AnsiColor::BrightBlue))),
        )
        .header(
            Style::new()
                .bold()
                .underline()
                .fg_color(Some(Color::Ansi(AnsiColor::BrightBlue))),
        )
        .literal(Style::new().fg_color(Some(Color::Ansi(AnsiColor::Cyan))))
        .valid(
            Style::new()
                .bold()
                .underline()
                .fg_color(Some(Color::Ansi(AnsiColor::Cyan))),
        )
        .invalid(
            Style::new()
                .bold()
                .fg_color(Some(Color::Ansi(AnsiColor::Red))),
        )
        .error(
            Style::new()
                .bold()
                .fg_color(Some(Color::Ansi(AnsiColor::Red))),
        )
        .placeholder(Style::new().fg_color(Some(Color::Ansi(AnsiColor::White))))
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Text colors
//
// NOTE: ANSI codes written by these are stripped automatically when the output is not a terminal,
//       `NO_COLOR` env is set, or `--no-color` is specified.
//       - help: by clap(`Command::color`)
//       - others: by `anstream::{println, eprintln}`

/// e.g. `Examples`
const HEADING: Style = Style::new()
    .bold()
    .underline()
    .fg_color(Some(Color::Ansi(AnsiColor::Blue)));
/// e.g. `- hkx -> xml`
const TITLE: Style = Style::new().fg_color(Some(Color::Ansi(AnsiColor::BrightBlue)));
/// e.g. `hkxc convert`, `--input`
const COMMAND: Style = Style::new().fg_color(Some(Color::Ansi(AnsiColor::BrightCyan)));
/// e.g. `(NOTE: ...)`
const NOTE: Style = Style::new().fg_color(Some(Color::Ansi(AnsiColor::Yellow)));
/// Error message
pub const ERROR: Style = Style::new().fg_color(Some(Color::Ansi(AnsiColor::Red)));
/// Success message
pub const SUCCESS: Style = Style::new().fg_color(Some(Color::Ansi(AnsiColor::Green)));

/// One example of the help.
pub struct Example {
    pub title: &'static str,
    /// Highlight the command name(`hkxc <sub>`) and options(`-x`/`--xxx`).
    pub command: &'static str,
    pub note: Option<&'static str>,
}

impl Example {
    pub const fn new(title: &'static str, command: &'static str) -> Self {
        Self {
            title,
            command,
            note: None,
        }
    }

    pub const fn with_note(mut self, note: &'static str) -> Self {
        self.note = Some(note);
        self
    }
}

/// Build the examples text for `after_long_help`.
pub fn examples(intro: Option<&str>, examples: &[Example]) -> String {
    use core::fmt::Write as _;

    let mut out = String::new();
    if let Some(intro) = intro {
        let _ = writeln!(out, "{intro}\n");
    }
    let _ = writeln!(out, "{HEADING}Examples{HEADING:#}");
    for (i, example) in examples.iter().enumerate() {
        if i != 0 {
            out.push('\n');
        }
        let _ = write!(out, "- {TITLE}{}{TITLE:#}", example.title);
        if let Some(note) = example.note {
            let _ = write!(out, " {NOTE}({note}){NOTE:#}");
        }
        let _ = writeln!(out, "\n  {}", highlight_command(example.command));
    }
    out
}

/// Highlight `hkxc <sub command>` and options.
fn highlight_command(command: &str) -> String {
    command
        .split(' ')
        .enumerate()
        .map(|(i, token)| {
            let is_command = i < 2 && token.trim_start_matches("./").starts_with("hkxc")
                || i == 1 && !token.starts_with(['.', '-', '"']);
            if is_command || token.starts_with('-') {
                format!("{COMMAND}{token}{COMMAND:#}")
            } else {
                token.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Is color enabled for the stream? (terminal & not `--no-color` & not `NO_COLOR` env)
pub fn is_color_enabled<S: anstream::stream::RawStream>(stream: &S) -> bool {
    anstream::AutoStream::choice(stream) != anstream::ColorChoice::Never
}
