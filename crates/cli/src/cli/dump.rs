use crate::error::{FailedReadFileSnafu, Result};
use serde_hkx::bytes::hexdump_string;
use snafu::ResultExt as _;
use std::path::Path;
use tokio::fs;

/// ANSI color representation command examples.
pub const EXAMPLES: &str = color_print::cstr!(
    r#"<blue><bold><underline>Examples</underline></bold></blue>
- <blue!>hkx -> hexdump -> stdout</blue!>
  <cyan!>hkxc dump</cyan!> ./defaultmale.hkx

- <blue!>hkx -> hexdump -> a file</blue!>
  <cyan!>hkxc dump</cyan!> ./defaultmale.hkx <cyan!>-o</cyan!> hexdump.txt
"#
);

#[derive(Debug, clap::Args)]
#[clap(arg_required_else_help = true, after_long_help = EXAMPLES)]
pub(crate) struct Args {
    /// hkx file path
    pub input: String,
    /// If specified, write to a file (If not specified, stdout)
    #[clap(short, long)]
    pub output: Option<String>,
}

/// Output hexdump to stdout/file.
pub async fn output<I, O>(input: I, output: Option<O>) -> Result<()>
where
    I: AsRef<Path>,
    O: AsRef<Path>,
{
    let input = input.as_ref();
    let hexdump = hexdump_string(fs::read(input).await.context(FailedReadFileSnafu {
        path: input.to_path_buf(),
    })?); // NOTE: With newline.
    match output {
        Some(output) => fs::write(output, &hexdump).await?,
        None => print!("{hexdump}"),
    };
    Ok(())
}
