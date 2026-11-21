//! Dump binary data in hexadecimal
use std::path::PathBuf;

/// Command examples.
pub fn examples() -> String {
    use super::color::{Example, examples};
    examples(
        None,
        &[
            Example::new("hkx -> hexdump -> stdout", "hkxc dump ./defaultmale.hkx"),
            Example::new(
                "hkx -> hexdump -> a file",
                "hkxc dump ./defaultmale.hkx -o hexdump.txt",
            ),
            Example::new(
                "hexdump -> hkx -> a file",
                "hkxc dump hexdump.txt -o ./defaultmale.hkx -r",
            )
            .with_note("NOTE: For reverse conversion, use `-o` to specify the output path."),
        ],
    )
}

#[derive(Debug, clap::Args)]
#[clap(arg_required_else_help = true, after_long_help = examples())]
pub(crate) struct Args {
    /// hkx file path/hexdump file path(When use `--reverse`)
    pub input: PathBuf,
    /// If specified, write to a file (If not specified, stdout)
    #[clap(short, long)]
    #[clap(long_help = "Output path to write a file.
NOTE: Bytes cannot be written to stdout, so always use `-o` to specify the output when use `--reverse`.")]
    pub output: Option<PathBuf>,

    /// Reverse conversion from hexdump to bytes
    #[clap(short, long)]
    pub reverse: bool,
}
