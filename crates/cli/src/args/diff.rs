use std::path::PathBuf;

/// Command examples.
pub fn examples() -> String {
    use super::color::{Example, examples};
    examples(
        None,
        &[
            Example::new(
                "XML diff -> stdout",
                "hkxc diff ./defaultmale_by_hkxcmd.xml ./defaultmale_by_HavokBehaviorProcess.xml",
            ),
            Example::new(
                "hkx hexdump diff -> stdout",
                "hkxc diff ./defaultmale_by_hkxcmd.hkx ./defaultmale_by_HavokBehaviorProcess.hkx",
            ),
            Example::new(
                "hexdump diff -> a file(no color)",
                "hkxc diff ./defaultmale_x86_hexdump.txt ./defaultmale_x64_hexdump.txt -o diff.txt",
            ),
        ],
    )
}

#[derive(Debug, clap::Args)]
#[clap(arg_required_else_help = true, after_long_help = examples())]
pub(crate) struct Args {
    /// Old path
    pub old: PathBuf,
    /// New path
    pub new: PathBuf,

    /// If specified, write to a file (If not specified, stdout)
    #[clap(short, long)]
    pub output: Option<PathBuf>,
}
