//! Show dependency tree from havok behavior state machine (hkx/xml file)
use std::path::PathBuf;

/// Command examples.
pub fn examples() -> String {
    use super::color::{Example, examples};
    examples(
        None,
        &[
            Example::new(
                "xml -> dependencies tree -> stdout",
                "hkxc tree ./defaultmale.xml",
            ),
            Example::new(
                "hkx -> dependencies tree -> stdout(+log)",
                "hkxc tree ./defaultmale.hkx --log-level trace --log-file ./hkx_tree.log",
            ),
            Example::new(
                "hkx -> dependencies tree -> a file",
                "hkxc tree ./defaultmale.hkx -o tree.txt",
            ),
        ],
    )
}

#[derive(Debug, clap::Args)]
#[clap(arg_required_else_help = true, after_long_help = examples())]
pub(crate) struct Args {
    /// hkx/xml file path
    pub input: PathBuf,
    /// If specified, write to a file (If not specified, stdout)
    #[clap(short, long)]
    pub output: Option<PathBuf>,
}
