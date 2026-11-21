//! Convert hkx <-> xml
use super::progress_handler::CliProgressHandler;
use clap::ValueEnum;
use serde_hkx_features::{convert::Format, error::Result};
use std::path::{Path, PathBuf};

/// Command examples.
pub fn examples() -> String {
    use super::color::{Example, examples};
    examples(
        None,
        &[
            Example::new(
                "hkx -> xml",
                "hkxc convert --input ./defaultmale.hkx --format xml",
            ),
            Example::new(
                "xml -> hkx(64bit)",
                "hkxc convert -i ./defaultmale.xml -v amd64 --stdout --log-level trace",
            ),
            #[cfg(feature = "hk2010")]
            Example::new(
                "hkx(32bit) -> hkx(64bit)",
                r#"hkxc convert -i ./defaultmale_x86.hkx -o ./defaultmale_x64.hkx -v amd64 --log-level debug --log-file "./convert_x86_to_x64_bytes.log""#,
            ),
            #[cfg(feature = "hk2010")]
            Example::new(
                "hkx(64bit) -> hkx(32bit)",
                "hkxc convert -i ./defaultmale_x64.hkx -o ./defaultmale_x86.hkx -v win32 --log-level trace --log-file ./convert_x64_to_x86_bytes.log",
            ),
        ],
    )
}

#[derive(Debug, clap::Args)]
#[clap(arg_required_else_help = true, after_long_help = examples())]
pub(crate) struct Args {
    /// Path containing the hkx/xml file/directory
    #[clap(short, long)]
    pub input: PathBuf,
    /// Output path (If not specified, it is automatically created in the same location as the input path.)
    #[clap(short, long)]
    pub output: Option<PathBuf>,

    /// File format to output
    #[clap(short = 'v', long, ignore_case = true)]
    pub format: Format,

    /// Execution runtime: async (Tokio) or parallel (Rayon) (NOTE: Effective only when dir path is selected.)
    #[arg(short, long, ignore_case = true, default_value = "rayon")]
    pub runtime: Runtime,
}

/// Runtime options
#[derive(ValueEnum, Clone, Debug)]
pub enum Runtime {
    /// Uses Rayon for multi-threaded parallel processing & Enumerate error paths
    Rayon,
    /// Uses Tokio for async processing(M:N thread) & Enumerate error paths
    Tokio,
}

pub async fn convert<I, O>(
    input: I,
    output: Option<O>,
    format: Format,
    runtime: Runtime,
) -> Result<()>
where
    I: AsRef<Path>,
    O: AsRef<Path> + Sync,
{
    match runtime {
        Runtime::Rayon => rayon_convert(&input, output, format),
        Runtime::Tokio => tokio_convert(&input, output, format).await,
    }
}

fn rayon_convert<I, O>(input: I, output: Option<O>, format: Format) -> Result<()>
where
    I: AsRef<Path>,
    O: AsRef<Path> + Sync,
{
    serde_hkx_features::convert::rayon::convert_progress(
        input,
        output,
        format,
        CliProgressHandler::new(),
    )
}

/// convert with cli progress.
pub async fn tokio_convert<I, O>(input: I, output: Option<O>, format: Format) -> Result<()>
where
    I: AsRef<Path>,
    O: AsRef<Path>,
{
    serde_hkx_features::convert::tokio::convert_progress(
        input,
        output,
        format,
        CliProgressHandler::new(),
    )
    .await
}
