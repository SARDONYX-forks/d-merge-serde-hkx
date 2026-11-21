use crate::args::progress_handler::CliProgressHandler;
use serde_hkx_features::{
    error::Result,
    verify::{VerifyMode, verify_dir_with, verify_file, verify_xml_file},
};
use std::path::{Path, PathBuf};

/// Command examples.
pub fn examples() -> String {
    use super::color::{Example, examples};
    examples(
        Some(
            "In case of error, dir and file behave differently
    - dir: All error paths are displayed
    - file: Error path + diff displayed",
        ),
        &[
            Example::new(
                "Test the reproducibility of hkx",
                "hkxc verify ./defaultmale.hkx",
            ),
            Example::new(
                "Test the reproducibility of hkx files",
                r#"hkxc verify ./input --log-level info --log-file "./verify_inputs.log""#,
            ),
            Example::new(
                "Test hkx -> XML -> hkx round trip & write a report to compare with the previous run",
                "hkxc verify ./input --mode xml --report ./test_results/xml.tsv",
            ),
        ],
    )
}

#[derive(Debug, clap::Args)]
#[clap(arg_required_else_help = true, after_long_help = examples())]
pub(crate) struct Args {
    /// hkx file path or dir for which you want to verify reproducibility. It is recommended to use log in INFO mode.
    pub path: PathBuf,
    /// Verification mode.
    #[clap(long, value_enum, default_value_t = VerifyMode::Bytes)]
    pub mode: VerifyMode,
    /// (dir only) Write `OK|NG<TAB>relative path[<TAB>error kind]` lines sorted by path.
    #[clap(long)]
    pub report: Option<PathBuf>,
}

pub fn verify<P>(path: P, color: bool, mode: VerifyMode, report: Option<&Path>) -> Result<()>
where
    P: AsRef<Path>,
{
    let path = path.as_ref();

    if path.is_file() {
        println!("Verifying...");
        let res = match mode {
            VerifyMode::Bytes => verify_file(path, color),
            VerifyMode::Xml => verify_xml_file(path, color),
        };
        res.map(|_| {
            use super::color::SUCCESS;
            anstream::println!(
                "{SUCCESS}Complete hkx reproduction: {}{SUCCESS:#}",
                path.display()
            );
        })
    } else {
        verify_dir_with(path, mode, CliProgressHandler::new(), report)
    }
}

// NOTE: We don't try it except local PC because MIRI makes an error. Therefore, leave it commented out.
#[cfg(test)]
mod tests {
    use super::*;

    #[ignore = "need templates"]
    #[test]
    fn test_verify_dir() {
        let input = "../../tests/input";
        verify(input, false, VerifyMode::Bytes, None).unwrap_or_else(|err| panic!("{err}"));
    }
}
