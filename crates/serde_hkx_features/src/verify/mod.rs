//! Verification of hkx -> Rust data type -> hkx conversion to original hkx

mod debug_cmp;
mod dir;
mod file;
mod xml;

pub use self::dir::{verify_dir, verify_dir_with};
pub use self::file::verify_file;
pub use self::xml::{verify_xml_bytes, verify_xml_file};

/// Verification mode
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum VerifyMode {
    /// hkx -> hkx must be the same bytes.
    #[default]
    Bytes,
    /// hkx -> XML -> hkx round trip.(XML is lossy by the specification, so checks semantic equality.)
    Xml,
}
use crate::progress::ProgressHandler;
use crate::{
    ClassMap,
    error::{HkxDeSnafu, HkxSerSnafu, Result},
};
use serde_hkx::bytes::serde::hkx_header::HkxHeader;
use snafu::ResultExt as _;
use std::path::Path;

/// Checks reproduction for file/dir hkx.
///
/// # Errors
/// If an error occurs, return a diff showing the location of each error.(If `input` is one file path)
pub fn verify<I, P>(input: I, color: bool, progress: P) -> Result<()>
where
    I: AsRef<Path>,
    P: ProgressHandler + Send + Sync,
{
    let path = input.as_ref();

    if path.is_file() {
        verify_file(path, color)
    } else {
        verify_dir(path, progress)
    }
}

pub(super) fn verify_inner<I>(input: I) -> Result<(Vec<u8>, Vec<u8>)>
where
    I: AsRef<Path>,
{
    let expected_bytes = std::fs::read(input)?;

    let actual_bytes = {
        let classes: ClassMap = serde_hkx::from_bytes(&expected_bytes).context(HkxDeSnafu)?;
        let header = HkxHeader::from_bytes(&expected_bytes).context(HkxDeSnafu)?;
        serde_hkx::to_bytes(&classes, &header).context(HkxSerSnafu)?
    };

    Ok((expected_bytes, actual_bytes))
}
