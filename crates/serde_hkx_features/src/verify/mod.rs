//! Verification of hkx -> Rust data type -> hkx conversion to original hkx

mod dir;
mod file;

pub use self::dir::verify_dir;
pub use self::file::verify_file;
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
