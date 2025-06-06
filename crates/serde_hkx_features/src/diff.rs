//! Show diff between two files.

use crate::error::{Error, Result};
use crate::fs::ReadExt as _;
use serde_hkx::bytes::hexdump;
use std::path::Path;
use tokio::fs;

/// Output diff between two files string to stdout/file.
/// - `output`: If not provided, then stdout.
/// - `use_color`: ANSI color diff. (red & green)
///
/// # Errors
/// - Not found extension.
/// - Fail to read.
/// - Fail to write.
///
/// # Note
/// extension
/// - `hkx` -> Hexdump string
/// - else -> Any encode string
pub async fn write_diff<I1, I2, O>(
    old: I1,
    new: I2,
    output: Option<O>,
    use_color: bool,
) -> Result<()>
where
    I1: AsRef<Path>,
    I2: AsRef<Path>,
    O: AsRef<Path>,
{
    let old_str = read_any_to_string(old).await?;
    let new_str = read_any_to_string(new).await?;

    let diff_str = diff(old_str, new_str, use_color);
    match output {
        Some(output) => fs::write(output, &diff_str).await?,
        None => print!("{diff_str}"),
    };
    Ok(())
}

/// extension
/// - `hkx` -> Hexdump string
/// - else -> Any encode string
///
/// # Errors
/// Not found extension.
async fn read_any_to_string<I>(path: I) -> Result<String>
where
    I: AsRef<Path>,
{
    let path = path.as_ref();
    let ext = path.extension();

    if let Some(ext) = ext {
        if ext.eq_ignore_ascii_case("hkx") {
            Ok(hexdump::to_string(path.read_bytes().await?))
        } else {
            path.read_any_string().await
        }
    } else {
        Err(Error::MissingExtension {
            path: path.to_path_buf(),
        })
    }
}

/// Show diff between two files.
///
/// - `color`: ANSI color diff. (red & green)
pub fn diff(old: impl AsRef<str>, new: impl AsRef<str>, color: bool) -> String {
    match color {
        true => ::diff::colored_diff(old.as_ref(), new.as_ref()),
        false => ::diff::diff(old.as_ref(), new.as_ref()),
    }
}
