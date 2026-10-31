//! Check the HKX file header to detect its format.
use crate::{
    Format,
    error::{Error, Result},
};
use std::path::Path;

/// Check the HKX file header to detect its format(`Win32`/`Amd64`).
///
/// # Errors
/// - If tag file magic number
/// - If hkx magic number is invalid
pub fn detect_hkx_format(input_path: &Path) -> Result<Format> {
    let header = {
        let mut header = [0_u8; 17];
        let mut file = std::fs::File::open(input_path)?;
        std::io::Read::read_exact(&mut file, &mut header)?;
        header
    };

    // Actually, both LE and SE versions of hkt can be loaded, and there are mods disguised as hkx files. Example: Ride Sharing's `rsh_horsepinion.hkx`
    // This is the processing for that.
    // NOTE: Tag files cannot be converted by serde_hkx, so they are skipped.
    let is_tag_file = {
        /// Tag file(.hkt) magic bytes
        const EXPECTED_MAGIC: [u8; 8] = [
            0x1E, 0x0D, 0xB0, 0xCA, // magic0
            0xCE, 0xFA, 0x11, 0xD0, // magic1
        ];
        header[0..8] == EXPECTED_MAGIC
    };
    if is_tag_file {
        return Err(Error::UnsupportedTagFile);
    }

    let is_hkx = {
        /// .hkx magic bytes
        const EXPECTED_MAGIC: [u8; 8] = [
            0x57, 0xE0, 0xE0, 0x57, // magic0
            0x10, 0xC0, 0xC0, 0x10, // magic1
        ];
        header[0..8] == EXPECTED_MAGIC
    };
    if !is_hkx {
        return Err(Error::HkxInvalidMagic {
            magic_bytes: header,
        });
    }

    // check ptr size
    let ptr_size = header[16];
    let current_format = match ptr_size {
        4 => Format::Win32,
        8 => Format::Amd64,
        _ => {
            return Err(Error::HkxInvalidHeader { actual: ptr_size });
        }
    };

    Ok(current_format)
}
