//! Show dependency tree from havok behavior state machine (hkx/xml file)
use crate::{
    ClassMap, Format,
    error::{DeSnafu, Error, Result},
    fs::ReadExt,
};
use serde_hkx::tree::HavokTree as _;
use snafu::ResultExt as _;
use std::path::Path;
use tokio::fs;

/// Output reference tree to stdout/file.
/// - `output`: If not provided, then stdout.
///
/// # Errors
/// If the extension is not `hkx` or `xml`.
pub async fn write_tree<I, O>(input: I, output: Option<O>) -> Result<()>
where
    I: AsRef<Path>,
    O: AsRef<Path>,
{
    let tree = generate(input).await?; // NOTE: With newline
    match output.as_ref() {
        Some(output) => fs::write(output, &tree).await?,
        None => print!("{tree}"),
    };
    Ok(())
}

/// Generate reference tree.
///
/// # Errors
/// If the unknown extension. (Not `hkx`, `xml`...).
pub async fn generate<P>(input: P) -> Result<String>
where
    P: AsRef<Path>,
{
    #[allow(unused_mut)] // need mut for `extra_fmt` feature
    let mut bytes = input.read_bytes().await?;

    let input = input.as_ref();
    let input_fmt = {
        let Some(input_ext) = input.extension() else {
            return Err(Error::MissingExtension {
                path: input.to_path_buf(),
            });
        };
        Format::from_extension(input_ext).map_err(|_| Error::UnsupportedExtensionPath {
            path: input.to_path_buf(),
        })?
    };

    let mut classes: ClassMap = match input_fmt {
        Format::Amd64 | Format::Win32 => serde_hkx::from_bytes(&bytes)
            .context(crate::serde::de::HkxSnafu {})
            .with_context(|_| DeSnafu {
                input: input.to_path_buf(),
            })?,
        Format::Xml => {
            let string = auto_charset::decode_to_utf8(bytes)?;
            let mut classes: ClassMap = serde_hkx::from_str(&string)
                .context(crate::serde::de::XmlSnafu {})
                .with_context(|_| DeSnafu {
                    input: input.to_path_buf(),
                })?;
            return Ok(classes.tree_for_bytes());
        }

        #[cfg(feature = "extra_fmt")]
        Format::Json => {
            use crate::types_wrapper::ClassPtrMap;

            let classes = simd_json::from_slice::<ClassPtrMap>(&mut bytes)
                .context(crate::serde::de::JsonSnafu {})
                .with_context(|_| crate::error::DeSnafu {
                    input: input.to_path_buf(),
                })?;
            classes.into_class_map()
        }
        #[cfg(feature = "extra_fmt")]
        Format::Toml => {
            use crate::types_wrapper::ClassPtrMap;

            let classes = basic_toml::from_slice::<ClassPtrMap>(&bytes)
                .context(crate::serde::de::TomlSnafu {})
                .with_context(|_| crate::error::DeSnafu {
                    input: input.to_path_buf(),
                })?;
            classes.into_class_map()
        }
    };

    Ok(classes.tree_for_bytes())
}
