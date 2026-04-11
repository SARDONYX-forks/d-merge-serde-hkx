pub mod hkx_checker;
pub mod rayon;
pub mod tokio;

use crate::error::{DeSnafu, Error, Result};
use parse_display::{Display, FromStr};
use snafu::ResultExt as _;
use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

/// An enum used to specify input/output formats
///
/// # Default
/// `Amd64`
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Display, FromStr)]
#[display(style = "camelCase")]
pub enum Format {
    /// 64bit hkx
    #[default]
    Amd64,
    /// 32bit hkx
    Win32,
    /// XML
    Xml,

    #[cfg(feature = "extra_fmt")]
    /// json
    Json,
    #[cfg(feature = "extra_fmt")]
    /// yaml
    Toml,
}

impl Format {
    /// Return the file extension corresponding to the format.
    ///
    /// # Examples
    /// ```edition2021
    /// use serde_hkx_features::convert::Format;
    ///
    /// assert_eq!(Format::Amd64.as_extension(), "hkx");
    /// assert_eq!(Format::Win32.as_extension(), "hkx");
    /// assert_eq!(Format::Xml.as_extension(), "xml");
    /// ```
    #[inline]
    pub const fn as_extension(&self) -> &str {
        match *self {
            Self::Amd64 => "hkx",
            Self::Win32 => "hkx",
            Self::Xml => "xml",

            #[cfg(feature = "extra_fmt")]
            Self::Json => "json",
            #[cfg(feature = "extra_fmt")]
            Self::Toml => "toml",
        }
    }

    /// Return current path format of this path.
    ///
    /// # Examples
    /// ```edition2021 ,no_run
    /// use serde_hkx_features::convert::Format;
    ///
    /// assert_eq!(Format::from_current_format("amd64.hkx").unwrap(), Format::Amd64);
    /// assert_eq!(Format::from_current_format("win32.hkx").unwrap(), Format::Win32);
    /// assert_eq!(Format::from_current_format("example.xml").unwrap(), Format::Xml);
    /// ```
    ///
    /// When enable `extra_fmt` feature.
    /// - `json` -> `Self::Json`
    /// - `yaml` -> `Self::Yaml`
    ///
    /// Internally, when the file extension is .hkx, the first 17 bytes of the file are examined to determine the architecture.
    ///
    /// # Errors
    /// In the case of unsupported file extensions or invalid hkx files.
    #[inline]
    pub fn from_current_format<P>(path: P) -> Result<Self>
    where
        P: AsRef<Path>,
    {
        let path = path.as_ref();
        let ext = path.extension().ok_or(Error::UnsupportedExtensionPath {
            path: path.to_path_buf(),
        })?;

        Ok(match ext {
            ext if ext.eq_ignore_ascii_case("hkx") => hkx_checker::detect_hkx_format(path)?,
            ext if ext.eq_ignore_ascii_case("xml") => Self::Xml,

            #[cfg(feature = "extra_fmt")]
            ext if ext.eq_ignore_ascii_case("json") => Self::Json,
            #[cfg(feature = "extra_fmt")]
            ext if ext.eq_ignore_ascii_case("toml") => Self::Toml,
            _ => {
                return Err(Error::UnsupportedExtensionPath {
                    path: path.to_path_buf(),
                });
            }
        })
    }

    /// Return output format from input path.
    ///
    /// # Examples
    /// ```edition2021
    /// use serde_hkx_features::convert::Format;
    ///
    /// assert_eq!(Format::infer_output_from_input("example.hkx").unwrap(), Format::Xml);
    /// assert_eq!(Format::infer_output_from_input("example.xml").unwrap(), Format::Amd64);
    /// ```
    ///
    /// When enable `extra_fmt` feature.
    /// - `json`, `yaml` -> `Self::Amd64`
    ///
    /// # Errors
    /// Unknown extension.
    #[inline]
    pub fn infer_output_from_input<P>(path: P) -> Result<Self>
    where
        P: AsRef<Path>,
    {
        let path = path.as_ref();
        let ext = path.extension().ok_or(Error::UnsupportedExtensionPath {
            path: path.to_path_buf(),
        })?;

        Ok(match ext {
            ext if ext.eq_ignore_ascii_case("hkx") => Self::Xml,
            ext if ext.eq_ignore_ascii_case("xml") => Self::Amd64,

            #[cfg(feature = "extra_fmt")]
            ext if ext.eq_ignore_ascii_case("json") => Self::Amd64,
            #[cfg(feature = "extra_fmt")]
            ext if ext.eq_ignore_ascii_case("toml") => Self::Amd64,
            _ => {
                return Err(Error::UnsupportedExtensionPath {
                    path: path.to_path_buf(),
                });
            }
        })
    }

    /// Determine format from extension.
    ///
    /// # Examples
    /// ```edition2021
    /// use serde_hkx_features::convert::Format;
    ///
    /// assert_eq!(Format::from_extension("hkx").unwrap(), Format::Amd64);
    /// assert_eq!(Format::from_extension("xml").unwrap(), Format::Xml);
    /// ```
    ///
    /// When enable `extra_fmt` feature.
    /// - `json` -> `Self::Json`
    /// - `toml` -> `Self::Toml`
    /// - `yaml` -> `Self::Yaml`
    ///
    /// # Errors
    /// Unknown extension.
    #[inline]
    pub fn from_extension<S>(ext: S) -> Result<Self>
    where
        S: AsRef<OsStr>,
    {
        let ext = ext.as_ref();
        Ok(match ext {
            ext if ext.eq_ignore_ascii_case("hkx") => Self::Amd64,
            ext if ext.eq_ignore_ascii_case("xml") => Self::Xml,

            #[cfg(feature = "extra_fmt")]
            ext if ext.eq_ignore_ascii_case("json") => Self::Json,
            #[cfg(feature = "extra_fmt")]
            ext if ext.eq_ignore_ascii_case("toml") => Self::Toml,
            _ => {
                return Err(Error::UnsupportedExtension {
                    ext: ext.to_string_lossy().to_string(),
                });
            }
        })
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// tokio & rayon common code

fn get_output_path<D, I, O>(
    input_dir: D,
    input: I,
    output_dir: &Option<O>,
    output_format: Format,
) -> Option<PathBuf>
where
    D: AsRef<Path>,
    I: AsRef<Path>,
    O: AsRef<Path>,
{
    let output_dir = output_dir.as_ref()?;

    let input_inner_dir = input.as_ref().strip_prefix(input_dir).ok()?;
    let mut output = output_dir.as_ref().join(input_inner_dir);
    output.set_extension(output_format.as_extension());
    Some(output)
}

fn filter_supported_files(entry: &jwalk::DirEntry<((), ())>) -> bool {
    let path = entry.path();

    if !path.is_file() {
        return false;
    }

    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| {
            if Format::from_extension(ext).is_err() {
                #[cfg(feature = "tracing")]
                tracing::info!("Skip this unsupported extension: {}", path.display());
                false
            } else {
                true
            }
        })
}

fn get_supported_files(input_dir: &Path) -> Vec<PathBuf> {
    jwalk::WalkDir::new(input_dir)
        .into_iter()
        .filter_map(|entry| entry.ok())
        .filter(filter_supported_files)
        .map(|entry| entry.path())
        .collect()
}

/// Deserializes HKX/XML/JSON/TOML bytes into a [`ClassMap`], then applies one
/// of two caller-supplied closures depending on the input format, and returns
/// the result as `T`.
///
/// Two closures are required because XML decoding produces a [`ClassMap`] that
/// borrows from a locally-owned [`String`], while all other formats borrow
/// directly from the input `bytes`. Splitting the paths allows the caller to
/// handle each lifetime separately (e.g. calling [`Hkanno::into_static`] on
/// the XML path while returning a borrowed [`Hkanno`] on the binary path).
///
/// # Arguments
/// * `bytes`     - Raw input file bytes. Consumed by the function.
/// * `input`     - Source path used **only** for error context and extension-based format detection; no I/O is performed.
/// * `on_xml`    - Called when the input format is XML. The [`ClassMap`] borrows from a function-local [`String`].
/// * `on_other`  - Called for all other formats (HKX, JSON, TOML). The [`ClassMap`] borrows from `bytes`.
///
/// # Returns
/// The `T` produced by whichever closure was invoked.
///
/// # Errors
/// * [`Error::MissingExtension`]         – `input` has no file extension.
/// * [`Error::UnsupportedExtensionPath`] – The extension is not a recognized format.
/// * [`Error::De`]                       – Deserialization of the input bytes failed.
/// * Any error returned by `on_xml` or `on_other` is propagated as-is.
pub fn process_serde_with<'a, I, F, G, T>(
    bytes: &'a [u8],
    input: I,
    on_xml: G,   // ClassMap borrows from local String, must return T directly
    on_other: F, // ClassMap borrows from bytes ('a), T can carry 'a
) -> Result<T>
where
    I: AsRef<Path>,
    F: FnOnce(crate::ClassMap<'a>) -> Result<T>,
    G: FnOnce(crate::ClassMap<'_>) -> Result<T>, // '_ = local String lifetime
{
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

    let classes = match input_fmt {
        Format::Amd64 | Format::Win32 => serde_hkx::from_bytes(bytes)
            .context(crate::serde::de::HkxSnafu {})
            .with_context(|_| DeSnafu {
                input: input.to_path_buf(),
            })?,
        Format::Xml => {
            let string = auto_charset::decode_str_to_utf8(bytes)?;
            let classes = serde_hkx::from_str(&string)
                .context(crate::serde::de::XmlSnafu {})
                .with_context(|_| DeSnafu {
                    input: input.to_path_buf(),
                })?;

            return on_xml(classes);
        }
        #[cfg(feature = "extra_fmt")]
        Format::Json => {
            use crate::types_wrapper::ClassPtrMap;
            let classes = sonic_rs::from_slice::<ClassPtrMap>(bytes)
                .context(crate::serde::de::JsonSnafu {})
                .with_context(|_| crate::error::DeSnafu {
                    input: input.to_path_buf(),
                })?;
            classes.into_class_map()
        }
        #[cfg(feature = "extra_fmt")]
        Format::Toml => {
            use crate::types_wrapper::ClassPtrMap;
            let classes = basic_toml::from_slice::<ClassPtrMap>(bytes)
                .context(crate::serde::de::TomlSnafu {})
                .with_context(|_| crate::error::DeSnafu {
                    input: input.to_path_buf(),
                })?;
            classes.into_class_map()
        }
    };

    on_other(classes)
}

/// bytes(input) -> output_format
pub(crate) fn process_serde<I>(
    bytes: Vec<u8>,
    input: I,
    output_format: Format,
) -> Result<Vec<u8>, Error>
where
    I: AsRef<Path>,
{
    let input = input.as_ref();

    process_serde_with(
        &bytes,
        input,
        |mut classes| {
            match output_format {
                Format::Amd64 | Format::Win32 | Format::Xml => {
                    crate::serde::ser::to_bytes(&mut classes, output_format)
                }
                #[cfg(feature = "extra_fmt")]
                Format::Json | Format::Toml => {
                    let mut classes = crate::types_wrapper::ClassPtrMap::from_class_map(classes);
                    crate::serde_extra::ser::to_bytes(&mut classes, output_format)
                }
            }
            .with_context(|_| crate::error::SerSnafu {
                input: input.to_path_buf(),
            })
        },
        |mut classes| {
            let bytes = match output_format {
                Format::Amd64 | Format::Win32 | Format::Xml => {
                    crate::serde::ser::to_bytes(&mut classes, output_format).with_context(|_| {
                        crate::error::SerSnafu {
                            input: input.to_path_buf(),
                        }
                    })?
                }
                #[cfg(feature = "extra_fmt")]
                Format::Json | Format::Toml => {
                    let mut classes = crate::types_wrapper::ClassPtrMap::from_class_map(classes);
                    crate::serde_extra::ser::to_bytes(&mut classes, output_format).with_context(
                        |_| crate::error::SerSnafu {
                            input: input.to_path_buf(),
                        },
                    )?
                }
            };

            Ok(bytes)
        },
    )
}
