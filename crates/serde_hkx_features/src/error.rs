//! `serde_hkx_features`'s errors
//!
//! # Path policy
//! Errors do not carry the path of the file being processed.
//! The caller that knows the path (e.g. CLI) attaches it with [`Error::with_path`].
//!
//! Only operations that handle multiple paths internally (dir conversion/verify, diff)
//! attach the path themselves.
use snafu::Location;
use std::path::{Path, PathBuf};

#[cfg(not(any(feature = "json", feature = "toml")))]
const SUPPORTED_EXTENSIONS: &str = "`.hkx`, `.xml`";
#[cfg(all(feature = "json", not(feature = "toml")))]
const SUPPORTED_EXTENSIONS: &str = "`.hkx`, `.xml`, `.json`";
#[cfg(all(not(feature = "json"), feature = "toml"))]
const SUPPORTED_EXTENSIONS: &str = "`.hkx`, `.xml`, `.toml`";
#[cfg(all(feature = "json", feature = "toml"))]
const SUPPORTED_EXTENSIONS: &str = "`.hkx`, `.xml`, `.json`, `.toml`";

/// `serde_hkx_features` error
#[allow(clippy::enum_variant_names, clippy::use_self)]
#[derive(Debug, snafu::Snafu)]
#[snafu(visibility(pub))]
pub enum Error {
    /// Wrap an error with the path of the file being processed.
    #[snafu(display("{}:\n{source}", path.display()))]
    WithPath { path: PathBuf, source: Box<Error> },

    // ---------------------------------------------------------------------------------------------
    // Path/Format
    /// Unsupported extension.
    #[snafu(display(
        "The only supported extensions are {SUPPORTED_EXTENSIONS}. But got: `{ext}`."
    ))]
    UnsupportedExtension { ext: String },

    /// This path has a missing extension.
    #[snafu(display("This path has a missing extension."))]
    MissingExtension,

    /// Please specify the output path.(OS cannot output bytes as stdout.)
    #[snafu(display("Please specify the output path.(OS cannot output bytes as stdout.)"))]
    InvalidStdout,

    // ---------------------------------------------------------------------------------------------
    // hkx header check
    /// Tag file(.hkt) disguised as hkx.
    #[snafu(display("serde-hkx does not currently support tag files."))]
    UnsupportedTagFile,

    /// Invalid hkx magic numbers.
    #[snafu(display(
        "HKX file did not have the expected Havok magic numbers. \
        Expected magic=[0x57, 0xe0, 0xe0, 0x57, 0x10, 0xc0, 0xc0, 0x10, ...], \
        but got {magic_bytes:x?}. \
        This file is not a valid Havok animation or may be from an unsupported version."
    ))]
    HkxInvalidMagic { magic_bytes: [u8; 17] },

    /// Invalid hkx pointer size.
    #[snafu(display(
        "HKX pointer size check failed. \
        Expected pointer size 4/8-byte, but could not determine a valid header or got {actual}-byte. \
        The HKX may be malformed or from an incompatible platform."
    ))]
    HkxInvalidHeader { actual: u8 },

    // ---------------------------------------------------------------------------------------------
    // Serialize/Deserialize
    /// hkx deserialize error
    #[snafu(display("{location}: hkx Deserialize Error: {source}"))]
    HkxDe {
        source: serde_hkx::errors::de::Error,
        #[snafu(implicit)]
        location: Location,
    },
    /// hkx serialize error
    #[snafu(display("{location}: hkx Serialize Error: {source}"))]
    HkxSer {
        source: serde_hkx::errors::ser::Error,
        #[snafu(implicit)]
        location: Location,
    },

    /// XML deserialize error
    #[snafu(display("{location}: XML Deserialize Error: {source}"))]
    XmlDe {
        source: serde_hkx::errors::de::Error,
        #[snafu(implicit)]
        location: Location,
    },
    /// XML serialize error
    #[snafu(display("{location}: XML Serialize Error: {source}"))]
    XmlSer {
        source: serde_hkx::errors::ser::Error,
        #[snafu(implicit)]
        location: Location,
    },

    /// Json deserialize error
    #[cfg(feature = "json")]
    #[snafu(display("{location}: Json Deserialize Error: {source}"))]
    JsonDe {
        source: sonic_rs::Error,
        #[snafu(implicit)]
        location: Location,
    },
    /// Json serialize error
    #[cfg(any(feature = "json", feature = "json_schema"))]
    #[snafu(display("{location}: Json Serialize Error: {source}"))]
    JsonSer {
        source: sonic_rs::Error,
        #[snafu(implicit)]
        location: Location,
    },

    /// Toml deserialize error
    #[cfg(feature = "toml")]
    #[snafu(display("{location}: Toml Deserialize Error: {source}"))]
    TomlDe {
        source: toml::de::Error,
        #[snafu(implicit)]
        location: Location,
    },
    /// Toml serialize error
    #[cfg(feature = "toml")]
    #[snafu(display("{location}: Toml Serialize Error: {source}"))]
    TomlSer {
        source: toml::ser::Error,
        #[snafu(implicit)]
        location: Location,
    },

    // ---------------------------------------------------------------------------------------------
    // Reproduce
    /// Reproduce file error
    #[snafu(display("Failed to reproduce (-: input / +: output):\n{diff}"))]
    FailedReproduceFile { diff: String },

    /// The output bytes did not match the input bytes.
    #[snafu(display("Failed to reproduce: output bytes do not match input bytes."))]
    ReproduceMismatch,

    // ---------------------------------------------------------------------------------------------
    // Multiple paths
    /// dir conversion error
    #[snafu(display("Failed to convert files (error count: {}/{total_files}):\n{}", errors.len(), display_errors(errors)))]
    FailedConvertFiles {
        total_files: usize,
        errors: Vec<(PathBuf, Error)>,
    },

    /// dir reproduce error
    #[snafu(display("Failed to reproduce files (error count: {}/{total_files}):\n{}", errors.len(), display_errors(errors)))]
    FailedReproduceFiles {
        total_files: usize,
        errors: Vec<(PathBuf, Error)>,
    },

    // ---------------------------------------------------------------------------------------------
    // External
    /// Standard io error
    #[snafu(transparent)]
    Io { source: std::io::Error },

    /// Tracing log error
    #[cfg(feature = "tracing")]
    #[snafu(transparent)]
    Tracing {
        source: tracing::subscriber::SetGlobalDefaultError,
    },

    /// Async task join error
    #[snafu(transparent)]
    ThreadJoin { source: tokio::task::JoinError },
}

impl Error {
    /// Wrap this error with the path of the file being processed.
    #[inline]
    pub fn with_path<P>(self, path: P) -> Self
    where
        P: AsRef<Path>,
    {
        Self::WithPath {
            path: path.as_ref().to_path_buf(),
            source: Box::new(self),
        }
    }
}

fn display_errors(errors: &[(PathBuf, Error)]) -> String {
    use core::fmt::Write as _;

    let mut out = String::new();
    for (path, err) in errors {
        let _ = writeln!(out, "- {}:\n  {err}", path.display());
    }
    out
}

/// `Result` for `serde_hkx_features` crate.
pub type Result<T, E = Error> = core::result::Result<T, E>;
