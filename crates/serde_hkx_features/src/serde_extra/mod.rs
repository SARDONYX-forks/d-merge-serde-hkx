pub mod ser {
    //! Serialize ClassMap with extra formats.
    use crate::convert::Format;
    #[cfg(feature = "json")]
    use crate::error::JsonSerSnafu;
    use crate::error::Result;
    #[cfg(feature = "toml")]
    use crate::error::TomlSerSnafu;
    use crate::types_wrapper::ClassPtrMap;
    use snafu::ResultExt as _;

    /// Serialize bytes(file contents) to a file.
    ///
    /// # Errors
    /// If the information required for serialization is missing.
    ///
    /// See `serde_hkx::errors::ser::Error` for possible errors that may occur.
    ///
    /// # Panics
    /// If the `OutFormat` is not an enabled extra format (`json`, `toml`).
    /// That means the API is being used incorrectly.
    pub fn to_bytes(classes: &mut ClassPtrMap<'_>, output_format: Format) -> Result<Vec<u8>> {
        let contents = match output_format {
            #[cfg(feature = "json")]
            Format::Json => sonic_rs::to_string_pretty(&classes).context(JsonSerSnafu)?,
            #[cfg(feature = "toml")]
            Format::Toml => toml::to_string(&classes).context(TomlSerSnafu)?,
            _ => unreachable!(),
        };
        Ok(contents.into_bytes())
    }
}
