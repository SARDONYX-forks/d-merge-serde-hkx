use crate::{NULL_STR, StringPtr, lib::*};

/// # CString
/// - binary data(.hkx): null-terminated string
/// - XML: `&str`
///
/// # C++ Info
/// - name: `char*`
/// - type_size: ` 4`(x86)/` 8`(x86_64)
/// - align: ` 4`(x86)/` 8`(x86_64)
///
/// # Null representation
/// - hkx: It will not be written to the binary data.
/// - XML: `\u{2400}`.
/// - Rust: If it is null then [`Option::None`].(To eliminate the risk of always being null ptr by type)
///
/// # Deserialization patterns
/// - hkx(`Vec<u8>`)   -> Struct([`str`] in `Cow<'_, str>`) => non copy
/// - xml([`String`])  -> Struct([`str`] in `Cow<'_, str>`) => non copy
/// - json: [`String`] -> Struct([`str`] in `Cow<'_, str>`) => non copy
///
/// # Serialization is alloc
/// Struct([`str`]) -> (alloc [`String`])
///
/// [`str`]: https://doc.rust-lang.org/std/primitive.str.html
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "json_schema", schemars(transparent))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CString<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    inner: Option<Cow<'a, str>>,
}

impl<'a> CString<'a> {
    /// Create a new `CString`
    #[inline]
    pub const fn new(inner: Option<Cow<'a, str>>) -> Self {
        Self { inner }
    }

    /// Get inner value.
    #[inline]
    pub fn into_inner(self) -> Option<Cow<'a, str>> {
        self.inner
    }

    /// Get inner ref.
    #[inline]
    pub const fn get_ref(&self) -> &Option<Cow<'a, str>> {
        &self.inner
    }

    /// Cast [`str`] with non copying.
    #[allow(clippy::should_implement_trait)]
    #[inline]
    pub const fn from_str(s: &'a str) -> Self {
        Self {
            inner: Some(Cow::Borrowed(s)),
        }
    }

    /// Inner to [`Self`]
    #[inline]
    pub const fn from_option(s: Option<Cow<'a, str>>) -> Self {
        Self { inner: s }
    }

    /// Null pointer or not?
    ///
    /// This indicates that no binary data was present.
    #[inline]
    pub const fn is_null(&self) -> bool {
        self.get_ref().is_none()
    }

    /// Should the data pointed to by the pointer be written to the binary data or not?
    ///
    /// This is an invalid value or not.
    #[inline]
    pub fn should_write_binary(&self) -> bool {
        match self.get_ref() {
            Some(s) => {
                if s.is_empty() || s == NULL_STR {
                    return false;
                };
                true
            }
            _ => false,
        }
    }
}

impl fmt::Display for CString<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = self.inner.as_ref().map_or(NULL_STR, |s| s.as_ref());
        write!(f, "{s}")
    }
}

impl<'a> From<&'a str> for CString<'a> {
    #[inline]
    fn from(value: &'a str) -> Self {
        Self::from_str(value)
    }
}

impl<'a> From<CString<'a>> for StringPtr<'a> {
    #[inline]
    fn from(value: CString<'a>) -> Self {
        Self::from_option(value.into_inner())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::borrow::Cow;

    #[test]
    fn test_is_null() {
        let cstring = CString::new(None);
        assert!(cstring.is_null());

        let cstring_with_str = CString::from_str("test");
        assert!(!cstring_with_str.is_null());
    }

    #[test]
    fn test_should_write_binary() {
        let cstring = CString::new(None);
        assert!(!cstring.should_write_binary());

        let empty_cstring = CString::from_str("");
        assert!(!empty_cstring.should_write_binary());

        let null_str_cstring = CString::from_str(NULL_STR);
        assert!(!null_str_cstring.should_write_binary());

        let valid_cstring = CString::from_str("valid");
        assert!(valid_cstring.should_write_binary());
    }

    #[test]
    fn test_display() {
        let null_cstring = CString::new(None);
        assert_eq!(format!("{}", null_cstring), NULL_STR);

        let cstring = CString::from_str("display test");
        assert_eq!(format!("{}", cstring), "display test");
    }

    #[test]
    fn test_from_str_conversion() {
        let cstring: CString = "test string".into();
        assert_eq!(cstring.get_ref(), &Some(Cow::Borrowed("test string")));
    }

    #[test]
    fn test_to_string_ptr() {
        let cstring = CString::from_str("test");
        let string_ptr: StringPtr = cstring.clone().into();
        assert_eq!(string_ptr.get_ref(), cstring.get_ref());
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_serialization() {
        let cstring = CString::from_str("serialize me");
        let serialized = serde_json::to_string(&cstring).unwrap();
        assert_eq!(serialized, "\"serialize me\"");

        let null_cstring = CString::new(None);
        let serialized_null = serde_json::to_string(&null_cstring).unwrap();
        assert_eq!(serialized_null, "null");
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_deserialization() {
        let json_data = "\"deserialize me\"";
        let deserialized: CString = serde_json::from_str(json_data).unwrap();
        assert_eq!(
            deserialized.get_ref(),
            &Some(Cow::Borrowed("deserialize me"))
        );

        let json_null = "null";
        let deserialized_null: CString = serde_json::from_str(json_null).unwrap();
        assert!(deserialized_null.is_null());
    }
}
