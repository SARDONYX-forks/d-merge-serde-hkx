pub mod bytes;
pub mod errors;
pub mod prelude;
mod sort;
pub mod tree;
pub mod xml;

#[cfg(test)]
pub(crate) mod tests;

/// A facade around all the types we need from the `std`, `core`, and `alloc`
/// crates. This avoids elaborate import wrangling having to happen in every
/// module.
mod lib {
    mod core {
        pub use core::*;
    }

    pub use self::core::f32;
    pub use self::core::fmt;
    pub use self::core::fmt::Display;
    pub use self::core::ops::Range;
    pub use self::core::str;
    pub use self::core::str::FromStr;

    pub use std::string::{String, ToString};
}

/// Avoiding type inference by the compiler, such as `?` and `into`, can speed up compile
/// time by a small amount, so use macros to avoid type inference.
///
/// # Info
/// This is a hack I learned at serde.
macro_rules! tri {
    ($expr:expr) => {
        match $expr {
            Ok(val) => val,
            Err(err) => return Err(err),
        }
    };
}

pub(crate) use tri;

pub use crate::bytes::de::from_bytes;
pub use crate::bytes::ser::{to_bytes, to_bytes_with_maps};
pub use crate::sort::HavokSort;
pub use crate::xml::de::from_str;
pub use crate::xml::ser::to_string;

use indexmap::IndexMap;
use std::borrow::Cow;
use std::collections::HashMap;

/// Key type alias for flexibility
pub type ClassMapKey<'a> = Cow<'a, str>;
pub type GenericClassMap<'a, V, K = ClassMapKey<'a>> = IndexMap<K, V>;

/// ASCII-ignore wrapper for &str in HashSet
#[derive(Debug, Default, Clone)]
pub struct AsciiIgnore<'a>(&'a str);

impl<'a> AsciiIgnore<'a> {
    #[inline]
    pub const fn new(value: &'a str) -> Self {
        Self(value)
    }
}

impl<'a> PartialEq for AsciiIgnore<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_ignore_ascii_case(other.0)
    }
}
impl<'a> Eq for AsciiIgnore<'a> {}
impl<'a> core::hash::Hash for AsciiIgnore<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for b in self.0.as_bytes() {
            state.write_u8(b.to_ascii_lowercase());
        }
    }
}

/// `eventNames` indexes of `hkbBehaviorGraphStringData`.
///
/// # Example
/// If the nemesis id e.g. `$eventID[variableSample]$`, then `variableSample` is the key,
///
/// The value is the index of the `eventSample` element in the `variableNames` field array of the `hkbBehaviorGraphStringData` class.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct VariableIdMap<'a>(pub HashMap<AsciiIgnore<'a>, usize>);
impl VariableIdMap<'_> {
    /// Creates a new `EventIdMap`
    #[inline]
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    /// Get the index for an event name, ignoring ASCII case.
    #[inline]
    pub fn get(&self, key: &str) -> Option<usize> {
        self.0.get(&AsciiIgnore(key)).copied()
    }
}

/// `variableNames` indexes of `hkbBehaviorGraphStringData`.
///
/// # Example
/// If the nemesis id e.g. `$variableID[variableSample]$`, then `variableSample` is the key,
///
/// The value is the index of the `variableSample` element in the `variableNames` field array of the `hkbBehaviorGraphStringData` class.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct EventIdMap<'a>(pub HashMap<AsciiIgnore<'a>, usize>);

impl EventIdMap<'_> {
    /// Creates a new `EventIdMap`
    #[inline]
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    /// Get the index for an event name, ignoring ASCII case.
    #[inline]
    pub fn get(&self, key: &str) -> Option<usize> {
        self.0.get(&AsciiIgnore(key)).copied()
    }
}
