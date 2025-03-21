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
pub use crate::bytes::ser::to_bytes;
pub use crate::sort::HavokSort;
pub use crate::xml::de::from_str;
pub use crate::xml::ser::to_string;

use indexmap::IndexMap;
use std::borrow::Cow;

/// Key type alias for flexibility
type ClassMapKey<'a> = Cow<'a, str>;
type GenericClassMap<'a, V, K = ClassMapKey<'a>> = IndexMap<K, V>;
