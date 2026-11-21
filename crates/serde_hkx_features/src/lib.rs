// Havok classes version: `hk2010`(SkyrimLE/SE) or `hk2014`(Fallout4).
// NOTE: These are exclusive because the classes of the same name are different.
#[cfg(all(feature = "hk2010", feature = "hk2014"))]
compile_error!("`hk2010` and `hk2014` features are exclusive. Enable only one of them.(Check `default-features = false`)");
#[cfg(not(any(feature = "hk2010", feature = "hk2014")))]
compile_error!("Either `hk2010` or `hk2014` feature must be enabled.");
// Use hk2014 classes as `havok_classes`.(`extern crate` at root adds it to the extern prelude of all modules.)
#[cfg(all(feature = "hk2014", not(feature = "hk2010")))]
extern crate havok_classes_hk2014 as havok_classes;

pub mod convert;
pub mod diff;
pub mod dump;
pub mod error;
pub mod fs;
pub mod id_maker;
#[cfg(feature = "json_schema")]
pub mod json_schema_gen;
pub mod progress;
pub mod serde;
#[cfg(feature = "json")]
pub mod serde_extra;
pub mod tree;
#[cfg(any(feature = "json", feature = "json_schema"))]
pub mod types_wrapper;
pub mod verify;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Module to allow import of the most used and useful APIs in one place.
// NOTE: Keep it to top-level public because it is not easy to use if prelude as `Result`.
pub use crate::convert::{Format, tokio::convert};
pub use crate::error::Result;
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/// - key: class index(e.g `#0001`, `#slide$1`)
/// - value: C++ Class
pub type ClassMap<'a> = serde_hkx::GenericClassMap<'a, havok_classes::Classes<'a>>;

// https://doc.rust-lang.org/rustdoc/write-documentation/documentation-tests.html#include-items-only-when-collecting-doctests

#[doc = include_str!("../../../README.md")]
#[cfg(doctest)]
pub struct ReadmeDoctests;
