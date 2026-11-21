//! hkx -> XML -> hkx round trip verification.
//!
//! hkx -> XML is lossy by the specification of Havok XML.
//! - float: `{:.06}`
//! - `Matrix3`/`Rotation`/`QsTransform`/`Transform`: `w` of each `Vector4` is not written.
//!
//! So comparing bytes(`hkx -> XML -> hkx` == `hkx`) can't detect XML bugs. Instead, check the followings.
//!
//! 1. `xml_fixed_point`: `XML(A)` == `XML(from_xml(XML(A)))`
//!    - Detects the information that is written to XML but not read(or read differently).
//! 2. `xml_semantic`: `A`(from hkx) == `B`(from XML) except for the above lossy parts.
//!    - Detects the information that is not written to XML(or written wrongly).
//! 3. `xml_to_bytes`: `B` == `from_hkx(to_hkx(B))` strictly.
//!    - Detects the bytes (de)serializer bugs for the classes from XML.(e.g. pointer order, sorting)
use super::debug_cmp::{CmpOptions, compare};
use crate::{
    ClassMap, diff,
    error::{HkxDeSnafu, HkxSerSnafu, Result, XmlDeSnafu, XmlRoundTripMismatchSnafu, XmlSerSnafu},
};
use serde_hkx::{HavokSort as _, bytes::serde::hkx_header::HkxHeader};
use snafu::ResultExt as _;
use std::{collections::HashMap, path::Path};

/// Verify hkx -> XML -> hkx round trip of a hkx file.
///
/// # Errors
/// - Failed to (de)serialize.
/// - [`crate::error::Error::XmlRoundTripMismatch`]
pub fn verify_xml_file<P>(path: P, color: bool) -> Result<()>
where
    P: AsRef<Path>,
{
    let bytes = std::fs::read(path)?;
    verify_xml_bytes(&bytes, color)
}

/// Verify hkx -> XML -> hkx round trip of hkx bytes.
///
/// # Errors
/// - Failed to (de)serialize.
/// - [`crate::error::Error::XmlRoundTripMismatch`]
pub fn verify_xml_bytes(bytes: &[u8], color: bool) -> Result<()> {
    let header = HkxHeader::from_bytes(bytes).context(HkxDeSnafu)?;
    let to_xml = |classes: &mut ClassMap| -> Result<String> {
        let top_ptr = classes.sort_for_xml().context(XmlSerSnafu)?;
        serde_hkx::to_string(classes, &top_ptr, &header).context(XmlSerSnafu)
    };

    // A: hkx -> classes
    let mut a: ClassMap = serde_hkx::from_bytes(bytes).context(HkxDeSnafu)?;
    let xml_a = to_xml(&mut a)?;

    // B: XML -> classes
    let mut b: ClassMap = serde_hkx::from_str(&xml_a).context(XmlDeSnafu)?;
    let xml_b = to_xml(&mut b)?;

    // 1. XML fixed point
    if xml_a != xml_b {
        return XmlRoundTripMismatchSnafu {
            check: "xml_fixed_point",
            detail: diff::diff(&xml_a, &xml_b, color),
        }
        .fail();
    }

    // 2. hkx classes == XML classes (except for lossy parts)
    let opt = CmpOptions {
        float_tolerance: true,
        ignore_xml_lost_w: true,
    };
    if let Err(detail) = compare_maps(&a, &b, opt, None) {
        return XmlRoundTripMismatchSnafu {
            check: "xml_semantic",
            detail,
        }
        .fail();
    }

    // 3. XML classes -> hkx -> classes(strict)
    let len = b.len();
    b.sort_for_bytes();
    if b.len() != len {
        let dropped: Vec<_> = a.keys().filter(|key| !b.contains_key(*key)).collect();
        return XmlRoundTripMismatchSnafu {
            check: "xml_to_bytes",
            detail: format!("`sort_for_bytes` dropped unreferenced classes: {dropped:?}"),
        }
        .fail();
    }
    let bytes_b = serde_hkx::to_bytes(&b, &header).context(HkxSerSnafu)?;
    let c: ClassMap = serde_hkx::from_bytes(&bytes_b).context(HkxDeSnafu)?;
    if b.len() != c.len() {
        return XmlRoundTripMismatchSnafu {
            check: "xml_to_bytes",
            detail: format!("class count: {} -> {}", b.len(), c.len()),
        }
        .fail();
    }
    // Pointer names are renumbered in the order of bytes.
    let rename: HashMap<String, String> = b
        .keys()
        .zip(c.keys())
        .map(|(b, c)| (b.to_string(), c.to_string()))
        .collect();
    let pairs = b
        .iter()
        .zip(c.iter())
        .map(|((b_key, b), (c_key, c))| (Entry::new(b_key, b), Entry::new(c_key, c)));
    if let Err(detail) = compare_pairs(pairs, CmpOptions::default(), Some(&rename)) {
        return XmlRoundTripMismatchSnafu {
            check: "xml_to_bytes",
            detail,
        }
        .fail();
    }

    Ok(())
}

/// Compare classes that have the same keys.
fn compare_maps(
    left: &ClassMap,
    right: &ClassMap,
    opt: CmpOptions,
    rename: Option<&HashMap<String, String>>,
) -> Result<(), String> {
    if left.len() != right.len() || left.keys().any(|k| !right.contains_key(k)) {
        let missing: Vec<_> = left.keys().filter(|k| !right.contains_key(*k)).collect();
        let extra: Vec<_> = right.keys().filter(|k| !left.contains_key(*k)).collect();
        return Err(format!(
            "class keys differ: missing {missing:?}, extra {extra:?}"
        ));
    }
    let pairs = left.iter().filter_map(|(key, l)| {
        let (r_key, r) = right.get_key_value(key)?;
        Some((Entry::new(key, l), Entry::new(r_key, r)))
    });
    compare_pairs(pairs, opt, rename)
}

/// (key, class name, `Debug` string)
struct Entry {
    key: String,
    name: &'static str,
    debug: String,
}

impl Entry {
    fn new(key: &str, class: &havok_classes::Classes<'_>) -> Self {
        use havok_serde::HavokClass as _;
        Self {
            key: key.to_string(),
            name: class.name(),
            debug: format!("{class:?}"),
        }
    }
}

fn compare_pairs(
    pairs: impl Iterator<Item = (Entry, Entry)>,
    opt: CmpOptions,
    rename: Option<&HashMap<String, String>>,
) -> Result<(), String> {
    for (l, r) in pairs {
        if let Err(mismatch) = compare(&l.debug, &r.debug, opt, rename) {
            return Err(format!("{} -> {}({}): {mismatch}", l.key, r.key, l.name));
        }
    }
    Ok(())
}
