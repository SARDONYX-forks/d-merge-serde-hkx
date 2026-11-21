//! Serialize/Deserialize ClassMap
use crate::ClassMap;
use crate::{
    convert::Format,
    error::{HkxSerSnafu, Result, XmlSerSnafu},
};
use serde_hkx::{HavokSort as _, bytes::serde::hkx_header::HkxHeader};
use snafu::ResultExt as _;

/// Serialize bytes(file contents) to a file.
///
/// # Errors
/// If the information required for serialization is missing.
///
/// See `serde_hkx::errors::ser::Error` for possible errors that may occur.
pub fn to_bytes(classes: &mut ClassMap<'_>, output_format: Format) -> Result<Vec<u8>> {
    match output_format {
        Format::Win32 | Format::Amd64 => to_bytes_inner(classes, &output_header(output_format)?),
        Format::Xml => {
            let top_ptr = classes.sort_for_xml().context(XmlSerSnafu)?;
            let header = output_header(Format::Amd64)?; // Only the version is used for XML.
            let xml = serde_hkx::to_string(classes, &top_ptr, &header).context(XmlSerSnafu)?;
            Ok(xml.into_bytes())
        }

        #[cfg(feature = "json")]
        Format::Json => {
            classes.sort_for_bytes();
            let json = sonic_rs::to_string_pretty(&classes).context(crate::error::JsonSerSnafu)?;
            Ok(json.into_bytes())
        }
    }
}

/// Header of the output hkx by the havok classes version(`hk2010`/`hk2014` feature).
///
/// # Errors
/// Fallout4(hk2014) does not have 32bit(`Win32`).
#[allow(clippy::missing_const_for_fn)]
pub fn output_header(format: Format) -> Result<HkxHeader> {
    #[cfg(feature = "hk2010")]
    {
        Ok(match format {
            Format::Win32 => HkxHeader::new_skyrim_le(),
            _ => HkxHeader::new_skyrim_se(),
        })
    }

    #[cfg(all(feature = "hk2014", not(feature = "hk2010")))]
    {
        match format {
            Format::Win32 => crate::error::Unsupported32bitSnafu.fail(),
            _ => Ok(HkxHeader::new_fallout4()),
        }
    }
}

fn find_behavior_graph_index<'a>(class_map: &'a crate::ClassMap<'a>) -> Option<&'a str> {
    use ::rayon::prelude::*;
    let results: Vec<_> = class_map
        .par_iter()
        .filter(|(_, class)| matches!(class, havok_classes::Classes::hkbBehaviorGraphData(_)))
        .map(|(key, _)| key)
        .collect();

    match results.len() {
        1 => Some(results[0]),
        _ => None,
    }
}

fn to_bytes_inner<'a>(classes: &mut ClassMap<'a>, header: &HkxHeader) -> Result<Vec<u8>> {
    classes.sort_for_bytes();

    let (event_id_map, variable_id_map) = find_behavior_graph_index(classes)
        .and_then(|idx| {
            #[cfg(feature = "tracing")]
            tracing::info!("Found hkbBehaviorGraphData({idx}). Creating id maps...");
            let (event_id_map, variable_id_map) = crate::id_maker::create_maps(classes, idx)?;

            #[cfg(feature = "tracing")]
            {
                tracing::debug!("event_id_map={event_id_map:#?}");
                tracing::debug!("variable_id_map={variable_id_map:#?}");
            }
            Some((event_id_map, variable_id_map))
        })
        .unwrap_or_default();

    serde_hkx::to_bytes_with_maps(classes, header, event_id_map, variable_id_map)
        .context(HkxSerSnafu)
}
