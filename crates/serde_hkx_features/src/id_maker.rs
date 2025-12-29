//! Create event_id & variable map from `hkbBehaviorGraphStringData`.
//!
//! [`Rust Playground`](https://play.rust-lang.org/?version=stable&mode=debug&edition=2024&gist=235411992c969021e3540eac07db2f30)
//!
//! This module provides functions to deduplicate event and variable names
//! in a `ClassMap` and create maps (`EventIdMap`, `VariableIdMap`) pointing
//! to the internal string data.
//!
//! # Important design note
//!
//! We intentionally separate the functionality into two functions:
//!
//! 1. [`dedup_event_variables`] — takes a mutable borrow (`&mut ClassMap`) to
//!    deduplicate names and synchronize the associated info vectors in-place.
//! 2. [`create_maps`] — takes an immutable borrow (`&ClassMap`) to create maps
//!    referencing the internal `&str` data.
//!
//! The reason for this separation is **Rust's borrow checker rules**. If we tried
//! to deduplicate and create maps in a single function, the returned maps would
//! hold references to the internal strings while `class_map` is still mutably
//! borrowed. This would make it impossible to use the maps and access the
//! `ClassMap` afterward due to conflicting borrows (`&mut` vs `&`).
//!
//! By splitting the operations, we:
//! - Ensure the mutable borrow ends after deduplication.
//! - Allow immutable borrows to create the maps referencing internal data.
//! - Keep the internal `&str` references in the maps valid as long as the
//!   `ClassMap` lives.
use havok_classes::Classes;
use havok_types::StringPtr;
use rayon::prelude::*;
use serde_hkx::{AsciiIgnore, EventIdMap, VariableIdMap};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use crate::ClassMap;

/// Deduplicates event and variable names in-place within a `ClassMap`.
///
/// Removes duplicates from `eventNames`/`variableNames` and keeps the
/// associated info vectors (`eventInfos`, `variableInfos`, `wordVariableValues`)
/// aligned.
///
/// This function **checks vector lengths before deduplication**. If there is a
/// mismatch, it returns an error. Both event and variable length errors can be
/// reported simultaneously.
///
/// Must be called **before** [`create_maps`] if you intend to create maps
/// referencing internal `&str` data.
///
/// # Notes
/// - The order of `ClassMap` may change; call `sort_for_bytes` afterward if needed.
/// - Only mutates the `ClassMap`; does not create maps or return references.
///
/// # Errors
/// After an error occurs, the class_map has its processed data deleted to avoid ownership errors.
/// Therefore, it cannot be converted to hkx in that state.
///
/// Returns `DedupError` if:
/// - `BehaviorGraphData`, `BehaviorGraphStringData`, or `VariableValueSet` is missing.
/// - Event or variable vector lengths do not match (can report both simultaneously).
///
/// # Example
/// ```no_run
/// use serde_hkx_features::id_maker::{dedup_event_variables, DedupError};
/// use serde_hkx_features::ClassMap;
///
/// let mut class_map: ClassMap = ClassMap::new();
///
/// // Fill class_map with hkbBehaviorGraphStringData and hkbBehaviorGraphData...
///
/// dedup_event_variables(&mut class_map, "#0002")
///     .expect("Deduplication should succeed");
/// ```
pub fn dedup_event_variables<'a>(
    class_map: &mut ClassMap<'a>,
    behavior_graph_index: Cow<'a, str>,
) -> Result<(), DedupError> {
    let mut graph_data = match class_map.swap_remove(&behavior_graph_index) {
        Some(Classes::hkbBehaviorGraphData(g)) => g,
        _ => {
            return Err(DedupError::BehaviorGraphDataMissing {
                index: behavior_graph_index.to_string(),
            });
        }
    };
    let (string_data_index, binding_set_index) = (
        graph_data.m_stringData.to_static().into_inner(),
        graph_data.m_variableInitialValues.to_static().into_inner(),
    );

    let mut string_data = match class_map.swap_remove(&string_data_index) {
        Some(Classes::hkbBehaviorGraphStringData(g)) => g,
        _ => {
            return Err(DedupError::BehaviorGraphStringDataMissing {
                index: string_data_index,
            });
        }
    };

    let mut binding_set = match class_map.swap_remove(&binding_set_index) {
        Some(Classes::hkbVariableValueSet(g)) => g,
        _ => {
            return Err(DedupError::VariableValueSetMissing {
                index: binding_set_index,
            });
        }
    };

    dedup_len_check(
        string_data_index.as_ref(),
        &behavior_graph_index,
        binding_set_index.as_ref(),
        string_data.m_eventNames.len(),
        graph_data.m_eventInfos.len(),
        string_data.m_variableNames.len(),
        graph_data.m_variableInfos.len(),
        binding_set.m_wordVariableValues.len(),
    )?;

    rayon::join(
        || {
            dedup_names_and_infos_in_place(
                &mut string_data.m_eventNames,
                &mut graph_data.m_eventInfos,
            );
        },
        || {
            dedup_three_way(
                &mut string_data.m_variableNames,
                &mut graph_data.m_variableInfos,
                &mut binding_set.m_wordVariableValues,
            );
        },
    );

    class_map.insert(
        behavior_graph_index,
        Classes::hkbBehaviorGraphData(graph_data),
    );
    class_map.insert(
        string_data_index,
        Classes::hkbBehaviorGraphStringData(string_data),
    );
    class_map.insert(binding_set_index, Classes::hkbVariableValueSet(binding_set));

    Ok(())
}

/// before dedup
#[allow(clippy::too_many_arguments)]
fn dedup_len_check(
    string_data_index: &str,
    behavior_graph_index: &str,
    binding_set_index: &str,

    event_names_len: usize,
    event_infos_len: usize,

    variable_names_len: usize,
    variable_infos_len: usize,
    word_values_len: usize,
) -> Result<(), DedupError> {
    let event_error = if event_names_len != event_infos_len {
        Some(DedupError::EventLengthMismatch {
            string_data_index: string_data_index.to_string(),
            behavior_graph_index: behavior_graph_index.to_string(),
            names: event_names_len,
            infos: event_infos_len,
        })
    } else {
        None
    };

    let variable_error =
        if variable_names_len != variable_infos_len || variable_names_len != word_values_len {
            Some(DedupError::VariableLengthMismatch {
                string_data_index: string_data_index.to_string(),
                behavior_graph_index: behavior_graph_index.to_string(),
                binding_set_index: binding_set_index.to_string(),
                names: variable_names_len,
                infos: variable_infos_len,
                word_values: word_values_len,
            })
        } else {
            None
        };

    match (event_error, variable_error) {
        (None, None) => Ok(()),
        (Some(e), None) => Err(e),
        (None, Some(v)) => Err(v),
        (Some(e), Some(v)) => Err(DedupError::BothLengthErrors {
            event_error: Box::new(e),
            variable_error: Box::new(v),
        }),
    }
}

/// Errors that may occur during duplicate removal
#[derive(Debug, snafu::Snafu)]
pub enum DedupError {
    /// BehaviorGraphData not found for index.
    #[snafu(display("BehaviorGraphData not found for index '{index}'"))]
    BehaviorGraphDataMissing { index: String },

    /// BehaviorGraphStringData not found for index.
    #[snafu(display("BehaviorGraphStringData not found for index '{index}'"))]
    BehaviorGraphStringDataMissing {
        index: std::borrow::Cow<'static, str>,
    },

    /// VariableValueSet not found for index.
    #[snafu(display("VariableValueSet not found for index '{}'", index))]
    VariableValueSetMissing {
        index: std::borrow::Cow<'static, str>,
    },

    /// Event names/infos length mismatch
    #[snafu(display(
        "Length mismatch in '{string_data_index}.hkbBehaviorGraphStringData.eventNames' (names len={names}) and '{behavior_graph_index}.hkbBehaviorGraphData.eventInfos' (infos len={infos}). \
         This usually means mod added an eventName but forgot to add a corresponding eventInfo.",
    ))]
    EventLengthMismatch {
        string_data_index: String,
        behavior_graph_index: String,
        names: usize,
        infos: usize,
    },

    /// Variable names/infos/word_values length mismatch
    #[snafu(display(
        "Length mismatch in '{string_data_index}.hkbBehaviorGraphStringData.variableNames' (names len={names}), '{behavior_graph_index}.hkbBehaviorGraphData.variableInfos' (infos len={infos}), \
         and '{binding_set_index}.hkbVariableValueSet.wordVariableValues' (word_values len={word_values}). \
         This usually means mod added a variableName but forgot to add corresponding variableInfo or wordValue.",
    ))]
    VariableLengthMismatch {
        string_data_index: String,
        behavior_graph_index: String,
        binding_set_index: String,
        names: usize,
        infos: usize,
        word_values: usize,
    },

    /// Multiple dedup pre_check errors
    #[snafu(display(
        "Multiple length mismatch errors.\nEvent error: {event_error}\nVariable error: {variable_error}",
    ))]
    #[allow(clippy::use_self)]
    BothLengthErrors {
        event_error: Box<DedupError>,
        variable_error: Box<DedupError>,
    },
}

fn dedup_names_and_infos_in_place<'a, T>(names: &mut Vec<StringPtr<'a>>, infos: &mut Vec<T>) {
    #[cfg(feature = "dedup_tracing")]
    let mut duplicates = Vec::new();

    let mut seen = HashSet::new();
    let mut keep = vec![false; names.len()];
    for (i, name) in names.iter().enumerate() {
        if let Some(s) = name.get_ref().as_ref().map(|s| s.as_ref()) {
            if seen.insert(AsciiIgnore::new(s)) {
                keep[i] = true;
            } else {
                #[cfg(feature = "dedup_tracing")]
                duplicates.push((i, s));
            }
        }
    }

    #[cfg(feature = "dedup_tracing")]
    if !duplicates.is_empty() {
        tracing::debug!(
            "[hkbBehaviorGraphStringData.eventNames] Duplicate events will be removed: {duplicates:#?}"
        );
    }

    let mut j = 0;
    names.retain(|_| {
        let k = keep[j];
        j += 1;
        k
    });

    // Sync infos
    let mut j = 0;
    infos.retain(|_| {
        let k = keep[j];
        j += 1;
        k
    });
}

fn dedup_three_way<T, U>(names: &mut Vec<StringPtr>, infos: &mut Vec<T>, word_values: &mut Vec<U>) {
    #[cfg(feature = "dedup_tracing")]
    let mut duplicates = Vec::new();

    let mut seen = HashSet::new();
    let mut keep = vec![false; names.len()];
    for (i, name) in names.iter().enumerate() {
        if let Some(s) = name.get_ref().as_ref().map(|s| s.as_ref()) {
            if seen.insert(AsciiIgnore::new(s)) {
                keep[i] = true;
            } else {
                #[cfg(feature = "dedup_tracing")]
                duplicates.push((i, s));
            }
        }
    }

    #[cfg(feature = "dedup_tracing")]
    if !duplicates.is_empty() {
        tracing::debug!(
            "[hkbBehaviorGraphStringData.variableNames] Duplicate variables will be removed: {duplicates:#?}"
        );
    }

    let mut j = 0;
    names.retain(|_| {
        let k = keep[j];
        j += 1;
        k
    });
    let mut j = 0;
    infos.retain(|_| {
        let k = keep[j];
        j += 1;
        k
    });
    let mut j = 0;
    word_values.retain(|_| {
        let k = keep[j];
        j += 1;
        k
    });
}

/// Create `EventIdMap` and `VariableIdMap` referencing internal strings in a `ClassMap`.
///
/// This function immutably borrows the `ClassMap` and returns maps where keys
/// are `&str` pointing directly to internal data.
///
/// # Important
///
/// Must be called **after** [`dedup_event_variables`] to ensure the internal
/// vectors are deduplicated. Separation of mutable and immutable borrows
/// is necessary due to Rust's borrow checker rules.
///
/// # Example
/// ```no_run
/// use serde_hkx_features::id_maker::create_maps;
/// use serde_hkx_features::{ClassMap, EventIdMap, VariableIdMap};
///
/// let class_map: ClassMap = ClassMap::new();
///
/// // Fill class_map and deduplicate first...
///
/// let (event_map, variable_map) = create_maps(&class_map, "#0000")
///     .expect("Should create maps");
///
/// // event_map and variable_map now reference internal strings
/// ```
pub fn create_maps<'a>(
    class_map: &'a ClassMap<'a>,
    behavior_graph_index: &str,
) -> Option<(EventIdMap<'a>, VariableIdMap<'a>)> {
    let string_data_index = match &class_map.get(behavior_graph_index)? {
        Classes::hkbBehaviorGraphData(g) => g.m_stringData.get(),
        _ => return None,
    };
    let string_data = match class_map.get(string_data_index)? {
        Classes::hkbBehaviorGraphStringData(g) => g,
        _ => return None,
    };

    let (event_map, variable_map) = rayon::join(
        || create_map_from_vec(&string_data.m_eventNames),
        || create_map_from_vec(&string_data.m_variableNames),
    );

    Some((EventIdMap(event_map), VariableIdMap(variable_map)))
}

fn create_map_from_vec<'a, 'b: 'a>(names: &'b [StringPtr<'a>]) -> HashMap<AsciiIgnore<'a>, usize> {
    names
        .par_iter()
        .enumerate()
        .filter_map(|(i, name)| Some((AsciiIgnore::new(name.get_ref().as_deref()?), i)))
        .collect()
}

#[cfg_attr(miri, ignore)] // It's impossible until the rayon fixes.
#[cfg(test)]
mod tests {
    use super::*;

    use havok_classes::{
        hkbBehaviorGraphData, hkbBehaviorGraphStringData, hkbEventInfo, hkbVariableInfo,
        hkbVariableValue, hkbVariableValueSet,
    };
    use havok_types::{I32, Pointer, StringPtr};
    use std::borrow::Cow;

    #[test]
    fn test_crate_maps_from_classmap() {
        let mut class_map: ClassMap = ClassMap::new();

        class_map.insert(
            Cow::Borrowed("#0000"),
            Classes::hkbBehaviorGraphStringData(Box::new(hkbBehaviorGraphStringData {
                m_eventNames: vec![
                    StringPtr::from_str("Run"),
                    StringPtr::from_str("Walk"),
                    StringPtr::from_str("Run"),
                    StringPtr::from_str("Jump"),
                ],
                m_variableNames: vec![
                    StringPtr::from_str("Health"),
                    StringPtr::from_str("Stamina"),
                    StringPtr::from_str("health"),
                    StringPtr::from_str("Mana"),
                ],
                ..Default::default()
            })),
        );
        class_map.insert(
            Cow::Borrowed("#0001"),
            Classes::hkbVariableValueSet(Box::new(hkbVariableValueSet {
                m_wordVariableValues: vec![
                    hkbVariableValue {
                        __ptr: None,
                        m_value: I32::Number(0),
                    },
                    hkbVariableValue {
                        __ptr: None,
                        m_value: I32::Number(1),
                    },
                    hkbVariableValue {
                        __ptr: None,
                        m_value: I32::Number(0),
                    },
                    hkbVariableValue {
                        __ptr: None,
                        m_value: I32::Number(0),
                    },
                ],
                ..Default::default()
            })),
        );

        class_map.insert(
            Cow::Borrowed("#0002"),
            Classes::hkbBehaviorGraphData(Box::new(hkbBehaviorGraphData {
                m_eventInfos: vec![
                    hkbEventInfo {
                        __ptr: None,
                        m_flags: havok_classes::hkbEventInfo_::Flags::FLAG_SILENT,
                    },
                    hkbEventInfo {
                        __ptr: None,
                        m_flags: havok_classes::hkbEventInfo_::Flags::FLAG_SILENT,
                    },
                    hkbEventInfo {
                        __ptr: None,
                        m_flags: havok_classes::hkbEventInfo_::Flags::FLAG_SYNC_POINT,
                    },
                    hkbEventInfo {
                        __ptr: None,
                        m_flags: havok_classes::hkbEventInfo_::Flags::FLAG_SILENT,
                    },
                ],
                m_variableInfos: vec![
                    hkbVariableInfo {
                        __ptr: None,
                        m_type: havok_classes::VariableType::VARIABLE_TYPE_BOOL,
                        ..Default::default()
                    },
                    hkbVariableInfo {
                        __ptr: None,
                        m_type: havok_classes::VariableType::VARIABLE_TYPE_INT16,
                        ..Default::default()
                    },
                    hkbVariableInfo {
                        __ptr: None,
                        m_type: havok_classes::VariableType::VARIABLE_TYPE_INT32,
                        ..Default::default()
                    },
                    hkbVariableInfo {
                        __ptr: None,
                        m_type: havok_classes::VariableType::VARIABLE_TYPE_REAL,
                        ..Default::default()
                    },
                ],
                m_stringData: Pointer::new(Cow::Borrowed("#0000")),
                m_variableInitialValues: Pointer::new(Cow::Borrowed("#0001")),
                ..Default::default()
            })),
        );

        dedup_event_variables(&mut class_map, "#0002".into()).expect("Should dedup class maps");
        let (event_map, variable_map) =
            create_maps(&class_map, "#0002").expect("Should create maps");

        // Check event map
        assert_eq!(event_map.0.len(), 3);
        assert_eq!(event_map.get("Run"), Some(0));
        assert_eq!(event_map.get("Walk"), Some(1));
        assert_eq!(event_map.get("Jump"), Some(2));

        // Check variable map
        assert_eq!(variable_map.0.len(), 3);
        assert_eq!(variable_map.get("Health"), Some(0));
        assert_eq!(variable_map.get("Stamina"), Some(1));
        assert_eq!(variable_map.get("Mana"), Some(2));

        // Optionally, inspect the deduplicated names/infos
        if let Classes::hkbBehaviorGraphStringData(s) = &class_map["#0000"] {
            assert_eq!(
                s.m_eventNames
                    .iter()
                    .filter_map(|e| e.get_ref().as_ref())
                    .collect::<Vec<_>>(),
                vec!["Run", "Walk", "Jump"]
            );
            assert_eq!(
                s.m_variableNames
                    .iter()
                    .filter_map(|v| v.get_ref().as_ref())
                    .collect::<Vec<_>>(),
                vec!["Health", "Stamina", "Mana"]
            );
        }
    }
}
