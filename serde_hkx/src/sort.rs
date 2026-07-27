//! Provides a method that can be used to sort bytes and XML to serialize.
use crate::{ClassMapKey, GenericClassMap, errors::ser::Error as SerError};
use havok_serde::HavokClass;
use havok_types::Pointer;
use indexmap::IndexMap;
use std::{borrow::Cow, collections::HashMap};

/// Trait that provides a method that can be used to sort bytes and XML to serialize.
pub trait HavokSort {
    type Error;

    /// Sort by dependent class from root for serialization of bytes.
    ///
    /// There is a rule that binary data serializes class dependency pointers in order from the root,
    /// which is `hkRootLevelContainer`.
    ///
    /// # Current implementation
    /// - If deserialize a binary with `serde-hkx` -> already sorted for bytes
    /// - If deserialize xml of official SDK -> sort is required
    fn sort_for_bytes(&mut self);

    /// Sort by dependent class for XML serialization.
    ///
    /// # Return
    /// top ptr
    ///
    /// # Errors
    /// Missing top pointer.
    fn sort_for_xml(&mut self) -> Result<Pointer<'static>, Self::Error>;

    /// Sort for bytes after validating the dependency graph.
    ///
    /// # Errors
    /// - Dependency cycle detected.
    fn checked_sort_for_bytes(&mut self) -> Result<(), Self::Error>;

    /// Sort for xml after validating the dependency graph.
    ///
    /// # Errors
    /// - Dependency cycle detected.
    /// - Missing top pointer.
    fn checked_sort_for_xml(&mut self) -> Result<Pointer<'static>, Self::Error>;
}

impl<V> HavokSort for GenericClassMap<'_, V>
where
    V: HavokClass,
{
    type Error = SerError;

    fn sort_for_bytes(&mut self) {
        if self.is_empty() {
            return;
        }

        let Some(root_key) = self.keys().min().cloned() else {
            return;
        };

        sort_for_bytes_with_root(self, &root_key);
    }

    fn checked_sort_for_bytes(&mut self) -> Result<(), Self::Error> {
        if self.is_empty() {
            return Ok(());
        }

        let root_key = self
            .keys()
            .min()
            .cloned()
            .ok_or_else(|| SerError::Message {
                msg: "Missing top pointer.".to_owned(),
            })?;

        let mut states = HashMap::new();
        let mut path = Vec::new();

        check_cycle(self, &root_key, &mut states, &mut path)?;

        sort_for_bytes_with_root(self, &root_key);

        Ok(())
    }

    fn sort_for_xml(&mut self) -> Result<Pointer<'static>, Self::Error> {
        let root_key = self
            .keys()
            .min()
            .cloned()
            .ok_or_else(|| SerError::Message {
                msg: "Missing top pointer.".to_owned(),
            })?;

        sort_for_xml_with_root(self, &root_key);

        Ok(Pointer::new(root_key).to_static())
    }

    fn checked_sort_for_xml(&mut self) -> Result<Pointer<'static>, Self::Error> {
        let root_key = self
            .keys()
            .min()
            .cloned()
            .ok_or_else(|| SerError::Message {
                msg: "Missing top pointer.".to_owned(),
            })?;

        let mut states = HashMap::new();
        let mut path = Vec::new();

        check_cycle(self, &root_key, &mut states, &mut path)?;

        sort_for_xml_with_root(self, &root_key);

        Ok(Pointer::new(root_key).to_static())
    }
}

fn sort_for_bytes_with_root<'a, V>(classes: &mut GenericClassMap<'a, V>, root_key: &ClassMapKey<'a>)
where
    V: HavokClass,
{
    #[expect(clippy::ptr_arg)]
    fn collect_deps<'bytes, V>(
        classes: &GenericClassMap<'bytes, V>,
        key: &Cow<'bytes, str>,
        sorted_keys: &mut Vec<ClassMapKey<'bytes>>,
    ) where
        V: HavokClass,
    {
        if sorted_keys.contains(key) {
            return;
        }

        sorted_keys.push(key.clone());

        let deps = {
            let Some(class) = classes.get(key) else {
                return;
            };

            #[cfg(feature = "tracing")]
            tracing::trace!("index = {key}, deps_indexes = {:?}", class.deps_indexes());

            class.deps_indexes()
        };

        for dep in deps {
            if dep.is_null() {
                continue;
            }

            let dep_key = Cow::Owned(dep.to_string());
            collect_deps(classes, &dep_key, sorted_keys);
        }
    }

    let mut sorted_keys = Vec::with_capacity(classes.len());
    collect_deps(classes, root_key, &mut sorted_keys);

    #[cfg(feature = "tracing")]
    tracing::trace!("sorted_keys = {sorted_keys:?}");

    let mut sorted_classes = GenericClassMap::with_capacity(classes.len());

    for key in sorted_keys {
        if let Some(class) = classes.swap_remove(&key) {
            sorted_classes.insert(key, class);
        }
    }

    *classes = sorted_classes;
}

fn sort_for_xml_with_root<'key, V>(
    classes: &mut GenericClassMap<'key, V>,
    root_key: &ClassMapKey<'key>,
) where
    V: HavokClass,
{
    fn collect_deps<'map, 'key: 'map, V>(
        classes: &'map IndexMap<ClassMapKey<'key>, V>,
        key: &ClassMapKey<'key>,
        sorted: &mut Vec<ClassMapKey<'key>>,
    ) where
        V: HavokClass,
    {
        if sorted.contains(key) {
            return;
        }

        let Some(class) = classes.get(key) else {
            return;
        };

        let deps = class.deps_indexes();

        #[cfg(feature = "tracing")]
        tracing::trace!("index = {key}, deps_indexes = {deps:?}");

        for dep in deps {
            if dep.is_null() {
                continue;
            }

            let dep_key = Cow::Owned(dep.to_string());
            collect_deps(classes, &dep_key, sorted);
        }

        sorted.push(key.clone());
    }

    let mut sorted_keys = Vec::with_capacity(classes.len());
    collect_deps(classes, root_key, &mut sorted_keys);

    #[cfg(feature = "tracing")]
    tracing::trace!("sorted_keys = {sorted_keys:?}");

    let mut sorted_classes = GenericClassMap::with_capacity(classes.len());

    for key in sorted_keys {
        if let Some(class) = classes.swap_remove(&key) {
            sorted_classes.insert(key, class);
        }
    }

    *classes = sorted_classes;
}

/// Find `hkRootLevelContainer`.
pub(crate) fn find_root_ptr<'a, V>(
    class_map: &'a GenericClassMap<'a, V>,
) -> Result<(ClassMapKey<'a>, &'a V), SerError>
where
    V: HavokClass,
{
    let Some((key, value)) =
        class_map.get_key_value(class_map.keys().min().ok_or_else(|| SerError::Message {
            msg: "Missing top pointer.".to_owned(),
        })?)
    else {
        return Err(SerError::Message {
            msg: "Missing top pointer.".to_owned(),
        });
    };

    Ok((key.clone(), value))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VisitState {
    Visiting,
    Visited,
}

fn check_cycle<'a, V>(
    classes: &GenericClassMap<'a, V>,
    key: &ClassMapKey<'a>,
    states: &mut HashMap<ClassMapKey<'a>, VisitState>,
    path: &mut Vec<ClassMapKey<'a>>,
) -> Result<(), SerError>
where
    V: HavokClass,
{
    match states.get(key) {
        Some(VisitState::Visited) => return Ok(()),
        Some(VisitState::Visiting) => {
            let start = path.iter().position(|k| k == key).unwrap_or_default();

            let mut cycle = path[start..].to_vec();
            cycle.push(key.clone());

            let cycle = cycle
                .into_iter()
                .map(|s| Pointer::new(s).to_static())
                .collect();

            return Err(SerError::CycleDetected { cycle });
        }
        None => {}
    }

    states.insert(key.clone(), VisitState::Visiting);
    path.push(key.clone());

    let Some(class) = classes.get(key) else {
        path.pop();
        states.insert(key.clone(), VisitState::Visited);
        return Ok(());
    };

    for dep in class.deps_indexes() {
        if dep.is_null() {
            continue;
        }

        let dep_key = Cow::Owned(dep.to_string());

        check_cycle(classes, &dep_key, states, path)?;
    }

    path.pop();
    states.insert(key.clone(), VisitState::Visited);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::mocks::new_defaultmale;

    #[test]
    fn test_sort() {
        let mut classes = new_defaultmale();
        classes.sort_for_bytes();

        assert_eq!(classes, new_defaultmale());
    }

    #[test]
    fn checked_sort_for_bytes_ok() {
        let mut classes = new_defaultmale();

        assert!(classes.checked_sort_for_bytes().is_ok());
    }

    #[test]
    fn checked_sort_for_xml_ok() {
        let mut classes = new_defaultmale();

        assert!(classes.checked_sort_for_xml().is_ok());
    }

    #[test]
    fn checked_sort_for_bytes_cycle_detected() {
        use havok_classes::Classes;

        let mut classes = new_defaultmale();

        let root = classes.keys().min().unwrap().clone();

        if let Some(Classes::hkRootLevelContainer(root_class)) = classes.get_mut(&root) {
            root_class.m_namedVariants[0].m_variant = Pointer::new(root.clone());
        }

        assert!(matches!(
            classes.checked_sort_for_bytes(),
            Err(SerError::CycleDetected { .. })
        ));
    }
}
