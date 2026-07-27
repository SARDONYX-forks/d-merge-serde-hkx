//! Trait to create a state transition tree
use crate::{ClassMapKey, GenericClassMap, HavokSort as _};
use havok_serde::HavokClass;
use havok_types::Pointer;
use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};

/// Trait to create ptr dependencies tree
pub trait HavokTree {
    type Error;

    /// Tree of the order in which to serialize as binary data.
    ///
    /// # Errors
    /// Missing hkRootLevelContainer or dependency cycle.
    fn tree_for_bytes(&mut self) -> Result<String, Self::Error>;

    // FIXME: This is not possible unless sort can be reproduced.
    // fn tree_for_xml(&mut self) -> Result<String, Self::Error>;

    /// Returns classes that are unreachable from hkRootLevelContainer.
    ///
    /// # Errors
    /// Missing hkRootLevelContainer.
    fn unreferenced_indexes(&self) -> Result<Vec<ClassMapKey<'_>>, Self::Error>;
}

#[derive(Debug)]
struct Node<'a> {
    name: String,
    deps: Vec<ClassMapKey<'a>>,
}

impl<'a> Node<'a> {
    #[inline]
    const fn new(name: String, deps: Vec<ClassMapKey<'a>>) -> Self {
        Self { name, deps }
    }
}

#[allow(clippy::too_many_arguments)]
fn print_node<'a>(
    nodes: &IndexMap<ClassMapKey<'a>, Node<'a>>,
    idx: ClassMapKey<'a>,
    depth: usize,
    visited: &mut HashMap<ClassMapKey<'a>, usize>,
    last_children: &mut Vec<bool>,
    result: &mut String,
    path: &mut Vec<ClassMapKey<'a>>,
    already_visited: &mut HashMap<ClassMapKey<'a>, bool>,
) {
    if path.contains(&idx) {
        let cycle_start = path.iter().position(|x| *x == idx).unwrap_or_default();

        let cycle_path: Vec<String> = path[cycle_start..]
            .iter()
            .map(|i| nodes[i].name.clone())
            .collect();

        result.push_str(&format!(
            "{}\\_Cycle Start(Invalid state transition): {}\n",
            "| ".repeat(depth + 2),
            nodes[&idx].name,
        ));

        for node_name in cycle_path.iter().skip(1) {
            result.push_str(&format!("{}|-- {}\n", "| ".repeat(depth + 2), node_name));
        }

        result.push_str(&format!(
            "{}    \\_Cycle End: {}\n",
            "| ".repeat(depth + 2),
            nodes[&idx].name,
        ));

        return;
    }

    let visit_count = visited.entry(idx.clone()).or_insert(0);
    *visit_count += 1;

    let is_already_visited = already_visited.entry(idx.clone()).or_insert(false);

    if !*is_already_visited {
        *is_already_visited = true;

        for level in 0..depth {
            if level == depth - 1 {
                if *last_children.last().unwrap_or(&false) {
                    result.push_str("`-- ");
                } else {
                    result.push_str("|-- ");
                }
            } else if *last_children.get(level).unwrap_or(&false) {
                result.push_str("    ");
            } else {
                result.push_str("|   ");
            }
        }

        if let Some(node) = nodes.get(&idx) {
            result.push_str(&node.name);
            result.push('\n');
        } else {
            #[cfg(feature = "tracing")]
            tracing::error!("Not found key: {idx}");
            return;
        }
    } else if *visit_count > 1 {
        for level in 0..depth {
            if level == depth - 1 {
                if *last_children.last().unwrap_or(&false) {
                    result.push_str("`-- ");
                } else {
                    result.push_str("|-- ");
                }
            } else if *last_children.get(level).unwrap_or(&false) {
                result.push_str("    ");
            } else {
                result.push_str("|   ");
            }
        }

        result.push_str(&format!(
            "{} (visited {visit_count} times)\n",
            nodes[&idx].name,
        ));
    }

    path.push(idx.clone());

    if let Some(node) = nodes.get(&idx) {
        for (i, dep) in node.deps.iter().enumerate() {
            last_children.push(i == node.deps.len() - 1);

            print_node(
                nodes,
                dep.clone(),
                depth + 1,
                visited,
                last_children,
                result,
                path,
                already_visited,
            );

            last_children.pop();
        }
    }

    path.pop();
}

impl<V> HavokTree for GenericClassMap<'_, V>
where
    V: HavokClass,
{
    type Error = crate::errors::ser::Error;

    fn tree_for_bytes(&mut self) -> Result<String, Self::Error> {
        self.checked_sort_for_bytes()?;

        let mut nodes: IndexMap<ClassMapKey, Node> = IndexMap::new();

        for (index, class) in self.iter() {
            let non_null_deps = class
                .deps_indexes()
                .into_iter()
                .filter(|ptr| !ptr.is_null())
                .map(|ptr| ptr.clone().into_inner())
                .collect();

            nodes.insert(
                index.clone(),
                Node::new(
                    format!("{}({})", class.name(), Pointer::new(index.clone())),
                    non_null_deps,
                ),
            );
        }

        let mut visited = HashMap::new();
        let mut result = String::new();
        let mut already_visited = HashMap::new();
        let mut path = Vec::new();

        for key in nodes.keys() {
            if !already_visited.contains_key(key) {
                print_node(
                    &nodes,
                    key.clone(),
                    0,
                    &mut visited,
                    &mut Vec::new(),
                    &mut result,
                    &mut path,
                    &mut already_visited,
                );
            }
        }

        Ok(result)
    }

    fn unreferenced_indexes(&self) -> Result<Vec<ClassMapKey<'_>>, Self::Error> {
        let (root_key, _) = crate::sort::find_root_ptr(self)?;

        let mut reachable = HashSet::new();

        fn collect_reachable<'a, V>(
            classes: &GenericClassMap<'a, V>,
            key: &ClassMapKey<'a>,
            reachable: &mut HashSet<String>,
        ) where
            V: HavokClass,
        {
            if !reachable.insert(key.to_string()) {
                return;
            }

            let Some(class) = classes.get(key) else {
                return;
            };

            for dep in class.deps_indexes() {
                if dep.is_null() {
                    continue;
                }

                let dep_key = dep.to_static().into_inner();
                collect_reachable(classes, &dep_key, reachable);
            }
        }

        collect_reachable(self, &root_key, &mut reachable);

        Ok(self
            .keys()
            .filter(|key| !reachable.contains(key.as_ref()))
            .cloned()
            .collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::mocks::new_defaultmale;

    #[cfg_attr(miri, ignore)] // Unexplained hang
    #[test]
    #[cfg_attr(
        feature = "tracing",
        quick_tracing::init(test = "generate_tree_from_xml", stdio = false)
    )]
    fn should_create_tree() {
        use crate::tests::ClassMap;

        let s = include_str!("../../docs/handson_hex_dump/wisp_skeleton/skeleton.xml");

        let mut classes: ClassMap = crate::from_str(s).unwrap();

        let tree = classes.tree_for_bytes().unwrap();

        tracing::debug!("tree =\n{tree}");
    }

    #[test]
    fn unreferenced_indexes_empty() {
        let classes = new_defaultmale();

        let unused = classes.unreferenced_indexes().unwrap();

        assert!(unused.is_empty());
    }

    #[test]
    fn tree_for_bytes_ok() {
        let mut classes = new_defaultmale();

        let tree = classes.tree_for_bytes().unwrap();

        assert!(!tree.is_empty());
    }

    #[test]
    fn tree_for_bytes_cycle_detected() {
        use havok_classes::Classes;

        let mut classes = new_defaultmale();

        let root = classes.keys().min().unwrap().clone();

        if let Some(Classes::hkRootLevelContainer(root_class)) = classes.get_mut(&root) {
            root_class.m_namedVariants[0].m_variant = Pointer::new(root.clone());
        }

        assert!(matches!(
            classes.tree_for_bytes(),
            Err(crate::errors::ser::Error::CycleDetected { .. })
        ));
    }
}
