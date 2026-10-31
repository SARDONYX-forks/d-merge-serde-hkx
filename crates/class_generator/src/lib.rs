#![allow(clippy::unwrap_used, clippy::panic)] // TODO: Replace with Result. Temporary Linter Override Action.

pub mod cpp_info;
pub mod error;
mod gen_index;
mod get_class_map;
mod rust_gen;

use crate::{cpp_info::Class, error::*};
use indexmap::IndexMap;
use rayon::{iter::Either, prelude::*};
use snafu::prelude::*;
use std::{fs, path::Path};

pub use get_class_map::{get_inherited_class, get_inherited_members};

pub type ClassMap<'a> = IndexMap<String, Class<'a>>;

/// `assets/classes` of this repository: the class json files.
///
/// For generators in other repositories (e.g. a richer class table): a git dependency checks out
/// the whole repository, so this path exists next to the crate.
pub fn classes_json_dir() -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../assets/classes")
}

/// Reads every class json file of `classes_json_dir` (see [`classes_json_dir`]).
///
/// # Errors
/// A file cannot be read or parsed (all failures, as text).
pub fn load_class_map<P: AsRef<Path>>(classes_json_dir: P) -> Result<ClassMap<'static>> {
    let (errors, class_map) = create_class_map(classes_json_dir);
    if errors.is_empty() {
        return Ok(class_map);
    }
    let errors: Vec<_> = errors.into_iter().map(|err| err.to_string()).collect();
    FailedGenerateSnafu { errors }.fail()
}

/// Generate havok class code(For Rust) from class info json files.
///
/// # Errors
/// When parse error.
pub fn generate_havok_class<P: AsRef<Path>>(classes_json_dir: P, out_dir: P) -> Result<()> {
    let class_out_dir = out_dir.as_ref().join("generated");
    std::fs::create_dir_all(&class_out_dir)?;

    let (mut errors, class_map) = create_class_map(classes_json_dir);
    let (write_errors, class_index) = generate_and_write_code(&class_map, &class_out_dir);
    std::fs::write(
        out_dir.as_ref().join("generated.rs"),
        gen_index::gen_index(&class_index)?,
    )?;

    errors.par_extend(write_errors);
    if errors.is_empty() {
        return Ok(());
    }

    let errors: Vec<_> = errors.into_par_iter().map(|err| err.to_string()).collect();
    tracing::error!("errors: {errors:#?}");
    FailedGenerateSnafu { errors }.fail()
}

fn create_class_map(
    classes_json_dir: impl AsRef<Path>,
) -> (Vec<ClassGeneratorError>, ClassMap<'static>) {
    let results: Vec<_> = jwalk::WalkDir::new(&classes_json_dir).into_iter().collect();

    let results: Vec<Result<(String, Class)>> = results
        .into_par_iter()
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.path().is_file())
        .map(|entry| {
            let path = entry.path();
            let class_name = path.file_stem().context(NotFoundFileStemSnafu {
                path: path.to_string_lossy().into_owned(),
            })?;
            let class_name = class_name.to_string_lossy().into_owned();

            let json = fs::read_to_string(&path).with_context(|_| IoSnafu { path })?;
            let class: Class = serde_json::from_str(&json)?;
            Ok((class_name, class))
        })
        .collect();

    results
        .into_par_iter()
        .partition_map(|result| match result {
            Ok((class_name, class)) => Either::Right((class_name, class)),
            Err(e) => Either::Left(e),
        })
}

fn generate_and_write_code<'a>(
    class_map: &'a ClassMap<'a>,
    class_out_dir: &Path,
) -> (Vec<ClassGeneratorError>, Vec<(&'a String, bool)>) {
    let output_results: Vec<Result<(&String, bool)>> = class_map
        .par_iter()
        .map(|(class_name, class)| {
            let file_name = format!("{class_name}_");

            // if matches!(
            //     class_name.as_str(),
            //     "hkaBone" | "hkColor" | "hkbRoleAttribute" | "hkAabbUint32"
            // ) {
            let output_file = class_out_dir.join(format!("{file_name}.rs"));
            let output_file = output_file.to_string_lossy();
            tracing::debug!(?output_file);
            let rust_code = prettyplease::unparse(&rust_gen::from_cpp_class(class, class_map)?);
            std::fs::write(output_file.as_ref(), rust_code)?;
            // }

            Ok((class_name, class.has_ref))
        })
        .collect();

    output_results
        .into_par_iter()
        .partition_map(|result| match result {
            Ok(tuple) => Either::Right(tuple),
            Err(e) => Either::Left(e),
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[ignore = "because it generates code and is not strictly a test."]
    // #[quick_tracing::init(test = "should_generate_classes")]
    #[test]
    fn should_generate_classes() {
        let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("..");

        let classes_json_dir = repo_root.join("assets").join("classes");
        let output_dir = repo_root.join("crates/havok_classes/src");
        std::fs::create_dir_all(&output_dir).unwrap();

        generate_havok_class(classes_json_dir, output_dir).unwrap_or_else(|err| panic!("{err}"));
    }
}
