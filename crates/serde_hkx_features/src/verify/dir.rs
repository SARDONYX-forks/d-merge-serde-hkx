use super::{VerifyMode, verify_inner, xml::verify_xml_file};
use crate::error::{Error, FailedReproduceFilesSnafu, Result};
use crate::progress::ProgressHandler;
use rayon::prelude::*;
use std::path::{Path, PathBuf};

/// Parallel checks reproduction for hkx files.
///
/// # Errors
/// If an error occurs, returns an array of error paths.
pub fn verify_dir<D, P>(dir: D, progress: P) -> Result<()>
where
    D: AsRef<Path>,
    P: ProgressHandler + Send + Sync,
{
    verify_dir_with(dir, VerifyMode::Bytes, progress, None)
}

/// Parallel checks reproduction for hkx files with the mode.
///
/// - `report`: Write a result of each file(sorted by relative path) to compare with the previous run.
///   ```txt
///   OK    meshes/actors/character/behaviors/0_master.hkx
///   NG    meshes/actors/character/behaviors/1hm_behavior.hkx    xml_semantic
///   ```
///
/// # Errors
/// If an error occurs, returns an array of error paths.
pub fn verify_dir_with<D, P>(
    dir: D,
    mode: VerifyMode,
    mut progress: P,
    report: Option<&Path>,
) -> Result<()>
where
    D: AsRef<Path>,
    P: ProgressHandler + Send + Sync,
{
    let dir = dir.as_ref();
    let filtered_entries = filter_hkx_files(dir);
    let total_files = filtered_entries.len();

    if total_files == 0 {
        progress.on_empty();
        return Ok(());
    }

    progress.on_set_total(total_files);

    let results: Vec<(PathBuf, Result<()>)> = filtered_entries
        .into_par_iter()
        .map(|path| {
            let result = crate::error::catch_panic(|| verify_path(&path, mode));

            progress.inc(1);
            progress.on_processing_path(path.as_path());
            match &result {
                Ok(()) => {
                    progress.success_inc(1);
                    #[cfg(feature = "tracing")]
                    tracing::info!("Passed Reproduction: {}", path.display());
                }
                Err(_) => progress.failure_inc(1),
            }
            (path, result)
        })
        .collect();

    progress.on_finish();

    if let Some(report) = report {
        write_report(dir, &results, report)?;
    }

    let errors: Vec<(PathBuf, Error)> = results
        .into_iter()
        .filter_map(|(path, result)| result.err().map(|err| (path, err)))
        .collect();
    if errors.is_empty() {
        Ok(())
    } else {
        FailedReproduceFilesSnafu {
            total_files,
            errors,
        }
        .fail()
    }
}

fn verify_path(path: &Path, mode: VerifyMode) -> Result<()> {
    match mode {
        VerifyMode::Bytes => match verify_inner(path) {
            Ok((expected_bytes, actual_bytes)) if expected_bytes == actual_bytes => Ok(()),
            Ok(_) => Err(Error::ReproduceMismatch),
            Err(err) => Err(err),
        },
        VerifyMode::Xml => verify_xml_file(path, false),
    }
}

fn filter_hkx_files(dir: &Path) -> Vec<PathBuf> {
    jwalk::WalkDir::new(dir)
        .into_iter()
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry
                .path()
                .extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("hkx"))
        })
        .map(|entry| entry.path())
        .collect()
}

/// Short error kind for the report.(Must be stable among runs to compare.)
const fn error_kind(err: &Error) -> &'static str {
    match err {
        Error::ReproduceMismatch => "bytes_mismatch",
        Error::XmlRoundTripMismatch { check, .. } => check,
        Error::HkxDe { .. } => "hkx_de",
        Error::HkxSer { .. } => "hkx_ser",
        Error::XmlDe { .. } => "xml_de",
        Error::XmlSer { .. } => "xml_ser",
        Error::Io { .. } => "io",
        Error::Panicked { .. } => "panic",
        _ => "other",
    }
}

fn write_report(dir: &Path, results: &[(PathBuf, Result<()>)], report: &Path) -> Result<()> {
    use core::fmt::Write as _;

    let mut lines: Vec<String> = results
        .iter()
        .map(|(path, result)| {
            let rel = path.strip_prefix(dir).unwrap_or(path);
            let rel = rel.to_string_lossy().replace('\\', "/");
            match result {
                Ok(()) => format!("OK\t{rel}"),
                Err(err) => format!("NG\t{rel}\t{}", error_kind(err)),
            }
        })
        .collect();
    lines.sort_unstable_by(|a, b| a[3..].cmp(&b[3..])); // sort by path

    let mut out = String::new();
    for line in lines {
        let _ = writeln!(out, "{line}");
    }
    if let Some(parent) = report.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(report, out)?;
    Ok(())
}
