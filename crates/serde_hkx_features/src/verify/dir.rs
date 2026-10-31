use super::verify_inner;
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
    let dir = dir.as_ref();
    verify_dir_(dir, progress)
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

fn verify_dir_<P>(dir: &Path, mut progress: P) -> Result<()>
where
    P: ProgressHandler + Sync + Send,
{
    let filtered_entries = filter_hkx_files(dir);
    let total_files = filtered_entries.len();

    if total_files == 0 {
        progress.on_empty();
        return Ok(());
    }

    progress.on_set_total(total_files);

    let errors: Vec<(PathBuf, Error)> = filtered_entries
        .into_par_iter()
        .filter_map(|path| {
            let result = match verify_inner(&path) {
                Ok((expected_bytes, actual_bytes)) if expected_bytes == actual_bytes => Ok(()),
                Ok(_) => Err(Error::ReproduceMismatch),
                Err(err) => Err(err),
            };

            progress.inc(1);
            progress.on_processing_path(path.as_path());

            match result {
                Ok(()) => {
                    progress.success_inc(1);
                    #[cfg(feature = "tracing")]
                    tracing::info!("Passed Reproduction: {}", path.display());
                    None
                }
                Err(err) => {
                    progress.failure_inc(1);
                    Some((path, err))
                }
            }
        })
        .collect();

    progress.on_finish();

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
