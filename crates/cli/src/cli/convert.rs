use crate::error::{Error, Result};
use havok_classes::Classes;
use serde_hkx::{
    bytes::serde::hkx_header::HkxHeader, from_bytes, from_str, to_bytes, to_string, HavokSort,
};
use std::{
    ffi::OsStr,
    io::{self, Read},
    path::Path,
};
use tokio::fs;

/// ANSI color representation command examples.
pub const EXAMPLES: &str = color_print::cstr!(
    r#"<blue><bold><underline>Examples</underline></bold></blue>
- <blue!>hkx -> xml</blue!>
  <cyan!>hkxc convert --input</cyan!> ./defaultmale.hkx <cyan!>--format</cyan!> xml

- <blue!>xml -> hkx(64bit)</blue!>
  <cyan!>hkxc convert -i</cyan!> ./defaultmale.xml <cyan!>-v</cyan!> amd64 <cyan!>--stdout --log-level</cyan!> trace

- <blue!>hkx(32bit) -> hkx(64bit)</blue!>
  <cyan!>hkxc convert -i</cyan!> ./defaultmale_x86.hkx <cyan!>-o</cyan!> ./defaultmale_x64.hkx <cyan!>-v</cyan!> amd64 <cyan!>--log-level</cyan!> debug <cyan!>--log-file</cyan!> "./convert_x86_to_x64_bytes.log"

- <blue!>hkx(64bit) -> hkx(32bit)</blue!>
  <cyan!>hkxc convert -i</cyan!> ./defaultmale_x64.hkx <cyan!>-o</cyan!> ./defaultmale_x86.hkx <cyan!>-v</cyan!>  win32 <cyan!>--log-level</cyan!> trace <cyan!>--log-file</cyan!> ./convert_x64_to_x86_bytes.log
"#
);

#[derive(Debug, clap::Args)]
#[clap(arg_required_else_help = true, after_long_help = EXAMPLES)]
pub(crate) struct Args {
    /// Path containing the hkx/xml file/directory
    #[clap(short, long)]
    pub input: String,
    /// Output path
    #[clap(short, long)]
    pub output: Option<String>,

    /// File format to output
    #[clap(short = 'v', long, ignore_case = true)]
    pub format: Format,
}

#[derive(Debug, clap::ValueEnum, Clone, Copy, parse_display::Display)]
pub enum Format {
    #[display("xml")]
    Xml,
    #[display("win32")]
    Win32,
    #[display("amd64")]
    Amd64,
}

impl<P> From<P> for Format
where
    P: AsRef<Path>,
{
    fn from(path: P) -> Self {
        if let Some(extension) = path.as_ref().extension() {
            let extension = extension.to_ascii_lowercase();
            match extension.to_string_lossy().as_ref() {
                "hkx" => Format::Xml,
                "xml" => Format::Amd64,
                _ => Format::Amd64,
            }
        } else {
            Format::Amd64
        }
    }
}

impl core::str::FromStr for Format {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(if s.eq_ignore_ascii_case("xml") {
            Self::Xml
        } else if s.eq_ignore_ascii_case("win32") {
            Self::Win32
        } else if s.eq_ignore_ascii_case("amd64") {
            Self::Amd64
        } else {
            return Err(Error::InvalidOutputFormat {
                unknown_fmt: s.to_string(),
            });
        })
    }
}

/// Convert dir or file(hkx, xml).
pub async fn convert<I, O>(input: I, output: Option<O>, format: Format) -> Result<()>
where
    I: AsRef<Path>,
    O: AsRef<Path>,
{
    let input = input.as_ref();
    if input.is_dir() {
        convert_dir(input, output, format).await?;
    } else if input.is_file() {
        convert_file(input, output, format).await?;
    } else {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("The path does not exist: {}", input.to_string_lossy()),
        ))?;
    }

    Ok(())
}

/// Convert dir.
pub async fn convert_dir<I, O>(input_dir: I, output_dir: Option<O>, format: Format) -> Result<()>
where
    I: AsRef<Path>,
    O: AsRef<Path>,
{
    let target_ext = format.to_string();

    let mut task_handles: Vec<tokio::task::JoinHandle<Result<()>>> = Vec::new();
    for path in jwalk::WalkDir::new(input_dir) {
        let path = path?.path();
        let path = path.as_path();
        if !path.is_file() && path.extension() != Some(std::ffi::OsStr::new(&target_ext)) {
            continue;
        }

        let input = path.to_path_buf();
        // If output_dir is specified, make it the root dir to maintain the hierarchy and output.
        let output = match output_dir.as_ref() {
            Some(output_root) => {
                let root_name = input.iter().next().unwrap_or_default();
                let relative_path = input.strip_prefix(root_name)?;
                let output = output_root.as_ref().join(relative_path);
                fs::create_dir_all(&output).await?;
                Some(output)
            }
            None => None,
        };

        task_handles.push(tokio::spawn(async move {
            convert_file(&input, output, format).await.map_err(|err| {
                tracing::error!("Error occurred path: {input:?}");
                err
            })
        }));
    }

    for task_handle in task_handles {
        task_handle.await??;
    }
    Ok(())
}

/// Convert `hkx`/`xml` file to `hkx`/`xml` file.
///
/// # Note
/// If `output` is not specified, the output is placed at the same level as `input`.
pub async fn convert_file<I, O>(input: I, output: Option<O>, format: Format) -> Result<()>
where
    I: AsRef<Path>,
    O: AsRef<Path>,
{
    let input = input.as_ref();
    let extension = input.extension();
    let bytes = fs::read(input).await?;
    let mut xml = String::new(); // To avoid ownership errors, declare it here, but since it is a 0-allocation, there is no problem.

    /// (ptr index, class)
    type ClassMap<'a> = indexmap::IndexMap<usize, Classes<'a>>;

    let mut classes: ClassMap = if extension == Some(OsStr::new("hkx")) {
        from_bytes(&bytes)?
    } else if extension == Some(OsStr::new("xml")) {
        let mut decoder = encoding_rs_io::DecodeReaderBytes::new(bytes.as_slice());
        decoder.read_to_string(&mut xml)?;
        from_str(&xml)?
    } else {
        return Err(Error::UnknownExtension {
            path: input.to_string_lossy().to_string(),
        });
    };

    match format {
        Format::Xml => {
            let top_ptr = classes.sort_for_xml()?;
            let xml = to_string(&classes, top_ptr)?;

            let output = output
                .map(|output| output.as_ref().to_path_buf())
                .unwrap_or({
                    let mut output = input.to_path_buf();
                    output.set_extension("xml");
                    output
                });

            if let Some(parent) = output.parent() {
                tokio::fs::create_dir_all(parent).await?;
            }
            fs::write(output, xml).await?;
        }

        Format::Win32 | Format::Amd64 => {
            classes.sort_for_bytes();
            let binary_data = match format {
                Format::Win32 => to_bytes(&classes, &HkxHeader::new_skyrim_le()),
                Format::Amd64 => to_bytes(&classes, &HkxHeader::new_skyrim_se()),
                Format::Xml => unreachable!(),
            }?;

            let output = output
                .map(|output| output.as_ref().to_path_buf())
                .unwrap_or({
                    let mut output = input.to_path_buf();
                    output.set_extension("hkx");
                    output
                });
            fs::write(&output, binary_data).await?;
            tracing::info!(
                "Converted {} -> {}",
                input.to_string_lossy(),
                output.to_string_lossy()
            );
        }
    }

    Ok(())
}
