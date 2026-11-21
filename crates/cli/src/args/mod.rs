pub(crate) mod color;
mod convert;
mod diff;
mod dump;
mod progress_handler;
mod tree;
mod verify;

#[cfg(feature = "color")]
use self::color::get_styles;
use self::convert::{convert, tokio_convert};
use crate::logger::LogLevel;
use clap::CommandFactory as _;
use serde_hkx_features::{
    convert::Format,
    diff::write_diff,
    dump as hexdump,
    error::{Error, Result},
    tree::write_tree,
};
use std::{io, path::PathBuf};

pub(crate) async fn run(args: Args) -> Result<()> {
    crate::logger::init(args.log_file, args.log_level, args.stdout)?;

    // For drag & drop
    if let Some(input) = args.input {
        let res = async {
            let out_fmt = Format::infer_output_from_input(&input)?;
            let output: Option<PathBuf> = None;
            tokio_convert(&input, output, out_fmt).await
        };
        return res.await.map_err(|e| e.with_path(&input));
    }

    if let Some(command) = args.subcommand {
        // NOTE: Errors do not carry the input path, so attach it here.
        match command {
            SubCommands::Convert(args) => {
                convert(&args.input, args.output, args.format, args.runtime)
                    .await
                    .map_err(|e| e.with_path(&args.input))
            }
            SubCommands::Tree(args) => write_tree(&args.input, args.output)
                .await
                .map_err(|e| e.with_path(&args.input)),
            SubCommands::Dump(args) => match args.reverse {
                true => hexdump::to_bytes(&args.input, args.output).await,
                false => hexdump::to_string(&args.input, args.output).await,
            }
            .map_err(|e| e.with_path(&args.input)),
            SubCommands::Diff(inner_args) => {
                // NOTE: `write_diff` attaches each input path by itself.
                // A file output never has color.
                let use_color =
                    inner_args.output.is_none() && color::is_color_enabled(&io::stdout());
                write_diff(inner_args.old, inner_args.new, inner_args.output, use_color).await
            }
            SubCommands::Verify(inner_args) => self::verify::verify(
                &inner_args.path,
                color::is_color_enabled(&io::stderr()), // diff is in the error(stderr)
                inner_args.mode,
                inner_args.report.as_deref(),
            )
            .map_err(|e| e.with_path(&inner_args.path)),

            SubCommands::Completions { shell } => {
                shell.generate(&mut Args::command(), &mut io::stdout());
                Ok(())
            }
        }
    } else {
        Args::command().print_long_help()?;
        let err = color::ERROR;
        let msg = format!(
            "
{err}error:{err:#} '{err}hkxc{err:#}' requires a subcommand or hkx/xml path.
For more information, try '--help'.
"
        );
        Err(Error::Io {
            source: io::Error::new(io::ErrorKind::InvalidInput, msg),
        })
    }
}

/// CLI command arguments
#[derive(Debug, clap::Parser)]
#[clap(version, about, author)]
#[cfg_attr(feature = "color", clap(styles=get_styles()))]
#[clap(arg_required_else_help = true, args_conflicts_with_subcommands = true, after_long_help = convert::examples())]
pub(crate) struct Args {
    /// The path of the target for drag and drop hkx(64bit) <-> xml conversion (The converted file will be created in the same location)
    pub input: Option<PathBuf>,

    #[clap(subcommand)]
    subcommand: Option<SubCommands>,

    // --logger (Global options)
    #[clap(global = true, long, display_order = 100)]
    /// Enable standard output of the log
    pub stdout: bool,
    #[clap(global = true, long, display_order = 101)]
    #[clap(ignore_case = true, default_value = "error")]
    /// Log level to be recorded in logger
    pub log_level: LogLevel,
    #[clap(global = true, long, display_order = 102)]
    /// Output path of log file
    pub log_file: Option<PathBuf>,

    /// Disable colored output.(Color is enabled by default only when the output is a terminal.)
    ///
    /// NOTE: This is read before parsing(see `main`) to apply it to help/errors of clap too.
    #[clap(global = true, long, display_order = 103)]
    pub no_color: bool,
}

impl Args {
    /// Parse args with `--no-color`/`NO_COLOR` applied to clap(help, errors) and `anstream`(others).
    pub fn parse_with_color() -> Self {
        use clap::FromArgMatches as _;

        let no_color = std::env::args_os().any(|arg| arg == "--no-color");
        if no_color {
            anstream::ColorChoice::Never.write_global();
        }
        let color = if no_color {
            clap::ColorChoice::Never
        } else {
            clap::ColorChoice::Auto // Auto: terminal only, respects `NO_COLOR` env
        };

        let mut matches = Self::command().color(color).get_matches();
        Self::from_arg_matches_mut(&mut matches).unwrap_or_else(|err| err.exit())
    }
}

#[derive(Debug, clap::Parser)]
pub(crate) enum SubCommands {
    /// Convert hkx <-> xml
    Convert(convert::Args),

    /// Show dependency tree from havok behavior(hkx/xml file)
    Tree(tree::Args),

    /// Dump binary data in hexadecimal
    Dump(dump::Args),

    /// Show diff between two files.(In the case of `.hkx`, it is automatically converted to hexdump)
    Diff(diff::Args),

    /// Parallel hkx reproduction checks. If an error occurs, return a diff showing the location of each error.
    Verify(verify::Args),

    /// Generate shell completions
    #[clap(arg_required_else_help = true)]
    Completions {
        /// The shell to generate the completions for
        #[arg(value_enum)]
        shell: clap_complete_command::Shell,
    },
}
