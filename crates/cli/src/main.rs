pub mod args;
mod logger;

use crate::args::Args;
use std::process::ExitCode;
use tokio::time::Instant;

// NOTE: Return `ExitCode` instead of calling `std::process::exit`.
//       `exit` does not run destructors(final draw of progress bars, shutdown of tokio runtime, buffered writers, etc.).
#[tokio::main]
async fn main() -> ExitCode {
    let start = Instant::now();

    match crate::args::run(Args::parse_with_color()).await {
        Ok(()) => {
            let elapsed = start.elapsed();
            let time = (elapsed.as_secs(), elapsed.subsec_millis());
            tracing::info!("Elapsed time: {}.{}secs.", time.0, time.1);
            ExitCode::SUCCESS
        }
        Err(err) => {
            tracing::error!("{err}");
            // NOTE: `anstream` strips colors if stderr is not a terminal, `NO_COLOR` env or `--no-color`.
            let red = crate::args::color::ERROR;
            anstream::eprintln!("{red}[Error]\n{err}{red:#}");
            ExitCode::FAILURE
        }
    }
}
