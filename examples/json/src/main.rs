use anyhow::Context as _;
use std::{env, fs, io};
use tracing_subscriber::EnvFilter;

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_ansi(true)
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let input = match env::args().nth(1) {
        Some(path) => fs::read_to_string(&path)
            .with_context(|| format!("failed to read from file: {}", path))?,
        None => io::read_to_string(io::stdin()).context("failed to read from stdin")?,
    };

    let parsed = lelele_example_json::parser::parse(&input).context("failed to parse")?;
    println!("parsed: {}", parsed);

    Ok(())
}
