use anyhow::Context;
use std::env;
use tracing_subscriber::EnvFilter;

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_ansi(true)
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let input = env::args().nth(1).context("missing input")?;

    let parsed = lelele_example_arithmetic::parser::parse(&input).context("parser error")?;
    println!("parsed: {}", parsed);

    Ok(())
}
