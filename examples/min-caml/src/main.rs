use anyhow::Context;
use std::{env, fs, io};
use tracing_subscriber::EnvFilter;

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_ansi(true)
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let input = match env::args().nth(1) {
        Some(path) => fs::read_to_string(&path).context("failed to load input file")?,
        None => io::read_to_string(io::stdin()).context("failed to load from stdin")?,
    };

    let expr = lelele_example_min_caml::parser::parse(&input).context("parse error")?;
    println!("parsed: {:?}", expr);

    Ok(())
}
