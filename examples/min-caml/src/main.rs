use anyhow::Context;
use std::{env, fs, io};

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_ansi(true)
        .with_max_level(tracing::Level::TRACE)
        .init();
    let input = match env::args().nth(1) {
        Some(path) => fs::read_to_string(&path).context("failed to load input file")?,
        None => io::read_to_string(io::stdin()).context("failed to load from stdin")?,
    };
    let expr = lelele_example_min_caml::parser::parse(&input).context("parse error")?;
    println!("parsed: {:?}", expr);
    Ok(())
}
