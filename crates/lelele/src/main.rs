use anyhow::Context as _;
use clap::{Parser, ValueEnum};
use lelele::codegen::Codegen;
use std::{fs, path::PathBuf, time::Instant};
use tracing_subscriber::EnvFilter;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The algorithm to generate LR(1) automaton.
    #[arg(long, value_enum, default_value_t = LRAlgorithm::IELR)]
    algorithm: LRAlgorithm,

    /// Specify the path of generated .rs file.
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// The path of grammar definition file.
    input: PathBuf,
}

#[derive(Debug, Copy, Clone, PartialEq, ValueEnum)]
enum LRAlgorithm {
    //Canonical,
    IELR,
    LALR,
}

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let args = Args::parse();
    tracing::debug!("parsed CLI args = {:?}", args);

    process_file(&args)
        .with_context(|| anyhow::anyhow!("errored during processing {}", args.input.display()))?;

    Ok(())
}

fn process_file(args: &Args) -> anyhow::Result<()> {
    let in_file = fs::canonicalize(&args.input) //
        .context("failed to canonicalize the input file name")?;

    let out_file = args
        .output
        .clone()
        .unwrap_or_else(|| in_file.with_extension("rs"));
    let backup_file = out_file.with_extension("rs.bak");

    let s = Instant::now();
    let grammar = lelele::syntax::parse_file(&in_file)?;
    tracing::info!("parse_file: {:?} elapsed", s.elapsed());

    let mut empty_nonterminals = vec![];
    for (&id, name) in &grammar.nonterminals {
        if grammar.cfg.rules.values().all(|rule| rule.left != id) {
            empty_nonterminals.push(&*name);
        }
    }
    if !empty_nonterminals.is_empty() {
        println!(
            "[warning] The following nonterminals have no associated production rule: {:?}",
            empty_nonterminals
        );
    }

    let mode = match args.algorithm {
        LRAlgorithm::LALR => lelele::ielr::Mode::LALR,
        LRAlgorithm::IELR => lelele::ielr::Mode::IELR,
    };

    let s = Instant::now();
    let table = lelele::ielr::compute(&grammar.cfg, mode)?;
    tracing::info!("compute_table: {:?} elapsed", s.elapsed());

    // dump results.
    if out_file.exists() {
        fs::copy(&out_file, &backup_file).with_context(|| {
            anyhow::anyhow!(
                "failed to backup the output file to {}",
                backup_file.display()
            )
        })?;
    }

    let codegen = Codegen::new(&grammar, &table);
    fs::write(&out_file, codegen.to_string()).with_context(|| {
        anyhow::anyhow!("failed to write generated parser to {}", out_file.display())
    })?;

    Ok(())
}
