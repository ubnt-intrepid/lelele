use anyhow::Context as _;
use clap::{Parser, ValueEnum};
use lelele::codegen::Codegen;
use std::{
    fs,
    path::{Path, PathBuf},
};
use tracing_subscriber::EnvFilter;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(long, value_enum, default_value_t = MergeMode::PGM)]
    merge_mode: MergeMode,

    /// The path of grammar definition file.
    input: Vec<PathBuf>,
}

#[derive(Debug, Copy, Clone, PartialEq, ValueEnum)]
enum MergeMode {
    Canonical,
    PGM,
    LALR,
}

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let args = Args::parse();
    tracing::debug!("parsed CLI args = {:?}", args);

    for in_file in &args.input {
        tracing::info!("process {}", in_file.display());
        process_file(&args, in_file)
            .with_context(|| anyhow::anyhow!("errored during processing {}", in_file.display()))?;
    }

    Ok(())
}

fn process_file(_args: &Args, in_file: &Path) -> anyhow::Result<()> {
    let in_file = fs::canonicalize(in_file) //
        .context("failed to canonicalize the input file name")?;

    let out_file = in_file.with_extension("rs");
    let backup_file = in_file.with_extension("rs.bak");

    let grammar = lelele::syntax::parse_file(&in_file)?;

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

    let table = lelele::ielr::compute(&grammar.cfg, Default::default())?;

    let codegen = Codegen::new(&grammar, &table);
    let mut generated: Vec<u8> = codegen.to_string().into();

    // attempt to apply rustfmt to generated code.
    let sh = xshell::Shell::new()?;
    let res = xshell::cmd!(sh, "rustfmt --emit=stdout --color=never --quiet")
        .quiet()
        .stdin(&generated)
        .output();
    if let Ok(output) = res {
        if output.status.success() {
            generated = output.stdout;
        }
    }

    // dump results.
    if out_file.exists() {
        fs::copy(&out_file, &backup_file).with_context(|| {
            anyhow::anyhow!(
                "failed to backup the output file to {}",
                backup_file.display()
            )
        })?;
    }
    fs::write(&out_file, &generated).with_context(|| {
        anyhow::anyhow!("failed to write generated parser to {}", out_file.display())
    })?;

    Ok(())
}
