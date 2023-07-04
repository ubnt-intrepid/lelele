use anyhow::Context as _;
use clap::Parser;
use lelele::{codegen::Codegen, dfa::DFA, grammar::Grammar};
use std::{fs, path::PathBuf};
use tracing_subscriber::EnvFilter;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The path of grammar definition file.
    input: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    tracing::trace!("CLI args = {:?}", args);

    let in_file =
        fs::canonicalize(&args.input).context("failed to canonicalize the input file name")?;

    let out_file = in_file.with_extension("rs");
    let backup_file = in_file.with_extension("rs.bak");
    let expanded_file = in_file.with_extension("lll.expanded");
    let automaton_file = in_file.with_extension("lll.automaton");

    let grammar = Grammar::from_file(&in_file)?;
    fs::write(&expanded_file, grammar.to_string()).context("writing .grammar")?;

    let dfa = DFA::generate(&grammar);
    fs::write(&automaton_file, dfa.display(&grammar).to_string()).context("writing .automaton")?;

    let codegen = Codegen::new(&grammar, &dfa);
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

    fs::copy(&out_file, &backup_file).with_context(|| {
        anyhow::anyhow!(
            "failed to backup the output file to {}",
            backup_file.display()
        )
    })?;

    fs::write(&out_file, &generated).with_context(|| {
        anyhow::anyhow!("failed to write generated parser to {}", out_file.display())
    })?;

    Ok(())
}
