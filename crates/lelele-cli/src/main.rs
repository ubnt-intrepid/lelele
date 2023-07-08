use anyhow::Context as _;
use clap::Parser;
use lelele::{codegen::Codegen, dfa::DFA, grammar::Grammar};
use std::{
    fs,
    path::{Path, PathBuf},
};
use tracing_subscriber::EnvFilter;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The path of grammar definition file.
    input: Vec<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let args = Args::parse();
    tracing::trace!("parsed CLI args = {:?}", args);

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
    let expanded_file = in_file.with_extension("lll.expanded");
    let automaton_file = in_file.with_extension("lll.automaton");

    let grammar = Grammar::from_file(&in_file)?;

    let mut empty_nonterminals = vec![];
    for nonterminal in grammar.nonterminals() {
        if grammar.rules().all(|rule| rule.left() != nonterminal.id()) {
            empty_nonterminals.extend(nonterminal.export_name());
        }
    }
    if !empty_nonterminals.is_empty() {
        println!(
            "[warning] The following nonterminals have no associated production rule: {:?}",
            empty_nonterminals
        );
    }

    let dfa = DFA::generate(&grammar)?;

    let mut num_inconsist_states = 0;
    for (_, node) in dfa.nodes() {
        let mut has_inconsistent_action = false;
        for (_terminal, action) in node.actions() {
            has_inconsistent_action |= !action.is_consistent();
        }
        if has_inconsistent_action {
            num_inconsist_states += 1;
        }
    }
    if num_inconsist_states > 0 {
        let suffix = if num_inconsist_states == 1 { "" } else { "s" };
        println!(
            "[warning] The automaton has {} inconsistent state{}. See {} for details.",
            num_inconsist_states,
            suffix,
            automaton_file.display()
        );
    }

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

    // dump results.
    fs::write(&expanded_file, grammar.to_string()).context("writing .grammar")?;
    fs::write(&automaton_file, dfa.display(&grammar).to_string()).context("writing .automaton")?;
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
