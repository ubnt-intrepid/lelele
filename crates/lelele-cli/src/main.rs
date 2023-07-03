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
    let grammar_name = in_file
        .file_stem()
        .context("invalid grammar file name")?
        .to_str()
        .context("grammar file name is not valid UTF-8")?;

    let out_dir = in_file
        .parent()
        .context("failed to obtain the parent directory path")?;
    let out_file = out_dir.join(format!("{}.rs", grammar_name));
    let expanded_file = out_dir.join(format!("{}.grammar", grammar_name));
    let automaton_file = out_dir.join(format!("{}.automaton", grammar_name));

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

    fs::write(&out_file, &generated).with_context(|| {
        anyhow::anyhow!("failed to write generated parser to {}", out_file.display())
    })?;

    Ok(())
}
