use anyhow::Context as _;
use lelele::{codegen::Codegen, dfa::DFA, grammar::Grammar};
use std::{env, fs, io::Write, path::Path};

fn main() -> anyhow::Result<()> {
    let project_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let in_file = project_root.join("src/parser/grammar.lll");
    let out_file = project_root.join("src/parser/gen.rs");
    let expanded_file = project_root.join("lelele.grammar");
    let automaton_file = project_root.join("lelele.automaton");

    let grammar = Grammar::from_file(&in_file)?;
    fs::write(&expanded_file, grammar.to_string()).context("writing .grammar")?;

    let dfa = DFA::generate(&grammar);
    fs::write(&automaton_file, dfa.display(&grammar).to_string()).context("writing .automaton")?;

    let codegen = Codegen::new(&grammar, &dfa);
    let mut out = fs::File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(&out_file)
        .context("opening parser file")?;
    write!(out, "{}", codegen).context("writing parser")?;

    Ok(())
}
