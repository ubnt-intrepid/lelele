use anyhow::Context;
use lelele::{
    codegen::Codegen,
    dfa::DFA,
    grammar::{Grammar, GrammarDef, GrammarDefError},
};
use lelele_tests::grammars;
use std::{env, fs, path::PathBuf};

fn main() -> anyhow::Result<()> {
    println!("cargo:rerun-if-changed=build.rs");

    generate_parser("g_arithmetic", grammars::g_arithmetic).context("generating g_arithmetic")?;
    generate_parser("g_arithmetic_prec", grammars::g_arithmetic_prec)
        .context("generating g_arithmetic")?;

    Ok(())
}

fn generate_parser(
    name: &str,
    grammar_def: impl FnOnce(&mut GrammarDef<'_>) -> Result<(), GrammarDefError>,
) -> anyhow::Result<()> {
    let project_root = env::var("CARGO_MANIFEST_DIR").map(PathBuf::from).unwrap();

    let grammar = Grammar::define(grammar_def).unwrap();
    fs::write(
        project_root.join(format!("{}.grammar", name)),
        grammar.to_string(),
    )?;

    let dfa = DFA::generate(&grammar);
    fs::write(
        project_root.join(format!("{}.automaton", name)),
        dfa.display(&grammar).to_string(),
    )?;

    let codegen = Codegen::new(&grammar, &dfa);

    let out_dir = env::var("OUT_DIR").map(PathBuf::from).unwrap().join(name);
    fs::create_dir_all(&out_dir)
        .with_context(|| format!("create_dir_all({})", out_dir.display()))?;
    let out_path = out_dir.join("parser.rs");
    fs::write(&out_path, codegen.to_string())
        .with_context(|| format!("write({}, <parser definition>)", out_path.display()))?;

    Ok(())
}
