use anyhow::Context;
use lelele::{
    codegen::ParserDefinition,
    dfa::DFA,
    grammar::{Grammar, GrammarDef},
};
use lelele_tests::grammars;
use std::{env, fs, path::PathBuf};

fn main() -> anyhow::Result<()> {
    println!("cargo:rerun-if-changed=build.rs");

    generate_parser("g_simple1", grammars::g_simple1).context("generating g_simple1")?;

    Ok(())
}

fn generate_parser(
    name: &str,
    grammar_def: impl FnOnce(&mut GrammarDef<'_>),
) -> anyhow::Result<()> {
    let grammar = Grammar::define(grammar_def);
    let dfa = DFA::generate(&grammar);
    let parser_def = ParserDefinition::new(&grammar, &dfa);

    let out_dir = env::var("OUT_DIR").map(PathBuf::from).unwrap().join(name);
    fs::create_dir_all(&out_dir)
        .with_context(|| format!("create_dir_all({})", out_dir.display()))?;
    let out_path = out_dir.join("parser.rs");
    fs::write(&out_path, parser_def.to_string())
        .with_context(|| format!("write({}, <parser definition>)", out_path.display()))?;

    Ok(())
}
