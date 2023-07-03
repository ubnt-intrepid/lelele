use anyhow::Context;
use lelele::{codegen::Codegen, dfa::DFA, grammar::Grammar};
use std::{env, fs, path::PathBuf};

fn main() -> anyhow::Result<()> {
    generate_parser("arithmetic").context("generating g_arithmetic")?;
    generate_parser("arithmetic_prec").context("generating g_arithmetic")?;

    Ok(())
}

fn generate_parser(name: &str) -> anyhow::Result<()> {
    let project_root = env::var("CARGO_MANIFEST_DIR").map(PathBuf::from).unwrap();
    let test_grammars_root = project_root
        .join("../lelele/tests")
        .canonicalize()
        .context("could not obtain test grammar file directory")?;

    let grammar_file = test_grammars_root.join(format!("{}.lll", name));
    println!("cargo:rerun-if-changed={}", grammar_file.display());

    let grammar = Grammar::from_file(&grammar_file)?;
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
