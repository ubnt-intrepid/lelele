use lelele::{codegen::Codegen, dfa::DFA};
use std::{env, fs, io::Write, path::PathBuf};
use tracing::Level;

fn main() {
    tracing_subscriber::fmt()
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .with_max_level(Level::TRACE)
        .init();

    let project_root = env::var_os("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .unwrap();

    // 文法定義から構文解析表を導出する
    println!("cargo:rerun-if-changed=min-caml.lll"); // 冗長なコード生成の抑制
    let grammar = lelele_syntax::grammar_from_file(&project_root.join("min-caml.lll")).unwrap();
    fs::write(project_root.join("min-caml.grammar"), grammar.to_string()).unwrap();

    let dfa = DFA::generate(&grammar);
    fs::write(
        project_root.join("min-caml.automaton"),
        dfa.display(&grammar).to_string(),
    )
    .unwrap();

    // 生成された構文解析表をコードに出力
    let codegen = Codegen::new(&grammar, &dfa);
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("parser.rs");
    let mut out = fs::File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(out_path)
        .unwrap();
    write!(out, "{}", codegen).unwrap();
}
