use lelele::codegen::Codegen;
use std::{env, fs, path::PathBuf};

fn main() {
    let manifest_dir = env::var_os("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .unwrap();
    let out_dir = env::var_os("OUT_DIR").map(PathBuf::from).unwrap();

    let input_path = manifest_dir.join("arithmetic.lll");
    let output_path = out_dir.join("parser.rs");

    let grammar = lelele::syntax::parse_file(&input_path).unwrap();
    let table = lelele::ielr::compute(&grammar.cfg, Default::default()).unwrap();

    let codegen = Codegen::new(&grammar, &table);
    fs::write(&output_path, codegen.to_string()).unwrap();
}
