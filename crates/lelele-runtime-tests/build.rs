use lelele::codegen::Codegen;
use std::{env, fs, path::PathBuf};

fn main() {
    let manifest_dir = env::var_os("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .unwrap();
    let grammars_dir = manifest_dir.join("../lelele/tests").canonicalize().unwrap();
    let out_dir = env::var_os("OUT_DIR").map(PathBuf::from).unwrap();

    for entry in fs::read_dir(&grammars_dir).unwrap() {
        let entry = entry.unwrap();
        let in_file = entry.path();
        if !in_file.is_file() {
            continue;
        }
        match in_file.extension().and_then(|ext| ext.to_str()) {
            Some("lll") => (),
            _ => continue,
        }
        println!("cargo:rerun-if-changed={}", in_file.display());

        let out_file = out_dir
            .join(in_file.file_stem().unwrap())
            .with_extension("rs");
        if let Some(out_dir) = out_file.parent() {
            fs::create_dir_all(&out_dir).unwrap();
        }

        let grammar_file = lelele::syntax::parse_file(&in_file).unwrap();
        let table = lelele::ielr::compute(&grammar_file.cfg, Default::default()).unwrap();

        let codegen = Codegen::new(&grammar_file, &table);
        fs::write(&out_file, codegen.to_string()).unwrap();
    }
}
