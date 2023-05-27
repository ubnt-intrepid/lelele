use lelele::{grammar::Grammar, parser::ParserDefinition};
use std::{env, fs, path::PathBuf};

#[allow(non_snake_case)]
fn main() {
    println!("cargo:rerun-if-changed=dummy.txt");

    let mut def = Grammar::definition();

    let t_ident = def.token("ID");
    let t_num = def.token("NUM");
    let t_plus = def.token("PLUS");
    let t_equal = def.token("EQUAL");

    let e_A = def.symbol("A");
    let e_E = def.symbol("E");
    let e_T = def.symbol("T");

    def.rule("R1", e_A, [e_E, t_equal, e_E]);
    def.rule("R2", e_A, [t_ident]);
    def.rule("R3", e_E, [e_E, t_plus, e_T]);
    def.rule("R4", e_E, [e_T]);
    def.rule("R5", e_T, [t_num]);
    def.rule("R6", e_T, [t_ident]);

    def.start(e_A);

    let grammar = def.end();

    // 上の文法定義から導出される構文解析表
    let parser_def = ParserDefinition::new(&grammar);

    //
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("parser.rs");
    let mut out = fs::File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(out_path)
        .unwrap();
    parser_def.generate(&mut out).unwrap();
}
