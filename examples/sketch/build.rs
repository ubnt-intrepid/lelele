use lelele::{
    codegen::ParserDefinition,
    dfa::DFA,
    grammar::{Choice, Grammar, GrammarDef},
};
use std::{env, fs, io::Write, path::PathBuf};

fn main() {
    // 冗長なコード生成の抑制
    println!("cargo:rerun-if-changed=build.rs");

    // 文法定義から構文解析表を導出する
    let grammar = Grammar::define(grammar_def);
    let dfa = DFA::generate(&grammar);
    eprintln!("Grammar:\n{}", grammar);

    let parser_def = ParserDefinition::new(&grammar, &dfa);

    // 生成された構文解析表をコードに出力
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("parser.rs");
    let mut out = fs::File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(out_path)
        .unwrap();
    write!(out, "{}", parser_def).unwrap();
}

fn grammar_def(def: &mut GrammarDef<'_>) {
    // declare terminal symbols.
    let lparen = def.token("LPAREN");
    let rparen = def.token("RPAREN");
    let plus = def.token("PLUS");
    let minus = def.token("MINUS");
    let star = def.token("STAR");
    let slash = def.token("SLASH");
    let num = def.token("NUM");
    let _ = def.token("UNUSED_0");

    // declare nonterminal symbols.
    let expr = def.symbol("EXPR");
    let factor = def.symbol("FACTOR");
    let term = def.symbol("TERM");
    let _ = def.symbol("UNUSED_1");

    def.start_symbol(expr);

    // declare syntax rules.
    def.rule(
        expr,
        Choice((
            (expr, plus, factor),  // expr '+' factor
            (expr, minus, factor), // expr '-' factor
            factor,                // factor
        )),
    );
    def.rule(
        factor,
        Choice((
            (factor, star, term),  // factor '*' term
            (factor, slash, term), // factor '/' term
            term,                  // term
        )),
    );
    def.rule(
        term,
        Choice((
            num,                    // num
            (lparen, expr, rparen), // '(' expr ')'
        )),
    );
}
