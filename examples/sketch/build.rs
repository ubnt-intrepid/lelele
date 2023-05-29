use lelele::{
    codegen::ParserDefinition,
    grammar::{Grammar, GrammarDef},
};
use std::{env, fs, io::Write, path::PathBuf};

fn main() {
    // 冗長なコード生成の抑制
    println!("cargo:rerun-if-changed=build.rs");

    // 文法定義から構文解析表を導出する
    let grammar = Grammar::define(grammar_def);
    let parser_def = ParserDefinition::new(&grammar);

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

    // declare nonterminal symbols.
    let expr = def.symbol("EXPR");
    let factor = def.symbol("FACTOR");
    let term = def.symbol("TERM");

    def.start_symbol(expr);

    // declare syntax rules.

    // expr : expr '+' factor | expr '-' factor | factor ;
    def.rule(expr, [expr, plus, factor]);
    def.rule(expr, [expr, minus, factor]);
    def.rule(expr, [factor]);

    // factor : factor '*' term | factor '/' term | term ;
    def.rule(factor, [factor, star, term]);
    def.rule(factor, [factor, slash, term]);
    def.rule(factor, [term]);

    // term : num | '(' expr ')'
    def.rule(term, [num]);
    def.rule(term, [lparen, expr, rparen]);
}
