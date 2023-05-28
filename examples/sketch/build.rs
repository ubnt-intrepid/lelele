use lelele::{grammar::Grammar, parser::ParserDefinition};
use std::{env, fs, path::PathBuf};

#[allow(non_snake_case)]
fn main() {
    // 文法定義から構文解析表を導出する
    let grammar = grammar();
    let parser_def = ParserDefinition::new(&grammar);

    // 生成された構文解析表をコードに出力
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("parser.rs");
    let mut out = fs::File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(out_path)
        .unwrap();
    parser_def.generate(&mut out).unwrap();
}

fn grammar() -> Grammar<'static> {
    let mut def = Grammar::definition();

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

    def.start(expr);

    // declare syntax rules.

    // expr : expr '+' factor | expr '-' factor | factor ;
    def.rule("EXPR_1", expr, [expr, plus, factor]);
    def.rule("EXPR_2", expr, [expr, minus, factor]);
    def.rule("EXPR_3", expr, [factor]);

    // factor : factor '*' term | factor '/' term | term ;
    def.rule("FACTOR_1", factor, [factor, star, term]);
    def.rule("FACTOR_2", factor, [factor, slash, term]);
    def.rule("FACTOR_3", factor, [term]);

    // term : num | '(' expr ')'
    def.rule("TERM_1", term, [num]);
    def.rule("TERM_2", term, [lparen, expr, rparen]);

    def.end()
}
