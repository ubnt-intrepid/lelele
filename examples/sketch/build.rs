use lelele::{
    codegen::ParserDefinition,
    dfa::DFA,
    grammar::{Grammar, GrammarDef, GrammarDefError},
};
use std::{env, fs, io::Write, path::PathBuf};

fn main() {
    // 冗長なコード生成の抑制
    println!("cargo:rerun-if-changed=build.rs");

    // 文法定義から構文解析表を導出する
    let grammar = Grammar::define(grammar_def).unwrap();
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

fn grammar_def(g: &mut GrammarDef<'_>) -> Result<(), GrammarDefError> {
    // declare terminal symbols.
    let lparen = g.token("LPAREN")?;
    let rparen = g.token("RPAREN")?;
    let plus = g.token("PLUS")?;
    let minus = g.token("MINUS")?;
    let star = g.token("STAR")?;
    let slash = g.token("SLASH")?;
    let num = g.token("NUM")?;
    g.token("UNUSED_0")?;

    // declare nonterminal symbols.
    let expr = g.symbol("EXPR")?;
    let factor = g.symbol("FACTOR")?;
    let term = g.symbol("TERM")?;
    g.symbol("UNUSED_1")?;

    g.start_symbol(expr)?;

    // declare syntax rules.

    g.rule(expr, [expr, plus, factor])?; // expr '+' factor
    g.rule(expr, [expr, minus, factor])?; // expr '-' factor
    g.rule(expr, [factor])?; // factor

    g.rule(factor, [factor, star, term])?; // factor '*' term
    g.rule(factor, [factor, slash, term])?; // factor '/' term
    g.rule(factor, [term])?; // term

    g.rule(term, [num])?; // num
    g.rule(term, [lparen, expr, rparen])?; // '(' expr ')'

    Ok(())
}
