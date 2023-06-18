use lelele::{
    codegen::Codegen,
    dfa::DFA,
    grammar::{Assoc, Grammar, GrammarDef, GrammarDefError},
    parse_table::ParseTable,
};
use std::{env, fs, io::Write, path::PathBuf};

fn main() {
    // 冗長なコード生成の抑制
    println!("cargo:rerun-if-changed=build.rs");

    let project_root = env::var_os("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .unwrap();

    // 文法定義から構文解析表を導出する
    let grammar = Grammar::define(grammar_def).unwrap();
    fs::write(project_root.join("sketch.grammar"), grammar.to_string()).unwrap();

    let dfa = DFA::generate(&grammar);
    fs::write(
        project_root.join("sketch.automaton"),
        dfa.display(&grammar).to_string(),
    )
    .unwrap();

    let parse_table = ParseTable::generate(&grammar, &dfa);

    // 生成された構文解析表をコードに出力
    let codegen = Codegen::new(&grammar, &parse_table);
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap()).join("parser.rs");
    let mut out = fs::File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(out_path)
        .unwrap();
    write!(out, "{}", codegen).unwrap();
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

    // The `bogus` tokens will never exported.
    let unary_minus = g.bogus_token()?;

    // declare nonterminal symbols.
    let expr = g.symbol("EXPR")?;

    g.start_symbol(expr)?;

    g.precedence(Assoc::Left, [plus, minus])?;
    g.precedence(Assoc::Left, [star, slash])?;
    g.precedence(Assoc::Right, [unary_minus])?;

    // declare production rules.
    g.rule("EXPR_ADD", expr, [expr, plus, expr])?;
    g.rule("EXPR_SUB", expr, [expr, minus, expr])?;
    g.rule("EXPR_MUL", expr, [expr, star, expr])?;
    g.rule("EXPR_DIV", expr, [expr, slash, expr])?;
    g.rule("EXPR_NUM", expr, [num])?;
    g.rule("EXPR_PAREN", expr, [lparen, expr, rparen])?;
    g.rule_with_prec("EXPR_NEG", expr, [minus, expr], Some(unary_minus))?;

    Ok(())
}
