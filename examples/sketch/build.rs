use lelele::{
    codegen::Codegen,
    dfa::DFA,
    grammar::{Assoc, Grammar, GrammarDef, GrammarDefError, Precedence},
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
    fs::write(project_root.join("sketch.automaton"), dfa.to_string()).unwrap();

    let parse_table = ParseTable::generate(&dfa);

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
    // declare precedences.
    let prec_add = Precedence::new(0, Assoc::Left);
    let prec_mul = Precedence::new(1, Assoc::Left);
    let prec_neg = Precedence::new(2, Assoc::Right);

    // declare terminal symbols.
    let lparen = g.token("LPAREN")?;
    let rparen = g.token("RPAREN")?;
    let plus = g.token_with_prec("PLUS", Some(prec_add))?;
    let minus = g.token_with_prec("MINUS", Some(prec_add))?;
    let star = g.token_with_prec("STAR", Some(prec_mul))?;
    let slash = g.token_with_prec("SLASH", Some(prec_mul))?;
    let num = g.token("NUM")?;
    g.token("UNUSED_0")?;

    // declare nonterminal symbols.
    let expr = g.symbol("EXPR")?;

    g.start_symbol(expr)?;

    // declare production rules.
    g.rule("EXPR_ADD", expr, [expr, plus, expr])?;
    g.rule("EXPR_SUB", expr, [expr, minus, expr])?;
    g.rule("EXPR_MUL", expr, [expr, star, expr])?;
    g.rule("EXPR_DIV", expr, [expr, slash, expr])?;
    g.rule("EXPR_NUM", expr, [num])?;
    g.rule("EXPR_PAREN", expr, [lparen, expr, rparen])?;
    g.rule_with_prec("EXPR_NEG", expr, [minus, expr], Some(prec_neg))?;

    Ok(())
}
