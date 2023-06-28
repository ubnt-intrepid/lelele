use lelele::{
    codegen::Codegen,
    dfa::DFA,
    grammar::{Assoc, Grammar, GrammarDef, GrammarDefError, Precedence},
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
    fs::write(project_root.join("arithmetic.grammar"), grammar.to_string()).unwrap();

    let dfa = DFA::generate(&grammar);
    fs::write(
        project_root.join("arithmetic.automaton"),
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

fn grammar_def(g: &mut GrammarDef<'_>) -> Result<(), GrammarDefError> {
    // declare precedences.
    let prec_add = Precedence::new(0, Assoc::Left);
    let prec_mul = Precedence::new(1, Assoc::Left);
    let prec_neg = Precedence::new(2, Assoc::Right);

    // declare terminal symbols.
    let lparen = g.terminal("LPAREN", None)?;
    let rparen = g.terminal("RPAREN", None)?;
    let plus = g.terminal("PLUS", Some(prec_add))?;
    let minus = g.terminal("MINUS", Some(prec_add))?;
    let star = g.terminal("STAR", Some(prec_mul))?;
    let slash = g.terminal("SLASH", Some(prec_mul))?;
    let num = g.terminal("NUM", None)?;
    let _unused = g.terminal("UNUSED", None)?;

    // declare nonterminal symbols.
    let expr = g.nonterminal("EXPR")?;

    g.start_symbol(expr)?;

    // declare production rules.
    g.rule(expr, [expr, plus, expr], None)?;
    g.rule(expr, [expr, minus, expr], None)?;
    g.rule(expr, [expr, star, expr], None)?;
    g.rule(expr, [expr, slash, expr], None)?;
    g.rule(expr, [num], None)?;
    g.rule(expr, [lparen, expr, rparen], None)?;
    g.rule(expr, [minus, expr], Some(prec_neg))?;

    Ok(())
}
