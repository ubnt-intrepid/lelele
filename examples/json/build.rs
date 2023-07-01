use lelele::{
    codegen::Codegen,
    dfa::DFA,
    grammar::{Grammar, GrammarDef, GrammarDefError},
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
    fs::write(project_root.join("json.grammar"), grammar.to_string()).unwrap();

    let dfa = DFA::generate(&grammar);
    fs::write(
        project_root.join("json.automaton"),
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
    let lbracket = g.terminal("LBRACKET", None)?;
    let rbracket = g.terminal("RBRACKET", None)?;
    let lbrace = g.terminal("LBRACE", None)?;
    let rbrace = g.terminal("RBRACE", None)?;
    let comma = g.terminal("COMMA", None)?;
    let colon = g.terminal("COLON", None)?;
    let null = g.terminal("NULL", None)?;
    let r#true = g.terminal("TRUE", None)?;
    let r#false = g.terminal("FALSE", None)?;
    let number = g.terminal("NUMBER", None)?;
    let string = g.terminal("STRING", None)?;

    let value = g.nonterminal("Value")?;
    let array = g.nonterminal("Array")?;
    let object = g.nonterminal("Object")?;
    let member = g.nonterminal("Member")?;
    let members = g.nonterminal("Members")?;
    let elements = g.nonterminal("Elements")?;

    g.start_symbol(value)?;

    g.rule(value, [object], None)?;
    g.rule(value, [array], None)?;
    g.rule(value, [string], None)?;
    g.rule(value, [number], None)?;
    g.rule(value, [r#true], None)?;
    g.rule(value, [r#false], None)?;
    g.rule(value, [null], None)?;

    g.rule(object, [lbracket, rbracket], None)?;
    g.rule(object, [lbracket, members, rbracket], None)?;

    g.rule(members, [member], None)?;
    g.rule(members, [member, comma, members], None)?;

    g.rule(member, [string, colon, value], None)?;

    g.rule(array, [lbrace, rbrace], None)?;
    g.rule(array, [lbrace, elements, rbrace], None)?;

    g.rule(elements, [value], None)?;
    g.rule(elements, [value, comma, elements], None)?;

    Ok(())
}
