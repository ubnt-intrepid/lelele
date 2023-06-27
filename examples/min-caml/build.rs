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
    let grammar = Grammar::define(min_caml).unwrap();
    fs::write(project_root.join("min-caml.grammar"), grammar.to_string()).unwrap();

    let dfa = DFA::generate(&grammar);
    fs::write(project_root.join("min-caml.automaton"), dfa.to_string()).unwrap();

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

fn min_caml(g: &mut GrammarDef<'_>) -> Result<(), GrammarDefError> {
    // imported from: https://github.com/esumii/min-caml/blob/master/parser.mly

    let mut prec = {
        let mut next_priority = 0;
        move |assoc| {
            let priority = next_priority;
            next_priority += 1;
            Precedence::new(priority, assoc)
        }
    };
    let prec_in = prec(Assoc::Nonassoc);
    let prec_let = prec(Assoc::Right);
    let prec_semicolon = prec(Assoc::Right);
    let prec_if = prec(Assoc::Right);
    let prec_less_minus = prec(Assoc::Right);
    let prec_tuple = prec(Assoc::Nonassoc);
    let prec_comma = prec(Assoc::Left);
    let prec_cmp = prec(Assoc::Left);
    let prec_add = prec(Assoc::Left);
    let prec_mul = prec(Assoc::Left);
    let prec_neg = prec(Assoc::Right);
    let prec_app = prec(Assoc::Left);
    let prec_dot = prec(Assoc::Left);

    let l_paren = g.terminal("LPAREN", None)?;
    let r_paren = g.terminal("RPAREN", None)?;
    let t_true = g.terminal("TRUE", None)?;
    let t_false = g.terminal("FALSE", None)?;
    let integer = g.terminal("INTEGER", None)?;
    let float = g.terminal("FLOAT", None)?;
    let ident = g.terminal("IDENT", None)?;
    let t_not = g.terminal("NOT", None)?;
    let plus = g.terminal("PLUS", Some(prec_add))?;
    let plus_dot = g.terminal("PLUS_DOT", Some(prec_add))?;
    let minus = g.terminal("MINUS", Some(prec_add))?;
    let minus_dot = g.terminal("MINUS_DOT", Some(prec_add))?;
    let star_dot = g.terminal("STAR_DOT", Some(prec_mul))?;
    let slash_dot = g.terminal("SLASH_DOT", Some(prec_mul))?;
    let equal = g.terminal("EQUAL", Some(prec_cmp))?;
    let less_greater = g.terminal("LESS_GREATER", Some(prec_cmp))?;
    let less = g.terminal("LESS", Some(prec_cmp))?;
    let greater = g.terminal("GREATER", Some(prec_cmp))?;
    let less_equal = g.terminal("LESS_EQUAL", Some(prec_cmp))?;
    let greater_equal = g.terminal("GREATER_EQUAL", Some(prec_cmp))?;
    let less_minus = g.terminal("LESS_MINUS", Some(prec_less_minus))?;
    let comma = g.terminal("COMMA", Some(prec_comma))?;
    let semicolon = g.terminal("SEMICOLON", Some(prec_semicolon))?;
    let t_if = g.terminal("IF", None)?;
    let t_then = g.terminal("THEN", None)?;
    let t_else = g.terminal("ELSE", None)?;
    let t_let = g.terminal("LET", None)?;
    let t_rec = g.terminal("REC", None)?;
    let t_in = g.terminal("IN", Some(prec_in))?;
    let array_make = g.terminal("ARRAY_MAKE", None)?;
    let dot = g.terminal("DOT", Some(prec_dot))?;

    let simple_exp = g.nonterminal("SIMPLE_EXP")?;
    let exp = g.nonterminal("EXP")?;
    let formal_args = g.nonterminal("FORMAL_ARGS")?;
    let actual_args = g.nonterminal("ACTUAL_ARGS")?;
    let fundef = g.nonterminal("FUNDEF")?;
    let pat = g.nonterminal("PAT")?;
    let elems = g.nonterminal("ELEMS")?;

    g.start_symbol(exp)?;

    g.rule(
        simple_exp,
        [l_paren, exp, r_paren],
        "SIMPLE_EXP_PAREN",
        None,
    )?;
    g.rule(simple_exp, [l_paren, r_paren], "SIMPLE_EXP_UNIT", None)?;
    g.rule(simple_exp, [t_true], "SIMPLE_EXP_TRUE", None)?;
    g.rule(simple_exp, [t_false], "SIMPLE_EXP_FALSE", None)?;
    g.rule(simple_exp, [integer], "SIMPLE_EXP_INT", None)?;
    g.rule(simple_exp, [float], "SIMPLE_EXP_FLOAT", None)?;
    g.rule(simple_exp, [ident], "SIMPLE_EXP_IDENT", None)?;
    g.rule(
        simple_exp,
        [simple_exp, dot, l_paren, exp, r_paren],
        "SIMPLE_EXP_ARRAY_GET",
        None,
    )?;

    g.rule(exp, [simple_exp], "EXP_REDIRECT", None)?;
    g.rule(exp, [t_not, exp], "EXP_NOT", Some(prec_app))?;
    g.rule(exp, [minus, exp], "EXP_NEG", Some(prec_neg))?;
    g.rule(exp, [minus_dot, exp], "EXP_NEG_DOT", Some(prec_neg))?;
    g.rule(exp, [exp, plus, exp], "EXP_ADD", None)?;
    g.rule(exp, [exp, minus, exp], "EXP_SUB", None)?;
    g.rule(exp, [exp, plus_dot, exp], "EXP_FADD", None)?;
    g.rule(exp, [exp, minus_dot, exp], "EXP_FSUB", None)?;
    g.rule(exp, [exp, star_dot, exp], "EXP_FMUL", None)?;
    g.rule(exp, [exp, slash_dot, exp], "EXP_FDIV", None)?;
    g.rule(exp, [exp, equal, exp], "EXP_EQUAL", None)?;
    g.rule(exp, [exp, less_greater, exp], "EXP_LESS_GREATER", None)?;
    g.rule(exp, [exp, less, exp], "EXP_LESS", None)?;
    g.rule(exp, [exp, greater, exp], "EXP_GREATER", None)?;
    g.rule(exp, [exp, less_equal, exp], "EXP_LESS_EQ", None)?;
    g.rule(exp, [exp, greater_equal, exp], "EXP_GREATER_EQ", None)?;
    g.rule(
        exp,
        [t_if, exp, t_then, exp, t_else, exp],
        "EXP_IF",
        Some(prec_if),
    )?;
    g.rule(
        exp,
        [t_let, ident, equal, exp, t_in, exp],
        "EXP_LET",
        Some(prec_let),
    )?;
    g.rule(
        exp,
        [t_let, t_rec, fundef, t_in, exp],
        "EXP_LET_REC",
        Some(prec_let),
    )?;
    g.rule(
        exp,
        [t_let, l_paren, pat, r_paren, equal, exp, t_in, exp],
        "EXP_LET_TUPLE",
        None,
    )?;
    g.rule(exp, [simple_exp, actual_args], "EXP_APP", Some(prec_app))?;
    g.rule(exp, [elems], "EXP_ELEMS", Some(prec_tuple))?;
    g.rule(
        exp,
        [simple_exp, dot, l_paren, exp, r_paren, less_minus, exp],
        "EXP_ARRAY_PUT",
        None,
    )?;
    g.rule(exp, [exp, semicolon, exp], "EXP_SEMICOLON", None)?;
    g.rule(
        exp,
        [array_make, simple_exp, simple_exp],
        "EXP_ARRAY_CREATE",
        Some(prec_app),
    )?;

    g.rule(fundef, [ident, formal_args, equal, exp], "FUNDEF", None)?;

    g.rule(formal_args, [ident, formal_args], "FORMAL_ARGS_LIST", None)?;
    g.rule(formal_args, [ident], "FORMAL_ARGS_ELEM", None)?;

    g.rule(
        actual_args,
        [actual_args, simple_exp],
        "ACTUAL_ARGS_LIST",
        Some(prec_app),
    )?;
    g.rule(
        actual_args,
        [simple_exp],
        "ACTUAL_ARGS_ELEM",
        Some(prec_app),
    )?;

    g.rule(elems, [elems, comma, exp], "ELEMS", None)?;
    g.rule(elems, [exp, comma, exp], "ELEMS_TAIL", None)?;

    g.rule(pat, [pat, comma, ident], "PAT_LIST", None)?;
    g.rule(pat, [ident, comma, ident], "PAT_TAIL", None)?;

    Ok(())
}
