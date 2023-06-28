use anyhow::Context as _;
use lelele::{
    codegen::Codegen,
    dfa::DFA,
    grammar::{Grammar, GrammarDef, GrammarDefError},
};
use std::{env, fs, io::Write, path::PathBuf};

fn main() -> anyhow::Result<()> {
    let project_root = env::var_os("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .context("obtaining project root")?;

    let out_project_root = project_root
        .parent()
        .context("project_root is root directory")?
        .join("lelele-syntax");

    let grammar = Grammar::define(grammar_def).unwrap();
    fs::write(project_root.join("lelele.grammar"), grammar.to_string())
        .context("writing .grammar")?;

    let dfa = DFA::generate(&grammar);
    fs::write(
        project_root.join("lelele.automaton"),
        dfa.display(&grammar).to_string(),
    )
    .context("writing .automaton")?;

    let codegen = Codegen::new(&grammar, &dfa);
    let out_path = out_project_root.join("src/parser/gen.rs");
    let mut out = fs::File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(out_path)
        .context("opening parser file")?;
    write!(out, "{}", codegen).context("writing parser")?;

    Ok(())
}

#[allow(non_snake_case)]
fn grammar_def(g: &mut GrammarDef<'_>) -> Result<(), GrammarDefError> {
    let LBRACKET = g.terminal("LBRACKET", None)?;
    let RBRACKET = g.terminal("RBRACKET", None)?;
    let AT_LBRACKET = g.terminal("AT_LBRACKET", None)?;
    let COLON_EQ = g.terminal("COLON_EQ", None)?;
    let EQ = g.terminal("EQ", None)?;
    let COMMA = g.terminal("COMMA", None)?;
    let SEMICOLON = g.terminal("SEMICOLON", None)?;
    let VERT_BAR = g.terminal("VERT_BAR", None)?;
    let TERMINAL = g.terminal("TERMINAL", None)?;
    let NONTERMINAL = g.terminal("NONTERMINAL", None)?;
    let START = g.terminal("START", None)?;
    let RULE = g.terminal("RULE", None)?;
    let EMPTY = g.terminal("EMPTY", None)?;
    let PREC = g.terminal("PREC", None)?;
    let IDENT = g.terminal("IDENT", None)?;

    let Grammar = g.nonterminal("Grammar")?;
    let Descs = g.nonterminal("Descs")?;
    let Desc = g.nonterminal("Desc")?;
    let TerminalDesc = g.nonterminal("TerminalDesc")?;
    let NonterminalDesc = g.nonterminal("NonterminalDesc")?;
    let StartDesc = g.nonterminal("StartDesc")?;
    let RuleDesc = g.nonterminal("RuleDesc")?;
    let PrecDesc = g.nonterminal("PrecDesc")?;
    let Productions = g.nonterminal("Productions")?;
    let Production = g.nonterminal("Production")?;
    let Configs = g.nonterminal("Configs")?;
    let Config = g.nonterminal("Config")?;
    let Idents = g.nonterminal("Idents")?;
    let Elems = g.nonterminal("Elems")?;
    g.start_symbol(Grammar)?;

    g.rule(Grammar, [Descs], None)?;

    g.rule(Descs, [], None)?;
    g.rule(Descs, [Descs, Desc, SEMICOLON], None)?;

    g.rule(Desc, [TerminalDesc], None)?;
    g.rule(Desc, [NonterminalDesc], None)?;
    g.rule(Desc, [RuleDesc], None)?;
    g.rule(Desc, [PrecDesc], None)?;
    g.rule(Desc, [StartDesc], None)?;

    g.rule(PrecDesc, [PREC, LBRACKET, Configs, RBRACKET, IDENT], None)?;

    g.rule(TerminalDesc, [TERMINAL, Idents], None)?;
    g.rule(
        TerminalDesc,
        [TERMINAL, LBRACKET, Configs, RBRACKET, Idents],
        None,
    )?;

    g.rule(NonterminalDesc, [NONTERMINAL, Idents], None)?;

    g.rule(StartDesc, [START, IDENT], None)?;

    g.rule(RuleDesc, [RULE, IDENT, COLON_EQ, Productions], None)?;
    g.rule(
        RuleDesc,
        [RULE, IDENT, COLON_EQ, VERT_BAR, Productions],
        None,
    )?;

    g.rule(Productions, [Production], None)?;
    g.rule(Productions, [Productions, VERT_BAR, Production], None)?;

    g.rule(Production, [EMPTY], None)?;
    g.rule(Production, [Elems], None)?;
    g.rule(Production, [AT_LBRACKET, Configs, RBRACKET, Elems], None)?;

    g.rule(Elems, [IDENT], None)?;
    g.rule(Elems, [IDENT, Elems], None)?;

    g.rule(Idents, [IDENT], None)?;
    g.rule(Idents, [IDENT, COMMA], None)?;
    g.rule(Idents, [IDENT, COMMA, Idents], None)?;

    g.rule(Configs, [Config], None)?;
    g.rule(Configs, [Config, COMMA], None)?;
    g.rule(Configs, [Config, COMMA, Configs], None)?;

    g.rule(Config, [IDENT, EQ, IDENT], None)?;

    Ok(())
}
