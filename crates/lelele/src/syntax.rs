pub mod ast;
pub mod grammar;
pub mod lexer;

use crate::{
    ielr::cfg::{Assoc, Grammar, GrammarDef, NonterminalID, Precedence, SymbolID, TerminalID},
    types::Map,
};
use anyhow::{anyhow, bail, Context as _};
use std::{fs, path::Path};

#[derive(Debug)]
pub struct GrammarFile {
    pub cfg: Grammar,
    pub terminals: Map<TerminalID, String>,
    pub nonterminals: Map<NonterminalID, String>,
}

struct ParseContext {
    def: GrammarDef,
    terminals: Map<String, TerminalID>,
    nonterminals: Map<String, NonterminalID>,
    precedences: Map<String, Precedence>,
    next_priority: u16,
}
impl ParseContext {
    fn add_precedence(&mut self, ident: &str, assoc: Assoc) {
        self.precedences.insert(
            ident.to_string(),
            Precedence::new(self.next_priority, assoc),
        );
        self.next_priority += 1;
    }

    fn add_terminal(&mut self, name: &str, prec: Option<Precedence>) -> anyhow::Result<()> {
        if !verify_ident(name) {
            bail!("The terminal `{}' is not valid Rust identifier", name);
        }
        let symbol = self.def.terminal(prec);
        self.terminals.insert(name.to_owned(), symbol);
        Ok(())
    }

    fn add_nonterminal(&mut self, name: &str) -> anyhow::Result<NonterminalID> {
        if !verify_ident(name) {
            bail!("The nonterminal `{}' is not valid Rust identifier", name);
        }
        let symbol = self.def.nonterminal();
        self.nonterminals.insert(name.to_owned(), symbol);
        Ok(symbol)
    }
}

pub fn parse_file(path: &Path) -> anyhow::Result<GrammarFile> {
    use ast::*;

    let input = fs::read_to_string(path)
        .with_context(|| anyhow!("failed to read grammar file: {}", path.display()))?;

    let mut ast = ast::parse(&input).with_context(|| anyhow!("syntax error"))?;

    ast.stmts.sort_by(|s1, s2| s1.cmp_by_desc(s2));

    let mut cx = ParseContext {
        def: GrammarDef::default(),
        terminals: Map::default(),
        nonterminals: Map::default(),
        precedences: Map::default(),
        next_priority: 0,
    };
    for desc in &ast.stmts {
        match desc {
            Stmt::PrecDesc(PrecDesc { configs, ident }) => {
                let mut assoc = None;
                for config in configs {
                    match (&*config.key, &*config.value) {
                        ("assoc", "left") => assoc = Some(Assoc::Left),
                        ("assoc", "right") => assoc = Some(Assoc::Right),
                        ("assoc", "none" | "nonassoc") => assoc = Some(Assoc::Nonassoc),
                        _ => bail!("unexpected config in @prec desc"),
                    }
                }
                let assoc = assoc.unwrap_or(Assoc::Nonassoc);
                cx.add_precedence(ident, assoc);
            }

            Stmt::TerminalDesc(TerminalDesc { configs, idents }) => {
                let mut prec = None;
                for config in configs {
                    if config.key == "prec" {
                        prec = Some(config.value.clone());
                    }
                }
                let prec = match prec {
                    Some(prec) => Some(
                        cx.precedences
                            .get(&prec)
                            .copied()
                            .ok_or_else(|| anyhow!("missing precedence name: `{}'", prec))?,
                    ),
                    None => None,
                };
                for name in idents {
                    cx.add_terminal(name, prec)?;
                }
            }

            Stmt::NonterminalDesc(NonterminalDesc { idents }) => {
                for name in idents {
                    cx.add_nonterminal(name)?;
                }
            }

            Stmt::StartDesc(StartDesc { name }) => {
                let start_symbol = cx
                    .nonterminals
                    .get(name)
                    .copied()
                    .ok_or_else(|| anyhow!("unknown start symbol: `{}'", name))?;
                cx.def.start_symbol(start_symbol);
            }

            Stmt::RuleDesc(RuleDesc { left, productions }) => {
                let left = match cx.nonterminals.get(left) {
                    Some(s) => *s,
                    // 未登場の記号は非終端記号と解釈する
                    None => cx.add_nonterminal(left)?,
                };

                for production in productions {
                    let mut prec = None;
                    for config in &production.configs {
                        match &*config.key {
                            "prec" => {
                                prec =
                                    Some(cx.precedences.get(&config.value).copied().ok_or_else(
                                        || anyhow!("unknown precedence name: `{}'", config.value),
                                    )?);
                            }
                            _ => (),
                        }
                    }

                    let mut right = vec![];
                    for symbol in &production.elems {
                        use ProductionElem::*;
                        let symbol = match symbol {
                            Ident(symbol) => {
                                let s = cx
                                    .terminals
                                    .get(symbol)
                                    .map(|t| SymbolID::T(*t)) //
                                    .or_else(|| {
                                        cx.nonterminals.get(symbol).map(|n| SymbolID::N(*n))
                                    });
                                match s {
                                    Some(s) => s,
                                    None => {
                                        // 未登場の記号は非終端記号と解釈する
                                        let id = cx.add_nonterminal(symbol)?;
                                        SymbolID::N(id)
                                    }
                                }
                            }
                            ErrorToken => SymbolID::T(TerminalID::ERROR),
                        };
                        right.push(symbol);
                    }

                    let _id = cx.def.rule(left, right, prec);
                }
            }
        }
    }

    let cfg = cx.def.end();

    let mut terminals = Map::default();
    for (name, id) in cx.terminals {
        terminals.insert(id, name.clone());
    }

    let mut nonterminals = Map::default();
    for (name, id) in cx.nonterminals {
        nonterminals.insert(id, name.clone());
    }

    Ok(GrammarFile {
        cfg,
        terminals,
        nonterminals,
    })
}

fn verify_ident(mut s: &str) -> bool {
    if s.is_empty() {
        // The identifier must not be empty.
        return false;
    }

    if s.bytes().all(|b| b >= b'0' && b <= b'9') {
        // The number must not be identifer.
        return false;
    }

    if s.starts_with("r#") {
        s = &s[2..];
        if matches!(s, "crate" | "self" | "super" | "Self") {
            // unexpected raw identifier
            return false;
        }
    } else if is_strict_keyword(s) || is_reserved(s) {
        // Reserved keyword specified.
        return false;
    }

    let mut chars = s.chars();
    let first = chars.next().unwrap();
    if !is_ident_start(first) {
        // The identifier must be started with XID-Start.
        return false;
    }
    if chars.any(|ch| !is_ident_continue(ch)) {
        // The idenfier must be continued with XID-Continue.
        return false;
    }

    true
}

fn is_ident_start(ch: char) -> bool {
    ch == '_' || unicode_ident::is_xid_start(ch)
}

fn is_ident_continue(ch: char) -> bool {
    unicode_ident::is_xid_continue(ch)
}

fn is_strict_keyword(s: &str) -> bool {
    matches!(
        s,
        "as" | "break" | "const" | "continue" | "crate" | "else" | "enum" | "extern"
        | "false" | "fn" | "for" | "if" | "impl" | "in" | "let" | "loop" | "match" | "mod"
        | "move" | "mut" | "pub" | "ref" | "return" | "self" | "Self" | "static" | "struct"
        | "super" | "trait" | "true" | "type" | "unsafe" | "use" | "where" | "while"
        // since Rust 2018
        | "async" | "await" | "dyn"
    )
}

fn is_reserved(s: &str) -> bool {
    matches!(
        s,
        "abstract" | "become" | "box" | "do" | "final" | "macro" | "override" | "priv"
        | "typeof" | "unsized" | "virtual" | "yield"
        // since Rust 2018
        | "try"
    )
}
