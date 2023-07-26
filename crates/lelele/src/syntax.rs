pub mod ast;
pub mod grammar;
pub mod lexer;
pub mod parser;

use crate::{
    ielr::cfg::{Assoc, Grammar, NonterminalID, Precedence, SymbolID, TerminalID},
    types::Map,
};
use anyhow::{anyhow, Context as _};
use std::{fs, path::Path};

#[derive(Debug)]
pub struct GrammarFile {
    pub cfg: Grammar,
    pub terminals: Map<TerminalID, String>,
    pub nonterminals: Map<NonterminalID, String>,
}

pub fn parse_file(path: &Path) -> anyhow::Result<GrammarFile> {
    use ast::*;

    let input = fs::read_to_string(path)
        .with_context(|| anyhow!("failed to read grammar file: {}", path.display()))?;

    let mut ast = self::parser::parse(&input).with_context(|| anyhow!("syntax error"))?;

    ast.stmts.sort_by(|s1, s2| {
        use std::cmp::Ordering::*;
        let cmp = match s1 {
            Stmt::PrecDesc(..) => match s2 {
                Stmt::PrecDesc(..) => Equal,
                _ => Greater,
            },
            Stmt::TerminalDesc(..) => match s2 {
                Stmt::PrecDesc(..) => Less,
                Stmt::TerminalDesc(..) => Equal,
                _ => Greater,
            },
            Stmt::NonterminalDesc(..) => match s2 {
                Stmt::RuleDesc(..) | Stmt::StartDesc(..) => Greater,
                Stmt::NonterminalDesc(..) => Equal,
                Stmt::TerminalDesc(..) | Stmt::PrecDesc(..) => Less,
            },
            Stmt::RuleDesc(..) => match s2 {
                Stmt::StartDesc(..) => Greater,
                Stmt::RuleDesc(..) => Equal,
                _ => Less,
            },
            Stmt::StartDesc(..) => match s2 {
                Stmt::StartDesc(..) => Equal,
                _ => Less,
            },
        };
        cmp.reverse()
    });

    let mut terminals = Map::default();
    let mut nonterminals = Map::default();

    let cfg = Grammar::define(|g| {
        let mut added_terminals = Map::default();
        let mut added_nonterminals = Map::default();
        let mut precedences = Map::default();
        let mut next_priority = 0;

        for desc in &ast.stmts {
            match desc {
                Stmt::PrecDesc(PrecDesc { configs, ident }) => {
                    let mut assoc = None;
                    for config in configs {
                        match (&*config.key, &*config.value) {
                            ("assoc", "left") => assoc = Some(Assoc::Left),
                            ("assoc", "right") => assoc = Some(Assoc::Right),
                            ("assoc", "none" | "nonassoc") => assoc = Some(Assoc::Nonassoc),
                            _ => return Err("unexpected config in @prec desc".into()),
                        }
                    }
                    let assoc = assoc.unwrap_or(Assoc::Nonassoc);
                    precedences.insert(ident.to_string(), Precedence::new(next_priority, assoc));
                    next_priority += 1;
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
                            precedences
                                .get(&prec)
                                .copied()
                                .ok_or_else(|| format!("missing precedence name: `{}'", prec))?,
                        ),
                        None => None,
                    };
                    for name in idents {
                        if !verify_ident(name) {
                            return Err(format!(
                                "The terminal `{}' is not valid Rust identifier",
                                name
                            )
                            .into());
                        }
                        let symbol = g.terminal(prec)?;
                        added_terminals.insert(name, symbol);
                    }
                }

                Stmt::NonterminalDesc(NonterminalDesc { idents }) => {
                    for name in idents {
                        if !verify_ident(name) {
                            return Err(format!(
                                "The nonterminal `{}' is not valid Rust identifier",
                                name
                            )
                            .into());
                        }
                        let symbol = g.nonterminal()?;
                        added_nonterminals.insert(name, symbol);
                    }
                }

                Stmt::StartDesc(StartDesc { name }) => {
                    let start_symbol = added_nonterminals
                        .get(name)
                        .copied()
                        .ok_or_else(|| format!("unknown start symbol: `{}'", name))?;
                    g.start_symbol(start_symbol)?;
                }

                Stmt::RuleDesc(RuleDesc { left, productions }) => {
                    let left = match added_nonterminals.get(left) {
                        Some(s) => *s,
                        None => {
                            // 未登場の記号は非終端記号と解釈する
                            if !verify_ident(left) {
                                return Err(format!(
                                    "The nonterminal `{}' is not valid Rust identifier",
                                    left
                                )
                                .into());
                            }
                            let id = g.nonterminal()?;
                            added_nonterminals.insert(left, id);
                            id
                        }
                    };

                    for production in productions {
                        let mut prec = None;
                        for config in &production.configs {
                            match &*config.key {
                                "prec" => {
                                    prec =
                                        Some(precedences.get(&config.value).copied().ok_or_else(
                                            || {
                                                format!(
                                                    "unknown precedence name: `{}'",
                                                    config.value
                                                )
                                            },
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
                                    let s = added_terminals
                                        .get(symbol)
                                        .map(|t| SymbolID::T(*t)) //
                                        .or_else(|| {
                                            added_nonterminals.get(symbol).map(|n| SymbolID::N(*n))
                                        });
                                    match s {
                                        Some(s) => s,
                                        None => {
                                            // 未登場の記号は非終端記号と解釈する
                                            if !verify_ident(symbol) {
                                                return Err(format!("The nonterminal `{}' is not valid Rust identifier", symbol).into());
                                            }
                                            let id = g.nonterminal()?;
                                            added_nonterminals.insert(symbol, id);
                                            SymbolID::N(id)
                                        }
                                    }
                                }
                                ErrorToken => SymbolID::T(TerminalID::ERROR),
                            };
                            right.push(symbol);
                        }

                        let _id = g.rule(left, right, prec)?;
                    }
                }
            }
        }

        for (name, id) in added_terminals {
            terminals.insert(id, name.clone());
        }
        for (name, id) in added_nonterminals {
            nonterminals.insert(id, name.clone());
        }

        Ok(())
    })?;

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
