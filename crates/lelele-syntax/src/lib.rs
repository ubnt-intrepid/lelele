//! Syntax support for Lelele grammar file.

pub mod ast;
pub mod lexer;
pub mod parser;

use anyhow::Context as _;
use lelele::grammar::{Assoc, Grammar, Precedence};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

pub fn grammar_from_file(path: &Path) -> anyhow::Result<Grammar> {
    let source =
        fs::read_to_string(path).with_context(|| format!("during read {}", path.display()))?;
    grammar_from_str(&source)
}

pub fn grammar_from_str(source: &str) -> anyhow::Result<Grammar> {
    let grammar = crate::parser::parse(source)?;
    Grammar::define(|g| {
        let mut precedences = HashMap::new();
        let mut next_priority = 0;
        let mut symbols = HashMap::new();

        for desc in &grammar.descs {
            match desc {
                ast::Desc::Prec(ast::PrecDesc { configs, ident }) => {
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
                ast::Desc::Terminal(ast::TerminalDesc { configs, idents }) => {
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
                        let symbol = g.terminal(&*name, prec)?;
                        symbols.insert(name, symbol);
                    }
                }

                ast::Desc::Nonterminal(ast::NonterminalDesc { idents }) => {
                    for name in idents {
                        let symbol = g.nonterminal(&*name)?;
                        symbols.insert(name, symbol);
                    }
                }

                ast::Desc::Start(ast::StartDesc { name }) => {
                    let start_symbol = symbols
                        .get(name)
                        .copied()
                        .ok_or_else(|| format!("unknown start symbol: `{}'", name))?;
                    g.start_symbol(start_symbol)?;
                }

                ast::Desc::Rule(ast::RuleDesc { left, productions }) => {
                    let left = symbols
                        .get(left)
                        .copied()
                        .ok_or_else(|| format!("unknown left symbol: `{}'", left))?;
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
                            let symbol = symbols
                                .get(symbol)
                                .copied()
                                .ok_or_else(|| format!("unknown symbol: `{}'", symbol))?;
                            right.push(symbol);
                        }

                        g.rule(left, right, prec)?;
                    }
                }
            }
        }

        Ok(())
    })
    .context("during construct Grammar")
}
