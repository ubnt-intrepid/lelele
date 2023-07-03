//! code generation.

use crate::{
    dfa::{Action, NodeID, DFA},
    grammar::{Grammar, RuleID},
};
use std::fmt;

trait FmtWriteExt: fmt::Write {
    fn line(&mut self, line: &str) -> fmt::Result {
        self.write_str(line)?;
        self.write_str("\n")?;
        Ok(())
    }

    fn bracket<F>(&mut self, prefix: &str, body: F) -> fmt::Result
    where
        F: FnOnce(&mut Self) -> fmt::Result,
    {
        self.write_str(prefix)?;
        self.write_str(" {")?;
        body(&mut *self)?;
        self.write_str("}\n")?;
        Ok(())
    }
}
impl<W: fmt::Write> FmtWriteExt for W {}

#[derive(Debug)]
pub struct Codegen<'g> {
    grammar: &'g Grammar,
    dfa: &'g DFA,
}

impl<'g> Codegen<'g> {
    pub fn new(grammar: &'g Grammar, dfa: &'g DFA) -> Self {
        Self { grammar, dfa }
    }

    fn fmt_preamble(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.line("// This file is automatically generated by lelele.")?;
        f.line("use ::lelele_runtime::_private as lelele;")?;
        Ok(())
    }

    fn fmt_node_id_def(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.line("/// The type to identify DFA state nodes.")?;
        f.line("#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]")?;
        f.bracket("pub enum NodeID", |f| {
            for (id, _) in self.dfa.nodes() {
                writeln!(f, "N{},", id)?;
            }
            Ok(())
        })?;
        writeln!(
            f,
            "const __START_NODE: NodeID = NodeID::N{};",
            NodeID::START
        )?;
        Ok(())
    }

    fn fmt_token_id_def(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.line("/// The type to identify terminal or nonterminal symbols used in generated DFA.")?;
        f.line("#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]")?;
        f.line("#[allow(non_camel_case_types)]")?;
        f.bracket("pub enum TokenID", |f| {
            for terminal in self.grammar.terminals() {
                let export_name = match terminal.export_name() {
                    Some(name) => name,
                    None => continue,
                };
                writeln!(f, "/// Terminal `{}`.", export_name)?;
                writeln!(f, "{},", export_name)?;
            }
            Ok(())
        })
    }

    fn fmt_symbol_id_def(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.line("/// The type to identify nonterminal symbols used in generated DFA.")?;
        f.line("#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]")?;
        f.bracket("pub enum Symbol", |f| {
            for symbol in self.grammar.nonterminals() {
                let export_name = match symbol.export_name() {
                    Some(name) => name,
                    None => continue,
                };
                writeln!(f, "/// Nonterminal `{}`.", export_name)?;
                writeln!(f, "{},", export_name)?;
            }
            Ok(())
        })
    }

    fn fmt_parser_def(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.line("/// The generated LR(1) parser definition.")?;
        f.line("#[derive(Debug, Default)]")?;
        f.line("pub struct ParserDef {_p: () }")?;
        f.bracket("impl lelele::ParserDef for ParserDef", |f| {
            f.line("type State = NodeID;")?;
            f.line("type Token = TokenID;")?;
            f.line("type Symbol = Symbol;")?;

            f.line("#[inline]")?;
            f.bracket("fn initial_state(&self) -> Self::State", |f| {
                f.line("__START_NODE")
            })?;

            f.line("#[inline]")?;
            let prefix = "\
                fn action<TAction>(
                    &self,
                    current: Self::State,
                    lookahead: Option<Self::Token>,
                    action: TAction,
                ) -> Result<TAction::Ok, TAction::Error>
                where
                    TAction: lelele::ParseAction<
                        State = Self::State,
                        Token = Self::Token,
                        Symbol = Self::Symbol,
                    >,
            ";
            f.bracket(prefix, |f| {
                f.bracket("match __action(current, lookahead)", |f| {
                    f.line("Some(SimulatedAction::Shift(next)) => action.shift(next),")?;
                    f.line(
                        "Some(SimulatedAction::Reduce(symbol, n)) => action.reduce(symbol, n),",
                    )?;
                    f.line("Some(SimulatedAction::Accept) => action.accept(),")?;
                    f.line(
                        "None => action.fail(__expected_tokens(current).into_iter().copied()),",
                    )?;
                    Ok(())
                })
            })?;

            f.line("#[inline]")?;
            f.bracket(
                "fn goto(&self, current: Self::State, symbol: Self::Symbol) -> Self::State",
                |f| f.line("__goto(current, symbol).unwrap()"),
            )?;

            Ok(())
        })?;

        f.bracket("enum SimulatedAction", |f| {
            f.line("Shift(NodeID),")?;
            f.line("Reduce(Symbol, usize),")?;
            f.line("Accept,")?;
            Ok(())
        })?;

        f.bracket("const fn __action(current: NodeID, lookahead: Option<TokenID>) -> Option<SimulatedAction>", |f| {
            f.bracket("match current", |f| {
                for (id, node) in self.dfa.nodes() {
                    write!(f, "NodeID::N{} => ", id)?;
                    f.bracket("match lookahead", |f| {
                        for (lookahead, action) in node.actions() {
                            match self.grammar.terminal(&lookahead).export_name() {
                                Some(name) => write!(f, "Some(TokenID::{}) => ", name)?,
                                None => f.write_str("None => ")?,
                            };
                            match action {
                                Action::Shift(n) => write!(f, "Some(SimulatedAction::Shift(NodeID::N{}))", n)?,
                                Action::Reduce(rule) if *rule == RuleID::ACCEPT => {
                                    f.write_str("Some(SimulatedAction::Accept)")?
                                }
                                Action::Reduce(rule) => {
                                    let rule = self.grammar.rule(rule);
                                    let left = self
                                        .grammar
                                        .nonterminal(&rule.left())
                                        .export_name()
                                        .unwrap();
                                    write!(
                                        f,
                                        "Some(SimulatedAction::Reduce(Symbol::{left}, {n}))",
                                        left = left,
                                        n = rule.right().len(),
                                    )?;
                                }
                                _ => f.write_str("None")?,
                            };
                            f.write_str(",\n")?;
                        }

                        f.line("#[allow(unreachable_patterns)]")?;
                        f.line("_ => None,")?;

                        Ok(())
                    })?;

                }
                Ok(())
            })
        })?;

        f.bracket(
            "const fn __expected_tokens(current: NodeID) -> &'static [TokenID]",
            |f| {
                f.bracket("match current", |f| {
                    for (id, node) in self.dfa.nodes() {
                        write!(f, "NodeID::N{} => &[", id)?;
                        for (lookahead, _) in node.actions() {
                            // FIXME: export EOI
                            if let Some(name) = self.grammar.terminal(&lookahead).export_name() {
                                write!(f, "TokenID::{},", name)?;
                            }
                        }
                        f.write_str("],\n")?;
                    }
                    Ok(())
                })
            },
        )?;

        let prefix = "const fn __goto(current: NodeID, symbol: Symbol) -> Option<NodeID>";
        f.bracket(prefix, |f| {
            f.bracket("match current", |f| {
                let mut unused_nodes = vec![];
                for (id, node) in self.dfa.nodes() {
                    if node.gotos().count() == 0 {
                        unused_nodes.push(id);
                        continue;
                    }

                    writeln!(f, "NodeID::N{} => ", id)?;
                    f.bracket("match symbol", |f| {
                        for (symbol, target) in node.gotos() {
                            let symbol = self.grammar.nonterminal(&symbol).export_name().unwrap();
                            writeln!(f, "Symbol::{} => Some(NodeID::N{}),", symbol, target)?;
                        }

                        f.line("#[allow(unreachable_patterns)]")?;
                        f.line("_ => None,")?;

                        Ok(())
                    })?;
                }

                f.line("#[allow(unreachable_patterns)]")?;
                f.line("_ => None,")?;

                Ok(())
            })
        })?;

        Ok(())
    }
}

impl<'g> fmt::Display for Codegen<'g> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_preamble(f)?;
        self.fmt_node_id_def(f)?;
        self.fmt_token_id_def(f)?;
        self.fmt_symbol_id_def(f)?;
        self.fmt_parser_def(f)?;
        Ok(())
    }
}
