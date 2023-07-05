//! code generation.

use crate::{
    dfa::{Action, NodeID, DFA},
    grammar::{Grammar, TerminalID},
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
        })?;
        f.bracket("impl lelele::Token<TokenID> for TokenID", |f| {
            f.line("#[inline]")?;
            f.bracket("fn to_index(&self) -> TokenID", |f| f.line("*self"))
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
            f.line("type StateIndex = NodeID;")?;
            f.line("type TerminalIndex = TokenID;")?;
            f.line("type NonterminalIndex = Symbol;")?;

            f.line("#[inline]")?;
            f.bracket("fn initial_state(&self) -> Self::StateIndex", |f| {
                writeln!(f, "NodeID::N{}", NodeID::START)
            })?;

            f.line("#[inline]")?;
            let prefix = "\
                fn action<TAction>(
                    &self,
                    current: Self::StateIndex,
                    lookahead: Option<Self::TerminalIndex>,
                    action: TAction,
                ) -> Result<TAction::Ok, TAction::Error>
                where
                    TAction: lelele::ParseAction<
                        StateIndex = Self::StateIndex,
                        TerminalIndex = Self::TerminalIndex,
                        NonterminalIndex = Self::NonterminalIndex,
                    >,
            ";
            f.bracket(prefix, |f| {
                f.bracket("match __action(current, lookahead)", |f| {
                    f.line("Some(SimulatedAction::Shift(next)) => action.shift(next),")?;
                    f.line(
                        "Some(SimulatedAction::Reduce(symbol, n)) => action.reduce(symbol, n),",
                    )?;
                    f.line("Some(SimulatedAction::Accept) => action.accept(),")?;
                    f.line("None => action.fail(),")?;
                    Ok(())
                })
            })?;

            f.line("#[inline]")?;
            f.bracket(
                "fn goto(&self, current: Self::StateIndex, symbol: Self::NonterminalIndex) -> Self::StateIndex",
                |f| f.line("__goto(current, symbol).unwrap()"),
            )?;

            f.line("#[inline]")?;
            f.bracket(
                "fn expected_terminals(&self, current: Self::StateIndex) -> &[lelele::Terminal<Self::TerminalIndex>]",
                |f| f.line("__expected_terminals(current)"),
            )?;

            Ok(())
        })?;

        f.bracket("enum SimulatedAction", |f| {
            f.line("Shift(NodeID),")?;
            f.line("Reduce(Symbol, usize),")?;
            f.line("Accept,")?;
            Ok(())
        })?;

        f.line("#[allow(unreachable_patterns)]")?;
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
                                Action::Accept => f.write_str("Some(SimulatedAction::Accept)")?,
                                Action::Fail => f.write_str("None")?,

                                Action::Inconsistent { shift, reduces, .. } => {
                                    if let Some(n) = shift {
                                        // shift/reduce conflict(s)
                                        write!(f, "Some(SimulatedAction::Shift(NodeID::N{}))", n)?;
                                    } else {
                                        // reduce/reduce conflict(s)
                                        let rule = self.grammar.rule(&reduces[0]);
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
                                },
                            };
                            f.write_str(",\n")?;
                        }

                        f.line("_ => None,")?;

                        Ok(())
                    })?;

                }
                Ok(())
            })
        })?;

        f.bracket(
            "const fn __expected_terminals(current: NodeID) -> &'static [lelele::Terminal<TokenID>]",
            |f| {
                f.bracket("match current", |f| {
                    for (id, node) in self.dfa.nodes() {
                        writeln!(f, "NodeID::N{} => &[", id)?;
                        for (lookahead, _) in node.actions() {
                            let name = self.grammar.terminal(&lookahead).export_name();
                            match (lookahead, name) {
                                (TerminalID::EOI, _) => write!(f, "lelele::Terminal::EOI")?,
                                (_, Some(name)) => write!(f, "lelele::Terminal::T(TokenID::{})", name)?,
                                _ => continue,
                            }
                            f.line(",")?;
                        }
                        f.line("],")?;
                    }
                    Ok(())
                })
            },
        )?;

        f.line("#[allow(unreachable_patterns)]")?;
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

                        f.line("_ => None,")?;

                        Ok(())
                    })?;
                }

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
