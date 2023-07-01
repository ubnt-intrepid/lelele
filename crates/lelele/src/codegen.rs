//! code generation.

use crate::{
    dfa::{Action, NodeID, DFA},
    grammar::{Grammar, RuleID},
};
use std::fmt;

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
        f.write_str(
            "\
// This file is automatically generated by lelele.
use ::lelele_runtime::_private as lelele;
",
        )
    }

    fn fmt_node_id_def(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            "\
/// The type to identify DFA state nodes.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum NodeID {\n",
        )?;
        for (id, _) in self.dfa.nodes() {
            writeln!(f, "N{},", id)?;
        }
        f.write_str("}\n")?;

        writeln!(
            f,
            "const __START_NODE: NodeID = NodeID::N{};",
            NodeID::START
        )?;

        Ok(())
    }

    fn fmt_token_id_def(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            "\
/// The type to identify terminal or nonterminal symbols used in generated DFA.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub enum TokenID {\n",
        )?;

        for terminal in self.grammar.terminals() {
            let export_name = match terminal.export_name() {
                Some(name) => name,
                None => continue,
            };
            writeln!(
                f,
                "#[doc = \"Terminal `{export_name}`.\"] {export_name},",
                export_name = export_name,
            )?;
        }

        f.write_str("}\n")?;

        Ok(())
    }

    fn fmt_symbol_id_def(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            "\
/// The type to identify nonterminal symbols used in generated DFA.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {\n",
        )?;

        for symbol in self.grammar.nonterminals() {
            let export_name = match symbol.export_name() {
                Some(name) => name,
                None => continue,
            };
            writeln!(
                f,
                "#[doc = \"Nonterminal `{export_name}`.\"] {export_name},",
                export_name = export_name,
            )?;
        }

        f.write_str("}\n")?;

        Ok(())
    }

    fn fmt_parser_def(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            "\
/// The generated LR(1) parse table.
#[derive(Debug, Default)]
pub struct ParserDef {
    _p: (),
}
impl lelele::ParserDef for ParserDef {
    type State = NodeID;
    type Token = TokenID;
    type Symbol = Symbol;
    #[inline]
    fn initial_state(&self) -> Self::State {
        __START_NODE
    }
    #[inline]
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
    {
        match __action(current, lookahead) {
            Some(SimulatedAction::Shift(next)) => action.shift(next),
            Some(SimulatedAction::Reduce(symbol, n)) => action.reduce(symbol, n),
            Some(SimulatedAction::Accept) => action.accept(),
            None => action.fail(__expected_tokens(current).into_iter().copied()),
        }
    }
    #[inline]
    fn goto(&self, current: Self::State, symbol: Self::Symbol) -> Self::State {
        __goto(current, symbol).unwrap()
    }
}\n",
        )?;

        f.write_str(
            "\
enum SimulatedAction {
    Shift(NodeID),
    Reduce(Symbol, usize),
    Accept,
}
const fn __action(current: NodeID, lookahead: Option<TokenID>) -> Option<SimulatedAction> {
    match current {\n",
        )?;

        for (id, node) in self.dfa.nodes() {
            writeln!(f, "NodeID::N{} => match lookahead {{", id)?;
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

            for lookahead in self
                .grammar
                .terminals()
                .filter(|t| !node.actions.contains_key(&t.id()))
            {
                match lookahead.export_name() {
                    Some(name) => writeln!(f, "Some(TokenID::{}) => None,", name)?,
                    None => writeln!(f, "None => None,")?,
                }
            }

            f.write_str("},\n")?;
        }

        f.write_str("}\n}\n")?;
        f.write_str(
            "\
const fn __expected_tokens(current: NodeID) -> &'static [TokenID] {
match current {\n",
        )?;

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
        f.write_str("}\n}\n")?;

        f.write_str(
            "\
const fn __goto(current: NodeID, symbol: Symbol) -> Option<NodeID> {
match current {\n",
        )?;

        let mut unused_nodes = vec![];
        for (id, node) in self.dfa.nodes() {
            if node.gotos().count() == 0 {
                unused_nodes.push(id);
                continue;
            }

            writeln!(f, "NodeID::N{} => match symbol {{", id)?;
            for (symbol, target) in node.gotos() {
                let symbol = self.grammar.nonterminal(&symbol).export_name().unwrap();
                writeln!(f, "Symbol::{} => Some(NodeID::N{}),", symbol, target)?;
            }

            for symbol in self
                .grammar
                .nonterminals()
                .filter(|n| !node.gotos.contains_key(&n.id()))
            {
                if let Some(symbol) = symbol.export_name() {
                    writeln!(f, "Symbol::{} => None,", symbol)?;
                }
            }

            f.write_str("},\n")?;
        }

        for id in &unused_nodes {
            writeln!(f, "NodeID::N{} => None,", id)?;
        }

        f.write_str("}\n}\n")?;

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
