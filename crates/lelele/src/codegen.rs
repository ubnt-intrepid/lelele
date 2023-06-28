//! code generation.

use crate::{
    dfa::{Action, NodeID, DFA},
    grammar::{Grammar, TerminalID},
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
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct NodeID { __raw: usize }
impl NodeID {\n",
        )?;

        writeln!(
            f,
            "    const __START: Self = Self {{ __raw: {} }};",
            NodeID::START
        )?;

        f.write_str("}\n")?;

        Ok(())
    }

    fn fmt_token_id_def(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            "\
/// The type to identify terminal or nonterminal symbols used in generated DFA.
#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct TokenID { __raw: u64 }
impl TokenID {\n",
        )?;

        writeln!(
            f,
            "const __EOI: Self = Self {{ __raw: {} }};",
            TerminalID::EOI.raw(),
        )?;

        for terminal in self.grammar.terminals() {
            let export_name = match terminal.export_name() {
                Some(name) => name,
                None => continue,
            };
            writeln!(
                f,
                "\
/// Terminal `{export_name}`
pub const {export_name}: Self = Self {{ __raw: {id} }};",
                export_name = export_name,
                id = terminal.id().raw(),
            )?;
        }

        f.write_str(
            "}
impl ::std::fmt::Debug for TokenID {
    #[inline]
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(self, f)
    }
}
impl ::std::fmt::Display for TokenID {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        match self.__raw {\n",
        )?;

        for symbol in self.grammar.terminals() {
            writeln!(
                f,
                "\
            {} => f.write_str(stringify!({})),",
                symbol.id().raw(),
                symbol,
            )?;
        }

        f.write_str(
            "\
            _ => f.write_str(\"<unknown>\"),
        }
    }
}",
        )?;

        Ok(())
    }

    fn fmt_symbol_id_def(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            "\
/// The type to identify nonterminal symbols used in generated DFA.
#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct SymbolID { __raw: u64 }
impl SymbolID {\n",
        )?;

        for symbol in self.grammar.nonterminals() {
            let export_name = match symbol.export_name() {
                Some(name) => name,
                None => continue,
            };
            writeln!(
                f,
                "\
/// Nonterminal `{export_name}`
pub const {export_name}: Self = Self {{ __raw: {id} }};",
                export_name = export_name,
                id = symbol.id().raw(),
            )?;
        }

        f.write_str(
            "}
impl ::std::fmt::Debug for SymbolID {
    #[inline]
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(self, f)
    }
}
impl ::std::fmt::Display for SymbolID {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        match self.__raw {\n",
        )?;

        for symbol in self.grammar.nonterminals() {
            writeln!(
                f,
                "\
            {} => f.write_str(stringify!({})),",
                symbol.id().raw(),
                symbol,
            )?;
        }

        f.write_str(
            "\
            _ => f.write_str(\"<unknown>\"),
        }
    }
}",
        )?;

        Ok(())
    }

    fn fmt_parser_def(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            "\
#[allow(dead_code)]
enum ParseAction {
    Shift(NodeID),
    Reduce(SymbolID, usize),
    Accept,
    Fail,
}
/// The generated LR(1) parse table.
#[derive(Debug, Default)]
pub struct ParserDef {
    _p: (),
}
impl lelele::ParserDef for ParserDef {
    type State = NodeID;
    type Token = TokenID;
    type Symbol = SymbolID;
    #[inline]
    fn initial_state(&self) -> Self::State {
        NodeID::__START
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
        let lookahead = lookahead.unwrap_or(TokenID::__EOI).__raw;
        let actions = &PARSE_TABLE[current.__raw];
        match actions.get(&lookahead) {
            Some(ParseAction::Shift(n)) => action.shift(*n),
            Some(ParseAction::Reduce(s, i)) => action.reduce(*s, *i),
            Some(ParseAction::Accept) => action.accept(),
            _ => action.fail(actions.keys().map(|key| TokenID { __raw: *key })),
        }
    }
    #[inline]
    fn goto(&self, current: Self::State, symbol: Self::Symbol) -> Self::State {
        *GOTO_TABLE[current.__raw].get(&symbol.__raw).unwrap()
    }
}
const PARSE_TABLE: &[ lelele::phf::Map<u64, ParseAction> ] = &[\n",
        )?;

        for (_id, node) in self.dfa.nodes() {
            let mut actions_g = phf_codegen::Map::<u64>::new();
            actions_g.phf_path("lelele::phf");

            for (symbol, action) in node.actions() {
                let action_g = match action {
                    Action::Shift(n) => {
                        format!("ParseAction::Shift(NodeID {{ __raw: {} }})", n)
                    }
                    Action::Reduce(rule) => {
                        let rule = self.grammar.rule(rule);
                        format!(
                            "ParseAction::Reduce(SymbolID {{ __raw: {} }}, {})",
                            rule.left().raw(),
                            rule.right().len(),
                        )
                    }
                    Action::Accept => "ParseAction::Accept".into(),
                    Action::Fail => "ParseAction::Fail".into(),
                    Action::Inconsistent { .. } => "ParseAction::Fail".into(),
                };
                actions_g.entry(symbol.raw(), &action_g);
            }

            writeln!(f, "{},", actions_g.build())?;
        }

        f.write_str(
            "];\n
const GOTO_TABLE: &[ lelele::phf::Map<u64, NodeID> ] = &[\n",
        )?;

        for (_, node) in self.dfa.nodes() {
            let mut actions_g = phf_codegen::Map::<u64>::new();
            actions_g.phf_path("lelele::phf");

            for (symbol, target) in node.gotos() {
                actions_g.entry(symbol.raw(), &format!("NodeID {{ __raw: {} }}", target));
            }

            writeln!(f, "{},", actions_g.build())?;
        }

        f.write_str("];\n")?;

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
