//! code generation.

use crate::{
    dfa::{Action, NodeID, DFA},
    grammar::{Grammar, RuleID, SymbolID},
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
        const PREAMBLE: &str = "\
// This file is automatically generated by lelele.

use ::lelele_runtime::_private as lelele;

/// The alias to parser type using generated parser definition.
pub type Parser<TTok> = lelele::Parser<ParseTable, TTok>;

/// Create an instance of parser using generated definition.
pub fn parser<TTok>() -> Parser<TTok>
where
    TTok: lelele::Token<SymbolID>,
{
    lelele::Parser::new(
        ParseTable::default()
    )
}

/// The generated LR(1) parse table.
#[derive(Default)]
pub struct ParseTable {
    _p: (),
}

impl lelele::ParseTable for ParseTable {
    type State = NodeID;
    type Symbol = SymbolID;
    type Reduce = RuleID;

    #[inline]
    fn initial_state(&self) -> Self::State {
        NodeID::__START
    }

    #[inline]
    fn action(
        &self,
        current: Self::State,
        lookahead: Option<Self::Symbol>,
    ) -> lelele::ParseAction<Self::State, Self::Symbol, Self::Reduce> {
        match PARSE_TABLE.get(current.__raw) {
            Some(actions) => {
                let lookahead = lookahead.unwrap_or(SymbolID::__EOI).__raw;
                actions.get(&lookahead)
                    .copied()
                    .unwrap_or_else(|| {
                        lelele::ParseAction::Error(
                            lelele::ParseActionError::IncorrectSymbol
                        )
                    })
            },
            None => lelele::ParseAction::Error(lelele::ParseActionError::IncorrectState),
        }
    }
}
";
        f.write_str(PREAMBLE)
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

    fn fmt_symbol_id_def(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            "\
/// The type to identify terminal or nonterminal symbols used in generated DFA.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct SymbolID { __raw: u64 }
impl SymbolID {\n",
        )?;

        writeln!(
            f,
            "const __EOI: Self = Self {{ __raw: {} }};",
            SymbolID::EOI.raw(),
        )?;

        for (id, symbol) in self
            .grammar
            .symbols()
            .filter(|(id, _)| *id != SymbolID::EOI && *id != SymbolID::ACCEPT)
        {
            writeln!(
                f,
                "\
/// `{name}`
pub const {name}: Self = Self {{ __raw: {id} }};",
                name = symbol.name(),
                id = id.raw(),
            )?;
        }

        f.write_str("}\n")?;

        Ok(())
    }

    fn fmt_rule_id_def(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            "\
/// The type to identify the syntax rule that matched input sequence.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct RuleID { __raw: u64 }
impl RuleID {\n",
        )?;

        for (rule_id, rule) in self.grammar.rules().filter(|(id, _)| *id != RuleID::ACCEPT) {
            let comment_lhs = self.grammar.symbol(rule.left()).name();
            let comment_rhs =
                rule.right()
                    .iter()
                    .enumerate()
                    .fold(String::new(), |mut acc, (i, s)| {
                        if i > 0 {
                            acc += " ";
                        }
                        acc += self.grammar.symbol(*s).name();
                        acc
                    });
            writeln!(f, "    /// `{} : {}`", comment_lhs, comment_rhs)?;
            writeln!(
                f,
                "    pub const {export}: Self = Self {{ __raw: {id} }};",
                export = rule.name(),
                id = rule_id.raw()
            )?;
        }

        f.write_str("}\n")?;

        Ok(())
    }

    fn fmt_parse_table(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            "\
const PARSE_TABLE: &[
    lelele::phf::Map<
        u64,
        lelele::ParseAction<
            NodeID,
            SymbolID,
            RuleID,
        >,
    >
] = &[\n",
        )?;

        for (_, node) in self.dfa.nodes() {
            let actions = node.parse_actions(&self.grammar);

            let mut actions_g = phf_codegen::Map::<u64>::new();
            actions_g.phf_path("lelele::phf");
            for (symbol, action) in actions {
                let action_g = match action {
                    Action::Shift(n) => {
                        format!("lelele::ParseAction::Shift(NodeID {{ __raw: {} }})", n)
                    }
                    Action::Reduce(r) => {
                        let rule = self.grammar.rule(r);
                        format!(
                        "lelele::ParseAction::Reduce(RuleID {{ __raw: {} }}, SymbolID {{ __raw: {} }}, {})",
                        r.raw(),
                        rule.left().raw(),
                        rule.right().len()
                    )
                    }
                    Action::Accept => format!("lelele::ParseAction::Accept"),
                };
                actions_g.entry(symbol.raw(), &action_g);
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
        self.fmt_symbol_id_def(f)?;
        self.fmt_rule_id_def(f)?;
        self.fmt_parse_table(f)?;
        Ok(())
    }
}
