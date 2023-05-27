//! Runtime parser definition.

use std::io;

use crate::{
    dfa::{NodeID, DFA},
    grammar::{Grammar, RuleID, SymbolID},
};
use indexmap::{IndexMap, IndexSet};
use lelele_runtime::parser::ParserAction;

#[derive(Debug, Copy, Clone)]
pub(crate) enum Action {
    Shift(NodeID),
    Goto(NodeID),
    Reduce(RuleID),
    Accept,
}

fn gen_parse_table(
    grammar: &Grammar<'_>,
    dfa: &DFA,
) -> IndexMap<NodeID, IndexMap<SymbolID, Action>> {
    let mut transition_table: IndexMap<NodeID, IndexMap<SymbolID, Action>> = IndexMap::new();
    for (id, node) in dfa.nodes() {
        let mut actions = IndexMap::new();
        // shift, goto
        for (label, target) in &node.edges {
            actions.insert(
                *label,
                if grammar.symbol(*label).is_terminal() {
                    Action::Shift(*target)
                } else {
                    Action::Goto(*target)
                },
            );
        }

        // reduce, accept
        for item in &node.item_set {
            let rule = &grammar.rule(item.rule_id);
            if item.marker < rule.rhs.len() {
                continue;
            }

            if item.rule_id == RuleID::START {
                actions.insert(SymbolID::EOI, Action::Accept);
            } else {
                actions.insert(item.lookahead, Action::Reduce(item.rule_id));
            }
        }

        transition_table.insert(id, actions);
    }
    transition_table
}

#[derive(Debug)]
pub struct ParserDefinition<'g> {
    grammar: &'g Grammar<'g>,
    start_node: NodeID,
    table: IndexMap<NodeID, IndexMap<SymbolID, Action>>,
}

impl<'g> ParserDefinition<'g> {
    pub fn new(grammar: &'g Grammar<'g>) -> Self {
        let dfa = DFA::generate(&grammar);
        let start_node = dfa.start_node().0;
        let table = gen_parse_table(&grammar, &dfa);
        Self {
            grammar,
            start_node,
            table,
        }
    }

    pub fn generate<W: ?Sized>(&self, w: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        generate_parser(w, &self.grammar, self.start_node, &self.table)
    }
}

impl<'g> lelele_runtime::parser::ParserDefinition for ParserDefinition<'g> {
    type State = NodeID;
    type Symbol = SymbolID;
    type Reduce = RuleID;
    type Error = std::convert::Infallible; // FIXME: appropriate error type

    fn initial_state(&self) -> Self::State {
        self.start_node
    }

    fn action(
        &self,
        current: Self::State,
        input: Option<Self::Symbol>,
    ) -> Result<ParserAction<Self::State, Self::Symbol, Self::Reduce>, Self::Error> {
        let input = input.unwrap_or(SymbolID::EOI);
        match self.table[&current][&input] {
            Action::Shift(n) | Action::Goto(n) => Ok(ParserAction::Shift(n)),
            Action::Reduce(rule_id) => {
                let rule = self.grammar.rule(rule_id);
                let n = rule.rhs.len();
                Ok(ParserAction::Reduce(rule_id, rule.lhs, n))
            }
            Action::Accept => Ok(ParserAction::Accept),
        }
    }
}

fn generate_parser<W: ?Sized>(
    w: &mut W,
    grammar: &Grammar<'_>,
    start_node: NodeID,
    table: &IndexMap<NodeID, IndexMap<SymbolID, Action>>,
) -> io::Result<()>
where
    W: io::Write,
{
    // 使用されている NodeID, RuleID, SymbolID を集計し、コード生成用に並び換える
    let node_ids: IndexSet<NodeID> = table.keys().copied().collect();
    let mut symbol_ids: IndexSet<SymbolID> = IndexSet::new();
    symbol_ids.insert(SymbolID::EOI);
    symbol_ids.insert(SymbolID::START);
    let mut rule_ids: IndexSet<RuleID> = IndexSet::new();
    rule_ids.insert(RuleID::START);
    for (symbol, action) in table.values().flatten() {
        symbol_ids.insert(*symbol);
        if let Action::Reduce(r) = action {
            let rule = grammar.rule(*r);
            symbol_ids.insert(rule.lhs);
            symbol_ids.extend(rule.rhs.iter().copied());
            rule_ids.insert(*r);
        }
    }
    let node_id_of = |n: &NodeID| node_ids.get_index_of(n).unwrap();
    let symbol_id_of = |s: &SymbolID| symbol_ids.get_index_of(s).unwrap();
    let rule_id_of = |r: &RuleID| rule_ids.get_index_of(r).unwrap();

    w.write_all(
        b"\
// This file is automatically generated by lelele.

use ::lelele_runtime::_private as lelele;

const PARSE_TABLE: &[
    lelele::phf::Map<
        u64,
        lelele::ParserAction<
            NodeID,
            SymbolID,
            RuleID,
        >,
    >
] = &[\n",
    )?;

    for actions in table.values() {
        let mut actions_g = phf_codegen::Map::<u64>::new();
        actions_g.phf_path("lelele::phf");
        for (symbol, action) in actions {
            let action_g = match action {
                Action::Shift(n) | Action::Goto(n) => {
                    format!(
                        "lelele::ParserAction::Shift(NodeID {{ __raw: {}_usize }})",
                        node_id_of(n)
                    )
                }
                Action::Reduce(r) => {
                    let rule = grammar.rule(*r);
                    format!(
                        "lelele::ParserAction::Reduce(RuleID {{ __raw: {}_u64 }}, SymbolID {{ __raw: {}_u64 }}, {}_usize)",
                        rule_id_of(r),
                        symbol_id_of(&rule.lhs),
                        rule.rhs.len()
                    )
                }
                Action::Accept => format!("lelele::ParserAction::Accept"),
            };
            actions_g.entry(symbol_id_of(&symbol) as u64, &action_g);
        }
        writeln!(w, "{},", actions_g.build())?;
    }

    w.write_all(
        b"];

/// The type to identify DFA state nodes.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct NodeID { __raw: usize }
impl NodeID {\n",
    )?;

    writeln!(
        w,
        "    const __START: Self = Self {{ __raw: {}_usize }};",
        node_id_of(&start_node)
    )?;

    w.write_all(
        b"\
}

/// The type to identify terminal or nonterminal symbols used in generated DFA.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct SymbolID { __raw: u64 }
impl SymbolID {\n",
    )?;

    for (id, symbol) in grammar
        .symbols()
        .filter(|(id, _)| *id != SymbolID::EOI && *id != SymbolID::START)
    {
        writeln!(
            w,
            "    \
    /// `\"{name}\"`
    pub const {name}: Self = Self {{ __raw: {id}_u64 }};",
            name = symbol.name(),
            id = symbol_id_of(&id)
        )?;
    }
    writeln!(
        w,
        "    const __EOI: Self = Self {{ __raw: {}_u64 }};",
        symbol_id_of(&SymbolID::EOI)
    )?;

    w.write_all(
        b"\
}

/// The type to identify the syntax rule that matched input sequence.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct RuleID { __raw: u64 }
impl RuleID {\n",
    )?;

    for (id, rule) in grammar.rules().filter(|(id, _)| *id != RuleID::START) {
        writeln!(
            w,
            "    \
    /// `\"{name}\"`
    pub const {name}: Self = Self {{ __raw: {id}_u64 }};",
            name = rule.name,
            id = rule_id_of(&id)
        )?;
    }

    w.write_all(
        b"\
}

/// The parser definition.
#[derive(Default)]
pub struct ParserDefinition {
    _p: (),
}
impl lelele::ParserDefinition for ParserDefinition {
    type State = NodeID;
    type Symbol = SymbolID;
    type Reduce = RuleID;
    type Error = ::std::convert::Infallible; // FIXME: should not be infallible.

    #[inline]
    fn initial_state(&self) -> Self::State {
        NodeID::__START
    }

    #[inline]
    fn action(
        &self,
        current: Self::State,
        input: Option<Self::Symbol>,
    ) -> Result<lelele::ParserAction<Self::State, Self::Symbol, Self::Reduce>, Self::Error> {
        let input = input.unwrap_or(SymbolID::__EOI).__raw;
        Ok(PARSE_TABLE[current.__raw][&input])
    }
}

/// The alias to parser type using generated parser definition.
pub type Parser<TTok> = lelele::Parser<ParserDefinition, TTok>;

/// Create an instance of parser using generated definition.
pub fn parser<TTok>() -> Parser<TTok>
where
    TTok: lelele::Token<SymbolID>,
{
    lelele::Parser::new(
        ParserDefinition::default()
    )
}
",
    )?;
    Ok(())
}
