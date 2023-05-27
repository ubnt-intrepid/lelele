//! Runtime parser definition.

use std::io;

use crate::{
    dfa::{NodeID, DFA},
    grammar::{Grammar, RuleID, SymbolID},
};
use indexmap::IndexMap;
use lelele_runtime::parser::ParserActionKind;

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
    table: IndexMap<NodeID, IndexMap<SymbolID, Action>>,
}

impl<'g> ParserDefinition<'g> {
    pub fn new(grammar: &'g Grammar<'g>) -> Self {
        let dfa = DFA::generate(&grammar);
        let table = gen_parse_table(&grammar, &dfa);
        Self { grammar, table }
    }

    pub fn generate<W: ?Sized>(&self, w: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        generate_parser(w, &self.grammar, &self.table)
    }
}

impl<'g> lelele_runtime::parser::ParserDefinition for ParserDefinition<'g> {
    type State = NodeID;
    type Symbol = SymbolID;
    type Reduce = RuleID;
    type Action = ParserAction<'g>;

    fn initial_state(&self) -> Self::State {
        *self.table.first().unwrap().0
    }

    fn action(&self, current: Self::State, input: Option<Self::Symbol>) -> Self::Action {
        let input = input.unwrap_or(SymbolID::EOI);
        ParserAction {
            grammar: self.grammar,
            action: self.table[&current][&input],
        }
    }
}

pub struct ParserAction<'g> {
    grammar: &'g Grammar<'g>,
    action: Action,
}
impl<'g> lelele_runtime::parser::ParserAction for ParserAction<'g> {
    type State = NodeID;
    type Symbol = SymbolID;
    type Reduce = RuleID;

    fn into_kind(self) -> ParserActionKind<Self> {
        match self.action {
            Action::Shift(n) | Action::Goto(n) => ParserActionKind::Shift(n),
            Action::Reduce(rule_id) => {
                let rule = self.grammar.rule(rule_id);
                let n = rule.rhs.len();
                ParserActionKind::Reduce(rule_id, rule.lhs, n)
            }
            Action::Accept => ParserActionKind::Accept,
        }
    }
}

fn generate_parser<W: ?Sized>(
    w: &mut W,
    grammar: &Grammar<'_>,
    table: &IndexMap<NodeID, IndexMap<SymbolID, Action>>,
) -> io::Result<()>
where
    W: io::Write,
{
    w.write_all(
        b"\
// auto-generated file

#[derive(Copy, Clone)]
enum Action {
    Shift(NodeID),
    Reduce(RuleID, SymbolID, usize),
    Accept,
}

const PARSE_TABLE: ::lelele_runtime::phf::Map<u64, ::lelele_runtime::phf::Map<u64, Action>> = ",
    )?;

    let mut table_g = phf_codegen::Map::<u64>::new();
    table_g.phf_path("::lelele_runtime::phf");
    for (node, actions) in table {
        let mut actions_g = phf_codegen::Map::<u64>::new();
        actions_g.phf_path("::lelele_runtime::phf");
        for (symbol, action) in actions {
            let action_g = match action {
                Action::Shift(n) | Action::Goto(n) => {
                    format!("Action::Shift(NodeID {{ __raw: {}_u64 }})", n.raw())
                }
                Action::Reduce(r) => {
                    let rule = grammar.rule(*r);
                    format!(
                        "Action::Reduce(RuleID {{ __raw: {}_u64 }}, SymbolID {{ __raw: {}_u64 }}, {}_usize)",
                        r.raw(),
                        rule.lhs.raw(),
                        rule.rhs.len()
                    )
                }
                Action::Accept => format!("Action::Accept"),
            };
            actions_g.entry(symbol.raw(), &action_g);
        }
        let actions_g = actions_g.build().to_string();
        table_g.entry(node.raw(), &actions_g);
    }
    let table_g = table_g.build();
    write!(w, "{}", table_g)?;

    w.write_all(
        b";

/// a
#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(transparent)]
pub struct NodeID { __raw: u64 }

///
#[derive(Debug, Copy, Clone, PartialEq)]
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
            "    pub const {}: Self = Self {{ __raw: {}_u64 }};",
            symbol.name(),
            id.raw()
        )?;
    }

    w.write_all(
        b"\
}

///
#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(transparent)]
pub struct RuleID { __raw: u64 }
impl RuleID {\n",
    )?;

    for (id, rule) in grammar.rules().filter(|(id, _)| *id != RuleID::START) {
        writeln!(
            w,
            "    pub const {}: Self = Self {{ __raw: {}_u64 }};",
            rule.name,
            id.raw()
        )?;
    }

    w.write_all(
        b"\
}

/// The parser definition
#[derive(Default)]
pub struct ParserDefinition {
    _p: (),
}
impl ::lelele_runtime::parser::ParserDefinition for ParserDefinition {
    type State = NodeID;
    type Symbol = SymbolID;
    type Reduce = RuleID;
    type Action = ParserAction;
    fn initial_state(&self) -> Self::State {\n",
    )?;

    writeln!(
        w,
        "        NodeID {{ __raw : {}_u64 }}",
        table.first().unwrap().0
    )?;

    w.write_all(
        b"\
    }
    fn action(&self, current: Self::State, input: Option<Self::Symbol>) -> Self::Action {\n",
    )?;

    writeln!(
        w,
        "let input = input.map_or({}_u64, |SymbolID {{ __raw: n }}| n);",
        SymbolID::EOI.raw()
    )?;

    w.write_all(
        b"\
        let actions = PARSE_TABLE.get(&(current.__raw)).unwrap();
        ParserAction {
            action: actions.get(&input).copied().unwrap(),
        }
    }
}

/// The parser action 
pub struct ParserAction {
    action: Action,
}
impl ::lelele_runtime::parser::ParserAction for ParserAction {
    type State = NodeID;
    type Symbol = SymbolID;
    type Reduce = RuleID;
    
    fn into_kind(self) -> ::lelele_runtime::parser::ParserActionKind<Self> {
        match self.action {
            Action::Shift(n) => ::lelele_runtime::parser::ParserActionKind::Shift(n),
            Action::Reduce(r, s, n) => ::lelele_runtime::parser::ParserActionKind::Reduce(r, s, n),
            Action::Accept => ::lelele_runtime::parser::ParserActionKind::Accept,
        }
    }
}

pub type Parser<TTok> = ::lelele_runtime::parser::Parser<ParserDefinition, TTok>;

pub fn parser<TTok>() -> Parser<TTok>
where
    TTok: ::lelele_runtime::parser::Token<SymbolID>,
{
    ::lelele_runtime::parser::Parser::new(
        ParserDefinition::default()
    )
}
",
    )?;
    Ok(())
}
