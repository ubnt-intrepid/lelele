//! Runtime parser definition.

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
