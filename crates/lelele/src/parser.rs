//! Runtime parser definition.

use crate::{
    dfa::{Action, NodeID, DFA},
    grammar::{Grammar, SymbolID},
};
use indexmap::IndexMap;
use lelele_runtime::ParserActionKind;

#[derive(Debug)]
pub struct ParserDefinition<'g, R> {
    grammar: &'g Grammar<'g, R>,
    table: IndexMap<NodeID, IndexMap<SymbolID, Action>>,
}

impl<'g, R> ParserDefinition<'g, R> {
    pub fn new(grammar: &'g Grammar<R>) -> Self {
        let dfa = DFA::generate(&grammar);
        let table = dfa.transition_table();
        Self { grammar, table }
    }
}

impl<'g, R> lelele_runtime::ParserDefinition for ParserDefinition<'g, R> {
    type State = NodeID;
    type Symbol = SymbolID;
    type Reduce = &'g R;
    type Action = ParserAction<'g, R>;

    fn initial_state(&self) -> Self::State {
        *self.table.first().unwrap().0
    }

    fn action(&self, current: Self::State, input: Option<Self::Symbol>) -> Self::Action {
        let input = input.unwrap_or(SymbolID::EOI);
        match self.table[&current][&input] {
            Action::Shift(n) | Action::Goto(n) => ParserAction::Shift(n),
            Action::Reduce(rule_id) => {
                let rule = self.grammar.rule(rule_id);
                let context = rule.context.as_ref().unwrap();
                let n = rule.rhs.len();
                ParserAction::Reduce {
                    context,
                    lhs: rule.lhs,
                    n,
                }
            }
            Action::Accept => ParserAction::Accept,
        }
    }
}

pub enum ParserAction<'g, R> {
    Shift(NodeID),
    Reduce {
        context: &'g R,
        lhs: SymbolID,
        n: usize,
    },
    Accept,
}
impl<'g, R> lelele_runtime::ParserAction for ParserAction<'g, R> {
    type State = NodeID;
    type Symbol = SymbolID;
    type Reduce = &'g R;

    fn into_kind(self) -> ParserActionKind<Self> {
        match self {
            Self::Shift(n) => ParserActionKind::Shift(n),
            Self::Reduce { context, lhs, n } => ParserActionKind::Reduce(context, lhs, n),
            Self::Accept => ParserActionKind::Accept,
        }
    }
}
