//! Parser.

use crate::{
    dfa::{Action, NodeID, DFA},
    grammar::{Grammar, SymbolID},
};
use indexmap::IndexMap;

pub trait TokenStream<T>: Iterator<Item = (SymbolID, T)> {
    fn peek_next(&mut self) -> Option<&(SymbolID, T)>;
}

impl<I, T> TokenStream<T> for std::iter::Peekable<I>
where
    I: Iterator<Item = (SymbolID, T)>,
{
    #[inline]
    fn peek_next(&mut self) -> Option<&(SymbolID, T)> {
        self.peek()
    }
}

#[derive(Debug)]
pub struct Parser<'g, T, R> {
    grammar: &'g Grammar<'g, R>,
    table: IndexMap<NodeID, IndexMap<SymbolID, Action>>,
    nodes: Vec<NodeID>,
    item_stack: Vec<StackItem<T>>,
    state: ParserState,
}

#[derive(Debug)]
enum ParserState {
    Reading,
    PendingGoto,
    Accepted,
}

impl<'g, T, R> Parser<'g, T, R> {
    pub fn new(grammar: &'g Grammar<R>) -> Self {
        let dfa = DFA::generate(&grammar);
        let table = dfa.transition_table();
        let initial_node = *table.first().unwrap().0;
        Self {
            grammar,
            table,
            nodes: vec![initial_node],
            item_stack: vec![],
            state: ParserState::Reading,
        }
    }

    pub fn next_event<I>(&mut self, tokens: &mut I) -> ParseEvent<'_, T, R>
    where
        I: TokenStream<T>,
    {
        loop {
            let current = self.nodes.last().unwrap();
            let input = match self.state {
                ParserState::PendingGoto => match self.item_stack.last().unwrap() {
                    StackItem::N(t) => *t,
                    StackItem::T(t, _) => *t,
                },
                _ => tokens.peek_next().map_or(SymbolID::EOI, |(id, _)| *id),
            };
            match self.table[current][&input] {
                Action::Shift(n) => {
                    self.state = ParserState::Reading;
                    let (id, t) = tokens.next().expect("unexpected EOI");
                    self.nodes.push(n);
                    self.item_stack.push(StackItem::T(id, t));
                    continue;
                }

                Action::Goto(n) => {
                    self.state = ParserState::Reading;
                    self.nodes.push(n);
                    continue;
                }

                Action::Reduce(rule_id) => {
                    let rule = self.grammar.rule(rule_id);
                    let ctx = rule.context.as_ref().unwrap();
                    let n = rule.rhs.len();
                    let mut args = vec![];
                    for _ in 0..n {
                        self.nodes.pop();
                        let item = self.item_stack.pop().unwrap();
                        let arg = match item {
                            StackItem::N(t) => ParseItem::N(self.grammar.symbol(t).name()),
                            StackItem::T(_, token) => ParseItem::T(token),
                        };
                        args.push(arg);
                    }
                    args.reverse();
                    self.state = ParserState::PendingGoto;
                    self.item_stack.push(StackItem::N(rule.lhs));
                    return ParseEvent::Reduce(ctx, args);
                }
                Action::Accept => {
                    self.state = ParserState::Accepted;
                    let item = self.item_stack.pop().expect("empty result stack");
                    let arg = match item {
                        StackItem::N(t) => ParseItem::N(self.grammar.symbol(t).name()),
                        StackItem::T(_, token) => ParseItem::T(token),
                    };
                    return ParseEvent::Accept(arg);
                }
            }
        }
    }
}

#[derive(Debug)]
enum StackItem<T> {
    T(SymbolID, T),
    N(SymbolID),
}

#[derive(Debug)]
pub enum ParseItem<'p, T> {
    T(T),
    N(&'p str),
}

#[derive(Debug)]
pub enum ParseEvent<'p, T, R> {
    Reduce(&'p R, Vec<ParseItem<'p, T>>),
    Accept(ParseItem<'p, T>),
}
