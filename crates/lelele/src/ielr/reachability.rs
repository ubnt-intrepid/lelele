use super::{
    lr0::StateID,
    table::{Action, ParseTable},
};
use crate::grammar::{Grammar, NonterminalID, SymbolID, TerminalID};
use std::{borrow::Cow, cmp::Ordering, collections::BinaryHeap};

// The fact that the LR state `start` is reachable to `end` on the specified lookahead symbol.
//
//  start --(alpha/words)->> end [lookahead]
//
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fact {
    start: StateID,
    end: StateID,
    symbols: Vec<SymbolID>,
    words: Vec<TerminalID>,
    lookahead: TerminalID,
}

impl Fact {
    fn first_word(&self) -> &TerminalID {
        self.words.first().unwrap_or(&self.lookahead)
    }

    fn is_subsumed_by(&self, fact: &Fact) -> bool {
        self.start == fact.start
            && self.end == fact.end
            && self.symbols == fact.symbols
            && self.lookahead == fact.lookahead
            && self.first_word() == fact.first_word()
            && self.words.len() <= fact.words.len()
    }
}

// The edge fact:
//  start --(A/words)->> end [lookahead]
#[derive(Debug, Clone)]
pub struct EdgeFact {
    start: StateID,
    end: StateID,
    symbol: NonterminalID,
    words: Vec<TerminalID>,
    lookahead: TerminalID,
}

impl EdgeFact {
    fn first_word(&self) -> &TerminalID {
        self.words.first().unwrap_or(&self.lookahead)
    }

    fn is_subsumed_by(&self, edge_fact: &EdgeFact) -> bool {
        self.start == edge_fact.start
            && self.end == edge_fact.end
            && self.symbol == edge_fact.symbol
            && self.lookahead == edge_fact.lookahead
            && self.first_word() == edge_fact.first_word()
            && self.words.len() <= edge_fact.words.len()
    }
}

#[derive(Debug)]
struct PendingFact(Fact);
impl PartialEq for PendingFact {
    fn eq(&self, other: &Self) -> bool {
        self.0.words.len() == other.0.words.len()
    }
}
impl Eq for PendingFact {}
impl PartialOrd for PendingFact {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for PendingFact {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.words.len().cmp(&other.0.words.len()).reverse()
    }
}

#[derive(Debug)]
pub struct ReachabilityAlgorithm<'g> {
    grammar: &'g Grammar,
    automaton: &'g ParseTable,
    pending_facts: BinaryHeap<PendingFact>,
    pub facts: Vec<Fact>,
    pub edge_facts: Vec<EdgeFact>,
}

impl<'g> ReachabilityAlgorithm<'g> {
    pub fn new(grammar: &'g Grammar, automaton: &'g ParseTable) -> Self {
        Self {
            grammar,
            automaton,
            pending_facts: BinaryHeap::new(),
            facts: vec![],
            edge_facts: vec![],
        }
    }

    #[tracing::instrument(skip_all)]
    pub fn process(&mut self) {
        // add fact: s -(ε/ε)->> s [z] for all s,z
        for (&node_id, _node) in &self.automaton.states {
            for &lookahead in self.grammar.terminals.keys() {
                self.pending_facts.push(PendingFact(Fact {
                    start: node_id,
                    end: node_id,
                    symbols: vec![],
                    words: vec![],
                    lookahead,
                }));
            }
        }

        //
        let max_facts = self.automaton.states.len()
            * self
                .grammar
                .rules
                .values()
                .fold(0usize, |acc, r| acc + r.right().len())
            * self.grammar.terminals.len()
            * self.grammar.terminals.len();
        tracing::trace!("maximum number of facts = {}", max_facts);

        let max_edge_facts = self
            .automaton
            .states
            .values()
            .fold(0usize, |acc, node| acc + node.gotos.len())
            * self.grammar.terminals.len()
            * self.grammar.terminals.len();
        tracing::trace!("maximum number of edge facts = {}", max_edge_facts);

        while let Some(PendingFact(fact)) = self.pending_facts.pop() {
            if self.facts.len() > max_facts {
                tracing::trace!("reached to maximum facts");
                break;
            }
            if self.edge_facts.len() > max_edge_facts {
                tracing::trace!("reached to maximum edge facts");
                break;
            }

            if self.facts.iter().all(|f| !f.is_subsumed_by(&fact)) {
                self.facts.push(fact.clone());
                self.step_terminal(&fact);
                self.step_nonterminal_left(&fact);
                self.step_reduce(&fact);
            }
        }
    }

    #[tracing::instrument(skip_all)]
    fn step_terminal(&mut self, fact: &Fact) {
        let node = &self.automaton.states[&fact.end];
        let found = node.actions.iter().find_map(|(&t, action)| match action {
            Action::Shift(n) | Action::Inconsistent { shift: Some(n), .. }
                if t == fact.lookahead =>
            {
                Some(*n)
            }
            _ => None,
        });
        if let Some(next) = found {
            for &terminal in self.grammar.terminals.keys() {
                let mut new_fact = fact.clone();
                new_fact.end = next;
                new_fact.symbols.push(SymbolID::T(fact.lookahead));
                new_fact.words.push(fact.lookahead);
                new_fact.lookahead = terminal;

                self.pending_facts.push(PendingFact(new_fact));
            }
        }
    }

    #[tracing::instrument(skip_all)]
    fn step_nonterminal_left(&mut self, fact: &Fact) {
        let node = &self.automaton.states[&fact.end];
        for (&symbol, &dest) in &node.gotos {
            for edge_fact in &self.edge_facts {
                if fact.end != edge_fact.start || edge_fact.symbol != symbol {
                    continue;
                }
                if fact.lookahead != *edge_fact.first_word() {
                    continue;
                }

                let mut new_fact = fact.clone();
                new_fact.end = dest;
                new_fact.symbols.push(SymbolID::N(symbol));
                new_fact.words.extend_from_slice(&edge_fact.words[..]);
                new_fact.lookahead = edge_fact.lookahead;

                self.pending_facts.push(PendingFact(new_fact));
            }
        }
    }

    #[tracing::instrument(skip_all)]
    fn step_nonterminal_right(&mut self, edge_fact: &EdgeFact) {
        for fact in &self.facts {
            if fact.end != edge_fact.start || fact.lookahead != *edge_fact.first_word() {
                continue;
            }
            let mut new_fact = Fact {
                start: fact.start,
                end: edge_fact.end,
                symbols: fact.symbols.clone(),
                words: fact.words.clone(),
                lookahead: edge_fact.lookahead,
            };
            new_fact.symbols.push(SymbolID::N(edge_fact.symbol));
            new_fact.words.extend_from_slice(&edge_fact.words[..]);

            self.pending_facts.push(PendingFact(new_fact));
        }
    }

    #[tracing::instrument(skip_all)]
    fn step_reduce(&mut self, fact: &Fact) {
        let node_start = &self.automaton.states[&fact.start];
        let node_end = &self.automaton.states[&fact.end];
        for (&t, action) in &node_end.actions {
            let mut reduce_ids = Cow::Borrowed(&[] as &[_]);
            match action {
                Action::Reduce(reduce) => {
                    reduce_ids = Cow::Owned(vec![*reduce]);
                }
                Action::Inconsistent { reduces, .. } => {
                    reduce_ids = Cow::Borrowed(&reduces[..]);
                }
                _ => (),
            }

            for reduce in &*reduce_ids {
                let reduce = &self.grammar.rules[reduce];
                if reduce.right() != fact.symbols || t != fact.lookahead {
                    continue;
                }

                let Some((_, &new_end)) = node_start.gotos.iter().find(|(n, _)| **n == reduce.left()) else { continue; };

                let edge_fact = EdgeFact {
                    start: fact.start,
                    end: new_end,
                    symbol: reduce.left(),
                    words: fact.words.clone(),
                    lookahead: fact.lookahead,
                };

                if self
                    .edge_facts
                    .iter()
                    .all(|e| !e.is_subsumed_by(&edge_fact))
                {
                    self.edge_facts.push(edge_fact.clone());
                    self.step_nonterminal_right(&edge_fact);
                }
            }
        }
    }
}
