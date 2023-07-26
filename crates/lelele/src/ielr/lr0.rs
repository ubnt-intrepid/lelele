use crate::{
    grammar::{Grammar, NonterminalID, RuleID, SymbolID, TerminalID},
    types::{Map, Set},
};
use std::{collections::VecDeque, fmt};

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StateID(u16);
impl fmt::Debug for StateID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "S#{:03}", self.0)
    }
}
impl StateID {
    pub const START: Self = Self::from_raw(0);
    pub(crate) const OFFSET: u16 = 1;

    pub const fn from_raw(raw: u16) -> Self {
        Self(raw)
    }
    pub const fn into_raw(self) -> u16 {
        self.0
    }
}

/// The LR(0) item, a.k.a. LR item core.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LR0Item {
    pub production: RuleID,
    pub index: u16,
}

#[derive(Debug, Clone)]
pub struct LR0State {
    pub kernels: Vec<LR0Item>,
    pub shifts: Map<TerminalID, StateID>,
    pub gotos: Map<NonterminalID, StateID>,
    pub reduces: Set<RuleID>,
}

#[derive(Debug)]
pub struct LR0Automaton {
    pub states: Map<StateID, LR0State>,
}

impl LR0Automaton {
    pub fn predecessors(&self) -> Map<StateID, Set<StateID>> {
        let mut predecessors = Map::<StateID, Set<StateID>>::default();
        for (&current, state) in &self.states {
            for &next in state.shifts.values().chain(state.gotos.values()) {
                predecessors.entry(next).or_default().insert(current);
            }
        }
        predecessors
    }
}

/// Calculate the LR(0) automaton based on the specified grammar.
pub fn lr0(g: &Grammar) -> LR0Automaton {
    let nonkernels = nonkernels(&g);

    let mut states = Map::<StateID, LR0State>::default();
    let mut state_id = {
        let mut next_state_id = StateID::OFFSET;
        move || {
            let id = StateID(next_state_id);
            next_state_id += 1;
            id
        }
    };

    let mut pending_states = VecDeque::<(StateID, Vec<LR0Item>)>::new();
    pending_states.push_back((
        StateID::START,
        Some(LR0Item {
            production: RuleID::ACCEPT,
            index: 0,
        })
        .into_iter()
        .collect(),
    ));

    let mut items = Set::default();
    let mut new_kernels = Map::<SymbolID, Set<LR0Item>>::default();
    let mut isocores = Map::<Vec<LR0Item>, StateID>::default();
    let mut predecessors = Map::<StateID, Map<StateID, SymbolID>>::default();
    while let Some((current, kernels)) = pending_states.pop_front() {
        items.clear();
        for kernel in &kernels {
            items.insert(*kernel);
            let production = &g.rules[&kernel.production];
            if let Some(SymbolID::N(n)) = production.right().get::<usize>(kernel.index.into()) {
                items.extend(&nonkernels[n]);
            }
        }

        let mut reduces = Set::default();
        new_kernels.clear();
        for item in items.drain(..) {
            let production = &g.rules[&item.production];
            match production.right().get::<usize>(item.index.into()) {
                Some(sym) => {
                    let new_kernel = new_kernels.entry(*sym).or_default();
                    new_kernel.insert(LR0Item {
                        index: item.index + 1,
                        ..item
                    });
                }
                None => {
                    reduces.insert(item.production);
                }
            }
        }

        let mut shifts = Map::default();
        let mut gotos = Map::default();
        for (sym, new_kernel) in new_kernels.drain(..) {
            let new_kernel: Vec<_> = new_kernel.into_iter().collect();
            let next = match isocores.get(&new_kernel) {
                Some(id) => *id,
                None => {
                    let id = state_id();
                    isocores.insert(new_kernel.clone(), id);
                    pending_states.push_back((id, new_kernel));
                    id
                }
            };
            match sym {
                SymbolID::T(t) => {
                    shifts.insert(t, next);
                }
                SymbolID::N(n) => {
                    gotos.insert(n, next);
                }
            }
            predecessors.entry(next).or_default().insert(current, sym);
        }

        states.insert(
            current,
            LR0State {
                kernels,
                shifts,
                gotos,
                reduces,
            },
        );
    }

    LR0Automaton { states }
}

fn nonkernels(g: &Grammar) -> Map<NonterminalID, Set<LR0Item>> {
    let mut nonkernels: Map<NonterminalID, Set<LR0Item>> = Map::default();
    for &n in g.nonterminals.keys() {
        let mut items = Set::default();
        for (id, p) in &g.rules {
            if p.left() != n {
                continue;
            }
            items.insert(LR0Item {
                production: *id,
                index: 0,
            });
        }

        let mut added = Set::default();
        loop {
            added.clear();
            for item in &items {
                let production = &g.rules[&item.production];
                if let Some(SymbolID::N(n)) = production.right().get(0) {
                    for (id, p) in &g.rules {
                        if p.left() != *n {
                            continue;
                        }
                        added.insert(LR0Item {
                            production: *id,
                            index: 0,
                        });
                    }
                }
            }

            let changed = added
                .drain(..)
                .fold(false, |changed, item| changed | items.insert(item));
            if !changed {
                break;
            }
        }
        nonkernels.insert(n, items);
    }
    nonkernels
}
