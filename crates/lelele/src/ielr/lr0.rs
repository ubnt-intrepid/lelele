use super::grammar::{Grammar, NonterminalID, ProductionID, SymbolID, TerminalID};
use crate::types::{Map, Set};
use std::{collections::VecDeque, fmt};

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StateID(u16);
impl fmt::Debug for StateID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "S#{:03}", self.0)
    }
}
impl StateID {
    pub const fn from_raw(raw: u16) -> Self {
        Self(raw)
    }
}

/// The LR(0) item, a.k.a. LR item core.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LR0Item {
    pub production: ProductionID,
    pub index: u16,
}
impl LR0Item {
    pub fn display<'g>(&'g self, g: &'g Grammar) -> impl fmt::Display + 'g {
        crate::util::display_fn(|f| {
            let production = g.production(self.production);
            write!(f, "{} -> [ ", g.nonterminals[&production.left])?;
            for (i, r) in production.right.iter().enumerate() {
                if i > 0 {
                    f.write_str(" ")?;
                }
                if i == self.index as usize {
                    f.write_str(". ")?;
                }
                match r {
                    SymbolID::N(n) => f.write_str(&*g.nonterminals[n])?,
                    SymbolID::T(t) => f.write_str(&*g.terminals[t].name)?,
                }
            }
            if production.right.len() == self.index as usize {
                write!(f, " .")?;
            }
            write!(f, " ]")
        })
    }
}

#[derive(Debug, Clone)]
pub struct LR0State {
    pub kernels: Vec<LR0Item>,
    pub shifts: Map<TerminalID, StateID>,
    pub gotos: Map<NonterminalID, StateID>,
    pub reduces: Set<ProductionID>,
    pub predecessors: Map<StateID, SymbolID>,
}

impl LR0State {
    pub fn display<'g>(&'g self, g: &'g Grammar) -> impl fmt::Display + 'g {
        crate::util::display_fn(|f| {
            writeln!(f, "## kernels:")?;
            for kernel in &self.kernels {
                writeln!(f, "- {}", kernel.display(g))?;
            }
            if !self.shifts.is_empty() {
                writeln!(f, "## shifts:")?;
                for (t, to) in &self.shifts {
                    writeln!(f, "- {} => {:?}", g.terminals[t].name, to)?;
                }
            }
            if !self.gotos.is_empty() {
                writeln!(f, "## gotos:")?;
                for (n, to) in &self.gotos {
                    writeln!(f, "- {} => {:?}", g.nonterminals[n], to)?;
                }
            }
            if !self.reduces.is_empty() {
                writeln!(f, "## reduces:")?;
                for reduce in &self.reduces {
                    writeln!(f, "- {}", g.production(*reduce).display(g))?;
                }
            }
            if !self.predecessors.is_empty() {
                writeln!(f, "## predecessors:")?;
                for (next, sym) in &self.predecessors {
                    let sym = match sym {
                        SymbolID::T(t) => g.terminals[t].name.as_str(),
                        SymbolID::N(n) => g.nonterminals[n].as_str(),
                    };
                    writeln!(f, "- {:?} --({})-->", next, sym)?;
                }
            }
            Ok(())
        })
    }
}

#[derive(Debug)]
pub struct LR0Automaton {
    pub states: Map<StateID, LR0State>,
}

/// Calculate the LR(0) automaton based on the specified grammar.
pub fn lr0(g: &Grammar) -> LR0Automaton {
    let nonkernels = nonkernels(&g);

    let mut states = Map::<StateID, LR0State>::default();
    let mut state_id = {
        let mut next_state_id = 0;
        move || {
            let id = StateID(next_state_id);
            next_state_id += 1;
            id
        }
    };

    let mut pending_states = VecDeque::<(StateID, Vec<LR0Item>)>::new();
    pending_states.push_back((
        state_id(),
        Some(LR0Item {
            production: ProductionID::ACCEPT,
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
            let production = g.production(kernel.production);
            if let Some(SymbolID::N(n)) = production.right.get::<usize>(kernel.index.into()) {
                items.extend(&nonkernels[n]);
            }
        }

        let mut reduces = Set::default();
        new_kernels.clear();
        for item in items.drain(..) {
            let production = g.production(item.production);
            match production.right.get::<usize>(item.index.into()) {
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
                predecessors: Map::default(),
            },
        );
    }

    for (id, predecessors) in predecessors {
        states[&id].predecessors = predecessors;
    }

    LR0Automaton { states }
}

fn nonkernels(g: &Grammar) -> Map<NonterminalID, Set<LR0Item>> {
    let mut nonkernels: Map<NonterminalID, Set<LR0Item>> = Map::default();
    for &n in g.nonterminals.keys() {
        let mut items = Set::default();
        for (id, p) in &g.productions {
            if p.left != n {
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
                let production = g.production(item.production);
                if let Some(SymbolID::N(n)) = production.right.get(0) {
                    for (id, p) in &g.productions {
                        if p.left != *n {
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
