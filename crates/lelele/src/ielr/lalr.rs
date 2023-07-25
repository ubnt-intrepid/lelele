//! LALR(1) look-ahead sets computation.
//!
//! The algorithm is based on DeRemer and Pennello's method\[1\], but also modified
//! to calculate auxiliary data mentioned in Denny and Malloy \[2\].
//!
//! \[1\]: DeRemer and Pennello, Efficient Computation of LALR(1) Look-Ahead Sets
//!       <https://dl.acm.org/doi/10.1145/69622.357187>
//!
//! \[2\]: Denny and Malloy, The IELR(1) algorithm for generating minimal LR(1) parser
//!       tables for non-LR(1) grammars with conflict resolution
//!       <https://www.sciencedirect.com/science/article/pii/S0167642309001191>

use super::{
    grammar::{Grammar, NonterminalID, ProductionID, SymbolID, TerminalSet},
    lr0::{LR0Automaton, StateID},
};
use crate::types::{Map, Queue, Set};
use std::fmt;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Goto {
    pub from: StateID,
    pub symbol: NonterminalID,
}

impl fmt::Debug for Goto {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:?},{:?})", self.from, self.symbol)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Reduce {
    pub state: StateID,
    pub production: ProductionID,
}

impl fmt::Debug for Reduce {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:?},{:?})", self.state, self.production)
    }
}

#[derive(Debug)]
pub struct LALRData {
    pub gotos: Map<Goto, StateID>,
    pub lookaheads: Map<Reduce, TerminalSet>,
    pub item_lookaheads: Map<StateID, Vec<TerminalSet>>,
    pub always_follows: Map<Goto, TerminalSet>,
    pub reads: Map<Goto, Set<Goto>>,
    pub includes: Map<Goto, Map<Goto, bool>>,
}

impl LALRData {
    pub fn always_includes(&self, a: &Goto, b: &Goto) -> bool {
        self.includes
            .get(a)
            .map_or(false, |i| i.get(b).copied().unwrap_or(false))
    }
}

/// Compute the look-ahead sets corresponding to the reductions in the provided LR automaton.
pub fn lalr(g: &Grammar, lr0: &LR0Automaton) -> LALRData {
    // Step 0: extract goto transitions and their direct-read sets.
    //
    // The direct-read set on the goto transition (p,A) (p: StateID, A: NonterminalID) is defined as follows:
    //   DirectRead(p,A) := { t \in T | \exists r:StateID s.t. p --(A)--> r --(t)--> ... }
    let mut gotos = Map::default();
    let mut direct_reads = Map::default();
    for (&from, state) in &lr0.states {
        for (&n, &to) in &state.gotos {
            let key = Goto { from, symbol: n };
            gotos.insert(key, to);
            direct_reads.insert(key, lr0.states[&to].shifts.keys().copied().collect());
        }
    }

    // Step 0.5: calcualte auxiliary data.
    //
    // - (p,A) `reads` (r,C) <==> p --(A)--> r --(C)--> && C =>* ε
    // - (p,A) `includes` (p',B) <==> B -> βAγ, γ =>* ε, p' -(β)-> p
    // - (q, A->ω) `lookback` (p,A) <==> p --(ω)--> q
    let reads = calc_reads(&g, lr0, &gotos);
    let includes = calc_includes(&g, lr0, &gotos);
    let lookbacks = calc_lookbacks(&g, lr0);

    // Step 1: calculate AlwaysFollow(p,A)
    //   AlwaysFollow(p,A) = DirectRead(p,A) \cup \bigcup {
    //      AlwaysFollow(r,C)
    //          | (p,A) `reads` (r,C) \lor (p,A) `always_includes` (r,C)
    //   }
    // where the relation `always_includes` means that:
    //   (p,A) `always_includes` (p',B) <==> B -> βAγ, γ =>* ε, p' -(β)-> p && β == ε
    // and is equivalent to the relation `includes` except for the additional condition that
    // the leading symbol β is empty.
    let mut always_follows = direct_reads;
    super::digraph::digraph(&mut always_follows, |a, b| {
        reads.get(a).map_or(false, |i| i.contains(b))
            || includes
                .get(a)
                .map_or(false, |i| i.get(b).copied().unwrap_or(false))
    });

    // Step 2: calculate Follow(p,A) from AlwaysFollow(p,A) and `includes` relations.
    //   Follow(p,A) = AlwaysFollow(p,A) \cup \bigcup { AlwaysFollow(p',B) | (p,A) `includes` (p',B) }
    let mut follows = always_follows.clone();
    super::digraph::digraph(&mut follows, |a, b| {
        includes.get(a).map_or(false, |i| i.contains_key(b))
    });

    // Step 3: calculate LA(q,A->ω) from Follow(p,A) and `lookbacks` relations.
    //   LA(q,A->ω) = \bigcup { Follow(p,A) | (q,A->ω) `lookback` (p,A) }
    let mut lookaheads = Map::<_, TerminalSet>::default();
    for (&from, lr0_state) in &lr0.states {
        for &production in &lr0_state.reduces {
            let reduce_id = Reduce {
                state: from,
                production,
            };
            for goto in &lookbacks[&reduce_id] {
                if let Some(follows) = follows.get(goto) {
                    lookaheads.entry(reduce_id).or_default().union_with(follows);
                }
            }
        }
    }

    // Step 4: calculate lookahead sets for LR(0) kernel items.
    let item_lookaheads = item_lookaheads(g, lr0, &follows);

    LALRData {
        gotos,
        lookaheads,
        item_lookaheads,
        always_follows,
        reads,
        includes,
    }
}

fn item_lookaheads(
    g: &Grammar,
    lr0: &LR0Automaton,
    follows: &Map<Goto, TerminalSet>,
) -> Map<StateID, Vec<TerminalSet>> {
    let mut item_lookaheads = Map::<StateID, Vec<TerminalSet>>::default();

    let mut pending_states: Queue<StateID> = lr0.states.keys().copied().collect();
    'queue: while let Some(id) = pending_states.pop() {
        if item_lookaheads.contains_key(&id) {
            continue;
        }

        let state = &lr0.states[&id];
        let mut lookaheads = vec![TerminalSet::default(); state.kernels.len()];
        for (k, kernel) in state.kernels.iter().enumerate() {
            let production = g.production(kernel.production);
            match kernel.index {
                0 => {
                    // Only if the production is #Start -> S #EOI`.
                    // The lookhead sets should be empty by definition.
                }
                1 => {
                    // X -> [ A . beta ]
                    //  => \bigcup { Follow(p,A) | p is predecessor }
                    for &from in state.predecessors.keys() {
                        for &symbol in lr0.states[&from].gotos.keys() {
                            if symbol != production.left {
                                continue;
                            }
                            lookaheads[k].union_with(&follows[&Goto { from, symbol }]);
                        }
                    }
                }
                _ => {
                    // X -> [ ... A . beta ]
                    //  => same as the LA set of X -> [ ... . A beta ] in predecessor
                    'outer: for &from in state.predecessors.keys() {
                        let from_s = &lr0.states[&from];
                        for (j, from_kernel) in from_s.kernels.iter().enumerate() {
                            if from_kernel.production != kernel.production
                                || from_kernel.index != kernel.index - 1
                            {
                                continue;
                            }
                            let Some(added) = item_lookaheads.get(&from) else {
                                pending_states.push(from);
                                pending_states.push(id);
                                continue 'queue;
                            };
                            lookaheads[k].union_with(&added[j]);
                            break 'outer;
                        }
                    }
                }
            }
        }
        item_lookaheads.insert(id, lookaheads);
    }

    item_lookaheads
}

fn calc_reads(g: &Grammar, lr0: &LR0Automaton, gotos: &Map<Goto, StateID>) -> Map<Goto, Set<Goto>> {
    let mut reads = Map::<Goto, Set<Goto>>::default();

    for a_key in gotos.keys() {
        let r = &lr0.states[&a_key.from].gotos[&a_key.symbol];
        reads.entry(*a_key).or_default().extend(
            lr0.states[r]
                .gotos
                .keys()
                .filter(|&c| g.nullables.contains(c))
                .map(|&c| Goto {
                    from: *r,
                    symbol: c,
                }),
        );
    }

    reads
}

fn calc_includes(
    g: &Grammar,
    lr0: &LR0Automaton,
    gotos: &Map<Goto, StateID>,
) -> Map<Goto, Map<Goto, bool>> {
    let mut includes = Map::<Goto, Map<Goto, bool>>::default();

    for a_key in gotos.keys() {
        for b_key in gotos.keys() {
            'outer: for p in g.productions.values() {
                // B -> β A γ and γ =>* ε
                if p.left != b_key.symbol {
                    continue;
                }
                let beta = match p
                    .right
                    .iter()
                    .position(|s| matches!(s, SymbolID::N(n) if *n == a_key.symbol))
                {
                    Some(i) => {
                        let is_gamma_nullable = p.right[i + 1..]
                            .iter()
                            .all(|s| matches!(s, SymbolID::N(n) if g.nullables.contains(n)));
                        if !is_gamma_nullable {
                            continue;
                        }
                        &p.right[..i]
                    }
                    None => continue,
                };

                // p' -->(beta)--> p
                let mut current = b_key.from;
                for n in beta {
                    let next = match n {
                        SymbolID::T(t) => lr0.states[&current].shifts.get(t),
                        SymbolID::N(n) => lr0.states[&current].gotos.get(n),
                    };
                    match next {
                        Some(next) => current = *next,
                        None => continue 'outer,
                    }
                }
                if current != a_key.from {
                    continue;
                }

                includes
                    .entry(*a_key)
                    .or_default()
                    .insert(*b_key, beta.is_empty());
            }
        }
    }

    includes
}

fn calc_lookbacks(g: &Grammar, lr0: &LR0Automaton) -> Map<Reduce, Set<Goto>> {
    let mut lookbacks = Map::<Reduce, Set<Goto>>::default();

    for &from in lr0.states.keys() {
        for (&p_id, p) in &g.productions {
            let mut current = from;
            let mut right = &p.right[..];
            while !right.is_empty() {
                let next = match &right[0] {
                    SymbolID::T(t) => lr0.states[&current].shifts.get(t),
                    SymbolID::N(n) => lr0.states[&current].gotos.get(n),
                };
                match next {
                    Some(next) => {
                        current = *next;
                        right = &right[1..];
                    }
                    None => break,
                }
            }
            if right.is_empty() {
                let reduce = Reduce {
                    state: current,
                    production: p_id,
                };
                lookbacks.entry(reduce).or_default().insert(Goto {
                    from,
                    symbol: p.left,
                });
            }
        }
    }

    lookbacks
}
