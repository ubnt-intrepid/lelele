//! Calculatation of IELR(1) annotation list.

use super::{
    cfg::{Grammar, RuleID, SymbolID, TerminalID, TerminalIDSet},
    lalr::{Goto, LASet, Reduce},
    lr0::{LR0Automaton, StateID},
};
use crate::types::{Map, Queue, Set};
use bit_set::BitSet;
use std::{fmt, iter, mem};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AnnotationDesc {
    pub state: StateID,
    pub token: TerminalID,
    pub actions: Vec<Action>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Action {
    Shift,
    Reduce(RuleID),
}

pub struct Annotation {
    rows: u16,
    cols: u16,
    data: Box<[u8]>,
}

impl Annotation {
    pub fn new(rows: usize, cols: usize) -> Self {
        assert!(rows < u16::MAX.into() && cols < u16::MAX.into());
        let n_bits = rows * (cols + 1);
        let n_bytes = n_bits / (8 * mem::size_of::<u8>()) + 1;
        let data = vec![0u8; n_bytes].into_boxed_slice();
        Self {
            rows: rows.try_into().unwrap(),
            cols: cols.try_into().unwrap(),
            data,
        }
    }

    pub fn num_rows(&self) -> usize {
        self.rows.into()
    }

    pub fn num_cols(&self) -> usize {
        self.cols.into()
    }

    pub fn set(&mut self, i: usize, j: usize) {
        self.set_raw(i * (usize::from(self.cols) + 1), true);
        self.set_raw(i * (usize::from(self.cols) + 1) + j + 1, true);
    }

    pub fn is_set(&self, i: usize, j: usize) -> bool {
        self.get_raw(i * (usize::from(self.cols) + 1) + j + 1)
    }

    pub fn is_defined(&self, i: usize) -> bool {
        self.get_raw(i * (usize::from(self.cols) + 1))
    }

    pub fn is_useless(&self) -> bool {
        self.data.iter().all(|b| *b == 0)
    }

    fn set_raw(&mut self, offset: usize, value: bool) {
        let b = offset / (8 * mem::size_of::<u8>());
        let w = offset % (8 * mem::size_of::<u8>());
        if value {
            self.data[b] |= 0b1 << w;
        } else {
            self.data[b] &= !(0b1 >> w);
        }
    }

    fn get_raw(&self, offset: usize) -> bool {
        let b = offset / (8 * mem::size_of::<u8>());
        let w = offset % (8 * mem::size_of::<u8>());
        self.data[b] & (0b1 << w) != 0
    }

    pub fn rows(&self) -> impl Iterator<Item = Row<'_>> + '_ {
        let mut row = 0;
        iter::from_fn(move || {
            if row < self.rows {
                let res = Some(Row {
                    annotation: self,
                    row,
                });
                row += 1;
                res
            } else {
                None
            }
        })
    }
}
impl fmt::Debug for Annotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_map();
        for (i, row) in self.rows().enumerate() {
            if row.is_defined() {
                f.entry(&i, &row);
            }
        }
        f.finish()
    }
}

pub struct Row<'a> {
    annotation: &'a Annotation,
    row: u16,
}
impl Row<'_> {
    pub fn is_set(&self, j: usize) -> bool {
        self.annotation.is_set(usize::from(self.row), j)
    }

    pub fn is_defined(&self) -> bool {
        self.annotation.is_defined(usize::from(self.row))
    }
}
impl fmt::Debug for Row<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut dbg = f.debug_set();
        for j in 0..self.annotation.cols {
            if self.is_set(usize::from(j)) {
                dbg.entry(&j);
            }
        }
        dbg.finish()
    }
}

#[derive(Debug)]
pub struct AnnotationList {
    pub annotations: Map<StateID, Map<AnnotationDesc, Annotation>>,
    pub follow_kernel_items: Map<Goto, BitSet>,
}

pub fn annotation_lists(g: &Grammar, lr0: &LR0Automaton, lalr: &LASet) -> AnnotationList {
    let predecessors = lr0.predecessors();
    let follow_kernel_items = follow_kernel_items(&g, &lr0, &lalr);
    let item_lookaheads = item_lookaheads(g, &lr0, &lalr.follows, &predecessors);

    // Collect inadequacy manifestation descriptions:
    //   inadequacy_lists[s] = { ni = (s, t, Γ(t,s)) | |Γ(t,s)| > 2 }
    let mut inadequacy_lists = Map::<StateID, Set<AnnotationDesc>>::default();
    let mut contributions = Map::<TerminalID, Vec<Action>>::default();
    for (&id, state) in &lr0.states {
        contributions.clear();
        for &token in state.shifts.keys() {
            contributions.entry(token).or_default().push(Action::Shift);
        }
        for &reduce in &state.reduces {
            let r = Reduce {
                state: id,
                production: reduce,
            };
            if let Some(lookaheads) = lalr.lookaheads.get(&r) {
                for token in lookaheads.iter() {
                    contributions
                        .entry(token)
                        .or_default()
                        .push(Action::Reduce(reduce));
                }
            }
        }

        let mut descs = Set::<AnnotationDesc>::default();
        for (token, mut actions) in contributions.drain(..) {
            actions.sort();
            if actions.len() > 1 {
                descs.insert(AnnotationDesc {
                    state: id,
                    token,
                    actions,
                });
            }
        }
        inadequacy_lists.insert(id, descs);
    }

    if inadequacy_lists.is_empty() {
        return AnnotationList {
            annotations: Map::default(),
            follow_kernel_items,
        };
    }

    let mut annotation_lists = Map::<StateID, Map<AnnotationDesc, Annotation>>::default();
    let mut pending_states = Queue::<(StateID, StateID)>::with_capacity(lr0.states.len());

    // given s: { na = annotate_manifestation(ni) | ni=(s, t, Γ(t,s)) ∈ inadequacy_lists[s] }
    for (state_id, inadequacy_list) in &inadequacy_lists {
        let state = &lr0.states[state_id];
        for desc in inadequacy_list {
            // annotate_manifestation(s, ni)
            let mut annotation = Annotation::new(desc.actions.len(), state.kernels.len());
            for (i, action) in desc.actions.iter().enumerate() {
                if let Action::Reduce(r) = action {
                    let production = &g.rules[r];
                    if production.right.is_empty() {
                        // compute_lhs_contributions
                        let goto = Goto {
                            from: *state_id,
                            symbol: production.left,
                        };
                        if !lalr.always_follows[&goto].contains(desc.token) {
                            for (j, _kernel) in state.kernels.iter().enumerate() {
                                if follow_kernel_items[&goto].contains(j)
                                    && item_lookaheads[state_id][j].contains(desc.token)
                                {
                                    annotation.set(i, j);
                                }
                            }
                        }
                    } else {
                        for (j, kernel) in state.kernels.iter().enumerate() {
                            if kernel.production == *r
                                && usize::from(kernel.index) == production.right.len()
                            {
                                annotation.set(i, j);
                            }
                        }
                    }
                }
            }

            annotation_lists
                .entry(*state_id)
                .or_default()
                .insert(desc.clone(), annotation);
        }

        if let Some(predecessors) = predecessors.get(state_id) {
            for &prev in predecessors {
                pending_states.push((prev, *state_id));
            }
        }
    }

    // given s: { na = annotate_predecessor(s,s',na') | s -> s', na' \in annotation_lists[s'] }
    let mut added = Map::<AnnotationDesc, Annotation>::default();
    while let Some((current, next)) = pending_states.pop() {
        let current_state = &lr0.states[&current];
        let next_state = &lr0.states[&next];

        let Some(annotation_list) = annotation_lists.get(&next) else { continue };
        added.clear();
        for (desc, annotation) in annotation_list {
            if annotation_lists
                .get(&current)
                .map_or(false, |list| list.contains_key(desc))
            {
                continue;
            }

            let mut new_annotation =
                Annotation::new(annotation.rows.into(), current_state.kernels.len());
            for (i, row) in annotation.rows().enumerate() {
                if !row.is_defined() {
                    continue;
                }

                if next_state.kernels.iter().enumerate().any(|(j, kernel)| {
                    let production = &g.rules[&kernel.production];
                    let goto = Goto {
                        from: current,
                        symbol: production.left,
                    };
                    row.is_set(j)
                        && kernel.index == 1
                        && lalr.always_follows[&goto].contains(desc.token)
                }) {
                    // keep γ[i] undefined;
                    continue;
                }

                // calculate γ[i][j]
                for (j, kernel_j) in current_state.kernels.iter().enumerate() {
                    if next_state.kernels.iter().enumerate().any(|(k, kernel_k)| {
                        if !row.is_set(k) {
                            return false;
                        }

                        if kernel_j.production == kernel_k.production
                            && kernel_j.index == kernel_k.index - 1
                            && item_lookaheads[&current][j].contains(desc.token)
                        {
                            return true;
                        }

                        if kernel_k.index != 1 {
                            return false;
                        }

                        let production = &g.rules[&kernel_k.production];
                        let goto = Goto {
                            from: current,
                            symbol: production.left,
                        };
                        if lalr.always_follows[&goto].contains(desc.token) {
                            return false;
                        }
                        follow_kernel_items[&goto].contains(j)
                            && item_lookaheads[&current][j].contains(desc.token)
                    }) {
                        new_annotation.set(i, j);
                    }
                }
            }
            added.insert(desc.clone(), new_annotation);
        }

        if added.is_empty() {
            continue;
        }

        let mut changed = false;
        for (desc, annot) in added.drain(..) {
            if annot.is_useless() {
                continue;
            }
            let slot = annotation_lists.entry(current).or_insert_with(|| {
                changed = true;
                Map::default()
            });
            if !slot.contains_key(&desc) {
                changed = true;
                slot.insert(desc, annot);
            }
        }

        if !changed {
            continue;
        }

        if let Some(predecessors) = predecessors.get(&current) {
            for &prev in predecessors {
                pending_states.push((prev, current));
            }
        }
    }

    AnnotationList {
        annotations: annotation_lists,
        follow_kernel_items,
    }
}

fn follow_kernel_items(g: &Grammar, lr0: &LR0Automaton, lalr: &LASet) -> Map<Goto, BitSet> {
    let mut follow_kernel_items = Map::<Goto, BitSet>::default();

    for key in lalr.gotos.keys() {
        let state = &lr0.states[&key.from];
        let mut kernels = BitSet::default();
        for (k, kernel) in state.kernels.iter().enumerate() {
            let production = &g.rules[&kernel.production];
            match production.right.get(usize::from(kernel.index)..) {
                Some([SymbolID::N(rho_d), gamma @ ..]) => {
                    if key.symbol == *rho_d
                        && gamma
                            .iter()
                            .all(|s| matches!(s, SymbolID::N(n) if g.nullables.contains(n)))
                    {
                        kernels.insert(k);
                    }
                }
                _ => (),
            }
        }
        follow_kernel_items.insert(*key, kernels);
    }

    super::digraph::digraph(&mut follow_kernel_items, |a, b| {
        lalr.includes.get(a).map_or(false, |i| match i.get(b) {
            Some(is_beta_empty) => *is_beta_empty,
            None => false,
        })
    });

    follow_kernel_items
}

fn item_lookaheads(
    g: &Grammar,
    lr0: &LR0Automaton,
    follows: &Map<Goto, TerminalIDSet>,
    predecessors: &Map<StateID, Set<StateID>>,
) -> Map<StateID, Vec<TerminalIDSet>> {
    let mut item_lookaheads = Map::<StateID, Vec<TerminalIDSet>>::default();

    let mut pending_states: Queue<StateID> = lr0.states.keys().copied().collect();
    'queue: while let Some(id) = pending_states.pop() {
        if item_lookaheads.contains_key(&id) {
            continue;
        }

        let state = &lr0.states[&id];
        let mut lookaheads = vec![TerminalIDSet::default(); state.kernels.len()];
        for (k, kernel) in state.kernels.iter().enumerate() {
            let production = &g.rules[&kernel.production];
            match kernel.index {
                0 => {
                    // Only if the production is #Start -> S #EOI`.
                    // The lookhead sets should be empty by definition.
                }
                1 => {
                    // X -> [ A . beta ]
                    //  => \bigcup { Follow(p,A) | p is predecessor }
                    if let Some(predecessors) = predecessors.get(&id) {
                        for &from in predecessors {
                            for &symbol in lr0.states[&from].gotos.keys() {
                                if symbol != production.left {
                                    continue;
                                }
                                lookaheads[k].union_with(&follows[&Goto { from, symbol }]);
                            }
                        }
                    }
                }
                _ => {
                    // X -> [ ... A . beta ]
                    //  => same as the LA set of X -> [ ... . A beta ] in predecessor
                    if let Some(predecessors) = predecessors.get(&id) {
                        'outer: for &from in predecessors {
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
        }
        item_lookaheads.insert(id, lookaheads);
    }

    item_lookaheads
}
