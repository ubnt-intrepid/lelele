use super::{
    annotation::{Action, AnnotationList},
    lalr::{Goto, LASet},
    lr0::{LR0Automaton, LR0State, StateID},
    TerminalSet,
};
use crate::{
    grammar::{Assoc, Grammar, Precedence, SymbolID, TerminalID},
    types::{Map, Queue, Set},
};
use std::cmp::Ordering;

pub fn split_states(
    g: &Grammar,
    lr0: &LR0Automaton,
    lalr: &LASet,
    annotation_list: &AnnotationList,
) -> LR0Automaton {
    let mut lookahead_set_filters = Map::<StateID, Vec<TerminalSet>>::default();
    for (&id, state) in &lr0.states {
        let mut filters = vec![TerminalSet::default(); state.kernels.len()];
        for (j, _kernel) in state.kernels.iter().enumerate() {
            let Some(annotations) = annotation_list.annotations.get(&id) else { continue };
            for (desc, annot) in annotations {
                if annot.rows().any(|row| row.is_set(j)) {
                    filters[j].insert(desc.token);
                }
            }
        }
        lookahead_set_filters.insert(id, filters);
    }

    let mut ielr_states = Map::<StateID, LR0State>::default();
    let mut ielr_state_id = {
        let mut next_state_id = StateID::OFFSET;
        move || {
            let id = StateID::from_raw(next_state_id);
            next_state_id += 1;
            id
        }
    };
    let mut lr0_isocores = Map::<StateID, Set<StateID>>::default();
    let mut ielr_item_lookaheads = Map::<StateID, Vec<TerminalSet>>::default();

    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    struct QueueItem {
        ielr_id: StateID,
        lr0_id: StateID,
    }
    let mut queue = Queue::<QueueItem>::default();
    {
        let start = &lr0.states[&StateID::START];
        queue.push(QueueItem {
            ielr_id: StateID::START,
            lr0_id: StateID::START,
        });
        ielr_states.insert(
            StateID::START,
            LR0State {
                shifts: Map::default(),
                gotos: Map::default(),
                ..(*start).clone()
            },
        );
        lr0_isocores
            .entry(StateID::START)
            .or_default()
            .insert(StateID::START);
        ielr_item_lookaheads.insert(
            StateID::START,
            vec![TerminalSet::default(); start.kernels.len()],
        );
    }

    while let Some(QueueItem { ielr_id, lr0_id }) = queue.pop() {
        let lr0_state = &lr0.states[&lr0_id];
        'transitions: for (sym, lr0_next) in lr0_state
            .shifts
            .iter()
            .map(|(&t, &s)| (SymbolID::T(t), s))
            .chain(lr0_state.gotos.iter().map(|(&n, &s)| (SymbolID::N(n), s)))
        {
            let lr0_next_state = &lr0.states[&lr0_next];
            let mut propagated_lookaheads =
                vec![TerminalSet::default(); lr0_next_state.kernels.len()];
            for (k, (kernel_k, filter)) in lr0_next_state
                .kernels
                .iter()
                .zip(&lookahead_set_filters[&lr0_next])
                .enumerate()
            {
                let lookaheads = &mut propagated_lookaheads[k];
                let prod_k = &g.rules[&kernel_k.production];
                match kernel_k.index {
                    0 => unreachable!(),
                    1 => {
                        let goto = Goto {
                            from: lr0_id,
                            symbol: prod_k.left(),
                        };
                        let item_lookaheads = &ielr_item_lookaheads[&ielr_id];
                        if let Some(follow) = annotation_list.follow_kernel_items.get(&goto) {
                            for k in follow {
                                lookaheads.union_with(&item_lookaheads[k]);
                            }
                        }
                        if let Some(follow) = lalr.always_follows.get(&goto) {
                            lookaheads.union_with(follow);
                        }
                    }
                    _ => {
                        for (kernel_j, item_lookaheads) in lr0_state
                            .kernels
                            .iter()
                            .zip(&ielr_item_lookaheads[&ielr_id])
                        {
                            if kernel_j.production != kernel_k.production
                                || kernel_j.index + 1 != kernel_k.index
                            {
                                continue;
                            }
                            lookaheads.union_with(item_lookaheads);
                            break;
                        }
                    }
                }
                lookaheads.intersect_with(filter);
            }

            if let Some(isocores) = lr0_isocores.get(&lr0_next) {
                for ielr_next in isocores {
                    // check compatibility
                    let mut is_compatible = true;
                    if let Some(annotations) = annotation_list.annotations.get(&lr0_next) {
                        is_compatible = annotations.iter().all(|(desc, annot)| {
                            let dominant_contribution = |item_lookaheads: &Vec<TerminalSet>| {
                                let contributions: Vec<_> = desc
                                    .actions
                                    .iter()
                                    .enumerate()
                                    .filter_map(|(i, action)| {
                                        if !annot.is_defined(i) {
                                            return Some(action.clone());
                                        }
                                        if item_lookaheads.iter().enumerate().any(
                                            |(j, lookaheads)| {
                                                annot.is_set(i, j)
                                                    && lookaheads.contains(desc.token)
                                            },
                                        ) {
                                            return Some(action.clone());
                                        }
                                        None
                                    })
                                    .collect();

                                resolve_conflict(g, desc.token, &contributions)
                            };
                            let c1 = dominant_contribution(&propagated_lookaheads);
                            let c2 = dominant_contribution(&ielr_item_lookaheads[ielr_next]);
                            match (c1, c2) {
                                (Some(c1), Some(c2)) => c1 == c2,
                                _ => true,
                            }
                        });
                    }

                    if !is_compatible {
                        continue;
                    }

                    let mut changed = false;
                    for (added, slot) in propagated_lookaheads
                        .iter()
                        .zip(&mut ielr_item_lookaheads[ielr_next])
                    {
                        let mut diff = slot.clone();
                        diff.difference_with(added);
                        if !diff.is_empty() {
                            changed = true;
                            slot.union_with(added);
                        }
                    }
                    if changed {
                        queue.push(QueueItem {
                            ielr_id: *ielr_next,
                            lr0_id: lr0_next,
                        });
                    }
                    match sym {
                        SymbolID::T(t) => {
                            ielr_states[&ielr_id].shifts.insert(t, *ielr_next);
                        }
                        SymbolID::N(n) => {
                            ielr_states[&ielr_id].gotos.insert(n, *ielr_next);
                        }
                    }
                    continue 'transitions;
                }
            }

            // If no compatible states are existed, initialize a new IELR(1) state.
            let ielr_next = ielr_state_id();
            ielr_states.insert(
                ielr_next,
                LR0State {
                    shifts: Map::default(),
                    gotos: Map::default(),
                    ..lr0_next_state.clone()
                },
            );
            lr0_isocores.entry(lr0_next).or_default().insert(ielr_next);
            ielr_item_lookaheads.insert(ielr_next, propagated_lookaheads);

            match sym {
                SymbolID::T(t) => {
                    ielr_states[&ielr_id].shifts.insert(t, ielr_next);
                }
                SymbolID::N(n) => {
                    ielr_states[&ielr_id].gotos.insert(n, ielr_next);
                }
            }

            queue.push(QueueItem {
                ielr_id: ielr_next,
                lr0_id: lr0_next,
            });
        }
    }

    LR0Automaton {
        states: ielr_states,
    }
}

fn resolve_conflict(g: &Grammar, token: TerminalID, actions: &[Action]) -> Option<Action> {
    match actions.split_first() {
        Some((action, [])) => Some(action.clone()),

        Some((Action::Shift, [head, tail @ ..])) => {
            // shift/reduce conflict
            let Action::Reduce(reduce) = head else { unreachable!() };
            let shift_prec = g.terminals[&token].precedence();
            let reduce_prec = g.rules[reduce].precedence(&g);
            let resolved = compare_precedences(&shift_prec, &reduce_prec);
            for r in tail {
                let Action::Reduce(r) = r else { unreachable!() };
                let prec = g.rules[r].precedence(&g);
                if resolved != compare_precedences(&shift_prec, &prec) {
                    return None;
                }
            }
            match resolved {
                Some(Direction::Left) => Some(Action::Shift),
                Some(Direction::Right) => Some(Action::Reduce(*reduce)),
                None => None,
            }
        }

        Some((Action::Reduce(..), remains)) => {
            // reduce/reduce conflicts cannot be resolved.
            debug_assert!(remains.iter().all(|a| matches!(a, Action::Reduce(..))));
            None
        }

        None => None,
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Direction {
    Left,
    Right,
}

fn compare_precedences(
    shift: &Option<Precedence>,
    reduce: &Option<Precedence>,
) -> Option<Direction> {
    match (shift, reduce) {
        (Some(shift), Some(reduce)) => match Ord::cmp(&shift.priority, &reduce.priority) {
            Ordering::Equal => match shift.assoc {
                Assoc::Left => Some(Direction::Right), // i.e. reduction
                Assoc::Right => Some(Direction::Left), // i.e. shift
                Assoc::Nonassoc => None,
            },
            Ordering::Greater => Some(Direction::Left),
            Ordering::Less => Some(Direction::Right),
        },
        (Some(..), None) => Some(Direction::Left),
        (None, Some(..)) => Some(Direction::Right),
        (None, None) => None,
    }
}
