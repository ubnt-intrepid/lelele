//! Calculation of LR(1) parse table with conflict resolution.

use super::{lalr::LALRData, lr0::LR0Automaton};
use crate::{
    grammar::{Assoc, Grammar, NonterminalID, Precedence, RuleID, TerminalID},
    ielr::{lalr::Reduce, lr0::StateID},
    types::Map,
    util::display_fn,
};
use std::{cmp::Ordering, fmt};

#[derive(Debug, thiserror::Error)]
pub enum DFAError {
    #[error("error during resolving conflicts")]
    ConflictResolution(
        #[from]
        #[source]
        ConflictResolutionError,
    ),
}

#[derive(Debug)]
pub struct ParseTable {
    pub states: Map<StateID, ParseTableRow>,
}

impl ParseTable {
    pub fn display<'g>(&'g self, g: &'g Grammar) -> impl fmt::Display + 'g {
        display_fn(|f| {
            for (i, (id, node)) in self.states.iter().enumerate() {
                if i > 0 {
                    writeln!(f)?;
                }

                writeln!(f, "#### State {:?}", id)?;
                writeln!(f, "## actions")?;
                for (token, action) in &node.actions {
                    let token = &g.terminals[token];
                    match action {
                        Action::Shift(n) => {
                            writeln!(f, "- {} => shift({:?})", token, n)?;
                        }
                        Action::Reduce(reduce) => {
                            let reduce = &g.rules[reduce];
                            writeln!(f, "- {} => reduce({})", token, reduce.display(g))?;
                        }
                        Action::Accept => {
                            writeln!(f, "- {} => accept", token)?;
                        }
                        Action::Fail => {
                            writeln!(f, "- {} => fail", token)?;
                        }
                        Action::Inconsistent {
                            reason,
                            shift,
                            reduces,
                        } => {
                            let token = &g.terminals[&token.id()];
                            writeln!(f, "- {} => inconsistent(reason = {:?})", token, reason)?;
                            writeln!(f, "## conflicted actions on {}", token)?;
                            if let Some(n) = shift {
                                writeln!(f, "  - shift({:?})", n)?;
                            }
                            for reduce in reduces {
                                let reduce = &g.rules[reduce];
                                writeln!(f, "  - reduce({})", reduce.display(g))?;
                            }
                        }
                    }
                }

                writeln!(f, "## gotos")?;
                for (symbol, goto) in &node.gotos {
                    writeln!(f, "- {} => goto({:?})", g.nonterminals[symbol], goto)?;
                }
            }
            Ok(())
        })
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub struct ParseTableRow {
    pub actions: Map<TerminalID, Action>,
    pub gotos: Map<NonterminalID, StateID>,
}

/// The action that the LR automaton in a state performs on a particular
/// lookahead symbol.
#[derive(Debug)]
#[non_exhaustive]
pub enum Action {
    /// Read a lookahead symbol and transition to the specified state.
    Shift(StateID),

    /// Reduce to the specified production rule.
    Reduce(RuleID),

    Accept,

    /// Reject the specified lookahead symbol.
    ///
    /// The behavior of this action is the same as if no action exists
    /// for the given lookahead symbol, but is explicitly inserted by
    /// resolving some shift/reduce conflicts.
    Fail,

    /// There are multiple conflicting actions for the lookahead symbol.
    Inconsistent {
        shift: Option<StateID>,
        reduces: Vec<RuleID>,
        reason: ConflictReason,
    },
}
impl Action {
    pub fn is_consistent(&self) -> bool {
        !matches!(self, Self::Inconsistent { .. })
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub enum ConflictReason {
    /// The state has multiple reductions even though it has no shift action
    /// on the target lookahead symbol.
    MultipleReductionWithoutShift,

    /// At least one of the competing actions has no precedence.
    MissingPrecedence,

    /// At least one of shift/reduce conflict resolution result is
    /// inconsistent with the others.
    InconsistentShiftResolution,
}

#[derive(Debug, thiserror::Error)]
pub enum ConflictResolutionError {
    #[error("there is no actions")]
    EmptyActions,

    #[error("detected shift/accept conflict(s)")]
    ShiftAcceptConflict,

    #[error("detected reduce/accept conflict(s)")]
    ReduceAcceptConflict,
}

pub fn generate(g: &Grammar, lr0: &LR0Automaton, lalr: &LALRData) -> Result<ParseTable, DFAError> {
    let mut states = Map::default();
    for (&id, lr0_state) in &lr0.states {
        #[derive(Default)]
        struct PendingAction {
            shift: Option<StateID>,
            reduces: Vec<RuleID>,
        }
        let mut pending_actions = Map::<TerminalID, PendingAction>::default();
        for (&t, &next) in &lr0_state.shifts {
            let action = pending_actions.entry(t).or_default();
            if t == TerminalID::EOI {
                action.reduces.push(RuleID::ACCEPT);
            } else {
                action.shift.replace(next);
            }
        }
        for &reduce in &lr0_state.reduces {
            if reduce != RuleID::ACCEPT {
                let key = Reduce {
                    state: id,
                    production: reduce,
                };
                for t in lalr.lookaheads[&key].iter() {
                    let action = pending_actions.entry(t).or_default();
                    action.reduces.push(reduce);
                }
            }
        }

        let mut actions: Map<TerminalID, Action> = Map::default();
        for (symbol, action) in pending_actions {
            let resolved = resolve_conflict(g, symbol, action.shift, &action.reduces)?;
            actions.insert(symbol, resolved);
        }

        let mut gotos = Map::default();
        for (&n, &next) in &lr0_state.gotos {
            gotos.insert(n, next);
        }

        states.insert(id, ParseTableRow { actions, gotos });
    }

    Ok(ParseTable { states })
}

/// Attempts to resolve shift/reduce conflicts based on precedence/associativity.
fn resolve_conflict(
    g: &Grammar,
    symbol: TerminalID,
    shift: Option<StateID>,
    reduces: &[RuleID],
) -> Result<Action, ConflictResolutionError> {
    use Action::*;

    match (shift, reduces) {
        (Some(next), []) => Ok(Shift(next)),
        (None, [RuleID::ACCEPT]) => Ok(Accept),
        (None, [reduce]) => Ok(Reduce(*reduce)),
        (None, []) => Err(ConflictResolutionError::EmptyActions),

        // exactly one shift/reduce conflict
        (Some(next), [reduce]) => {
            if *reduce == RuleID::ACCEPT {
                return Err(ConflictResolutionError::ShiftAcceptConflict);
            }

            let shift_prec = g.terminals[&symbol].precedence();
            let reduce_prec = g.rules[reduce].precedence(g);

            match compare_precs(shift_prec, reduce_prec) {
                Some(PrecDiff::Left) => Ok(Shift(next)),
                Some(PrecDiff::Right) => Ok(Reduce(*reduce)),
                Some(PrecDiff::Neither) => Ok(Fail),
                None => Ok(Inconsistent {
                    shift,
                    reduces: reduces.to_owned(),
                    reason: ConflictReason::MissingPrecedence,
                }),
            }
        }

        // multiple shift/reduce conflicts
        (Some(next), reduces) => {
            let shift_prec = g.terminals[&symbol].precedence();

            let mut resolved = None;
            for reduce in reduces {
                if *reduce == RuleID::ACCEPT {
                    return Err(ConflictResolutionError::ShiftAcceptConflict);
                }
                let reduce_prec = g.rules[reduce].precedence(g);
                let new_resolved = match compare_precs(shift_prec, reduce_prec) {
                    Some(diff) => diff,
                    None => {
                        return Ok(Inconsistent {
                            shift,
                            reduces: reduces.to_owned(),
                            reason: ConflictReason::MissingPrecedence,
                        })
                    }
                };

                match (resolved, new_resolved) {
                    (Some(PrecDiff::Left), PrecDiff::Left)
                    | (Some(PrecDiff::Neither), PrecDiff::Neither) => (),
                    (None, diff) => resolved = Some(diff),
                    _ => {
                        return Ok(Inconsistent {
                            shift,
                            reduces: reduces.to_owned(),
                            reason: ConflictReason::InconsistentShiftResolution,
                        })
                    }
                }
            }

            match resolved {
                Some(PrecDiff::Left) => Ok(Shift(next)),
                Some(PrecDiff::Neither) => Ok(Fail),
                _ => unreachable!(),
            }
        }

        // reduce/reduce conflict(s)
        (None, reduces) => {
            debug_assert!(reduces.len() > 1);
            if reduces.contains(&RuleID::ACCEPT) {
                return Err(ConflictResolutionError::ReduceAcceptConflict);
            }

            Ok(Inconsistent {
                shift: None,
                reduces: reduces.to_owned(),
                reason: ConflictReason::MultipleReductionWithoutShift,
            })
        }
    }
}

#[derive(Copy, Clone)]
enum PrecDiff {
    Left,
    Right,
    Neither,
}
fn compare_precs(
    shift_prec: Option<Precedence>,
    reduce_prec: Option<Precedence>,
) -> Option<PrecDiff> {
    match (shift_prec, reduce_prec) {
        (Some(p1), Some(p2)) => match Ord::cmp(&p1.priority, &p2.priority) {
            Ordering::Greater => Some(PrecDiff::Left),
            Ordering::Less => Some(PrecDiff::Right),
            Ordering::Equal => match p1.assoc {
                Assoc::Left => Some(PrecDiff::Right),
                Assoc::Right => Some(PrecDiff::Left),
                Assoc::Nonassoc => Some(PrecDiff::Neither),
            },
        },
        _ => None,
    }
}
