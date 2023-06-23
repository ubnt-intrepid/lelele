use crate::{
    dfa::{Action, NodeID, DFA},
    grammar::{Assoc, Precedence, Rule, Symbol},
    IndexMap,
};
use std::cmp::Ordering;

#[derive(Debug)]
pub struct ParseTable {
    pub(crate) map: IndexMap<NodeID, IndexMap<Symbol, ResolvedAction>>,
}

impl ParseTable {
    pub fn generate(dfa: &DFA) -> Self {
        let mut map = IndexMap::default();

        for (node_id, node) in dfa.nodes() {
            let mut actions = IndexMap::<Symbol, ResolvedAction>::default();

            for (symbol, action) in node.actions() {
                // FIXME: report conflict
                if let Ok(resolved) = resolve_action(symbol, action) {
                    actions.insert(symbol.clone(), resolved);
                }
            }

            map.insert(node_id, actions);
        }

        Self { map }
    }
}

/// Attempts to resolve shift/reduce conflicts based on precedence/associativity.
fn resolve_action(symbol: &Symbol, action: &Action) -> Result<ResolvedAction, ResolveError> {
    use ResolvedAction::*;

    if action.accepted {
        debug_assert!(action.shift.is_none(), "detect shift/accept conflict");
        debug_assert!(action.reduces.is_empty(), "detect reduce/accept conflict");
        return Ok(Accept);
    }

    match (action.shift, &action.reduces[..]) {
        (Some(next), []) => Ok(Shift(next)),
        (None, [reduce]) => Ok(Reduce(reduce.clone())),

        (Some(next), [reduce, remains @ ..]) => {
            let shift_prec = symbol.precedence();

            let reduce_prec = reduce.precedence();
            let resolved = resolve_shift_reduce_conflict(shift_prec, reduce_prec)?;

            if matches!(resolved, Some(false)) && !remains.is_empty() {
                return Err(ResolveError::ResolutionMismatched);
            }

            for reduce in remains {
                let reduce_prec = reduce.precedence();
                let new_resolved = resolve_shift_reduce_conflict(shift_prec, reduce_prec)?;
                if resolved != new_resolved {
                    return Err(ResolveError::ResolutionMismatched);
                }
            }

            match resolved {
                Some(true) => Ok(Shift(next)),
                Some(false) => Ok(Reduce(reduce.clone())),
                None => Ok(Fail),
            }
        }

        (None, [_, ..]) => Err(ResolveError::MultipleReduces),

        (None, []) => unreachable!(),
    }
}

fn resolve_shift_reduce_conflict(
    shift_prec: Option<&Precedence>,
    reduce_prec: Option<&Precedence>,
) -> Result<Option<bool>, ResolveError> {
    match (shift_prec, reduce_prec) {
        (Some(p1), Some(p2)) => match Ord::cmp(&p1.priority, &p2.priority) {
            Ordering::Greater => Ok(Some(true)),
            Ordering::Less => Ok(Some(false)),
            Ordering::Equal => match p1.assoc {
                Assoc::Left => Ok(Some(false)),
                Assoc::Right => Ok(Some(true)),
                Assoc::Nonassoc => Ok(None),
            },
        },
        _ => return Err(ResolveError::MissingPrecedence), // 比較不可能
    }
}

#[derive(Debug)]
pub(crate) enum ResolvedAction {
    Shift(NodeID),
    Reduce(Rule),
    Accept,
    Fail,
}

enum ResolveError {
    MultipleReduces,
    MissingPrecedence,
    ResolutionMismatched,
}
