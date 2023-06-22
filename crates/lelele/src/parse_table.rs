use crate::{
    dfa::{Action, NodeID, DFA},
    grammar::{Assoc, Grammar, Precedence, RuleID, SymbolID},
    IndexMap,
};
use std::cmp::Ordering;

#[derive(Debug)]
pub struct ParseTable {
    pub(crate) map: IndexMap<NodeID, IndexMap<SymbolID, ResolvedAction>>,
}

impl ParseTable {
    pub fn generate(grammar: &Grammar, dfa: &DFA) -> Self {
        let mut map = IndexMap::default();

        for (node_id, node) in dfa.nodes() {
            let mut actions = IndexMap::<SymbolID, ResolvedAction>::default();

            for (symbol_id, action) in node.actions() {
                // FIXME: report conflict
                if let Ok(resolved) = resolve_action(grammar, symbol_id, action) {
                    actions.insert(symbol_id, resolved);
                }
            }

            map.insert(node_id, actions);
        }

        Self { map }
    }
}

/// Attempts to resolve shift/reduce conflicts based on precedence/associativity.
fn resolve_action(
    grammar: &Grammar,
    symbol_id: SymbolID,
    action: &Action,
) -> Result<ResolvedAction, ResolveError> {
    use ResolvedAction::*;

    if action.accepted {
        debug_assert!(action.shift.is_none(), "detect shift/accept conflict");
        debug_assert!(action.reduces.is_empty(), "detect reduce/accept conflict");
        return Ok(Accept);
    }

    match (action.shift, &action.reduces[..]) {
        (Some(next), []) => Ok(Shift(next)),
        (None, [reduce]) => Ok(Reduce(*reduce)),

        (Some(next), [reduce, remains @ ..]) => {
            let shift_prec = grammar.symbol(symbol_id).precedence();

            let reduce_prec = grammar.rule(*reduce).precedence(grammar);
            let resolved = resolve_shift_reduce_conflict(shift_prec, reduce_prec)?;

            if matches!(resolved, Some(false)) && !remains.is_empty() {
                return Err(ResolveError::ResolutionMismatched);
            }

            for reduce in remains {
                let reduce_prec = grammar.rule(*reduce).precedence(grammar);
                let new_resolved = resolve_shift_reduce_conflict(shift_prec, reduce_prec)?;
                if resolved != new_resolved {
                    return Err(ResolveError::ResolutionMismatched);
                }
            }

            match resolved {
                Some(true) => Ok(Shift(next)),
                Some(false) => Ok(Reduce(*reduce)),
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
    Reduce(RuleID),
    Accept,
    Fail,
}

enum ResolveError {
    MultipleReduces,
    MissingPrecedence,
    ResolutionMismatched,
}
