//! An IELR(1) implementation.

pub mod annotation;
pub mod digraph;
pub mod lalr;
pub mod lr0;
pub mod split;

use self::{lalr::LALRData, lr0::LR0Automaton};
use crate::grammar::{Grammar, TerminalID};

#[derive(Debug, Default, Clone)]
pub struct TerminalSet {
    inner: bit_set::BitSet,
}

impl TerminalSet {
    pub fn contains(&self, id: TerminalID) -> bool {
        self.inner.contains(id.into_raw().into())
    }
    pub fn insert(&mut self, id: TerminalID) -> bool {
        self.inner.insert(id.into_raw().into())
    }
    pub fn union_with(&mut self, other: &Self) {
        self.inner.union_with(&other.inner)
    }
    pub fn intersect_with(&mut self, other: &Self) {
        self.inner.intersect_with(&other.inner)
    }
    pub fn difference_with(&mut self, other: &Self) {
        self.inner.difference_with(&other.inner)
    }
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
    pub fn iter(&self) -> impl Iterator<Item = TerminalID> + '_ {
        self.inner
            .iter()
            .map(|raw| raw.try_into().map(TerminalID::from_raw).unwrap())
    }
}

impl FromIterator<TerminalID> for TerminalSet {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = TerminalID>,
    {
        Self {
            inner: iter.into_iter().map(|t| t.into_raw().into()).collect(),
        }
    }
}
impl digraph::Set for TerminalSet {
    fn union_with(&mut self, other: &Self) {
        self.union_with(other)
    }
}

/// Compute the IELR(1) automaton from the specified grammar.
pub fn compute(g: &Grammar) -> (LR0Automaton, LALRData) {
    let lr0 = lr0::lr0(&g);

    let lalr_lookaheads = lalr::lalr(&g, &lr0);
    let follow_kernel_items = annotation::follow_kernel_items(&g, &lr0, &lalr_lookaheads);
    let annotation_lists =
        annotation::annotation_lists(&g, &lr0, &lalr_lookaheads, &follow_kernel_items);

    if annotation_lists
        .iter()
        .all(|(_id, annotations)| annotations.is_empty())
    {
        // The grammar is LALR(1) and hence state state splitting is not required.
        return (lr0, lalr_lookaheads);
    }

    let ielr = split::split_states(
        &g,
        &lr0,
        &lalr_lookaheads,
        &follow_kernel_items,
        &annotation_lists,
    );
    let ielr_lookaheads = lalr::lalr(&g, &ielr);
    (ielr, ielr_lookaheads)
}
