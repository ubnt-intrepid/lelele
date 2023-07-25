//! An IELR(1) implementation.

pub mod annotation;
pub mod digraph;
pub mod grammar;
pub mod lalr;
pub mod lr0;
pub mod split;

use self::{grammar::Grammar, lalr::LALRData, lr0::LR0Automaton};

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
