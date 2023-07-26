//! The implementation of IELR(1) method and related algorithms.

pub mod annotation;
pub mod digraph;
pub mod lalr;
pub mod lr0;
pub mod reachability;
pub mod split;
pub mod table;

use self::{lalr::LALRData, lr0::LR0Automaton, table::ParseTable};
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
pub fn compute(g: &Grammar) -> Result<ParseTable, table::DFAError> {
    let (lr0, lalr) = compute_automaton(g);
    table::generate(g, &lr0, &lalr)
}

fn compute_automaton(g: &Grammar) -> (LR0Automaton, LALRData) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::SymbolID::*;

    #[test]
    fn smoketest1() {
        let grammar = Grammar::define(|def| {
            let equal = def.terminal("EQUAL", None)?;
            let plus = def.terminal("PLUS", None)?;
            let ident = def.terminal("ID", None)?;
            let num = def.terminal("NUM", None)?;

            let a = def.nonterminal("A")?;
            let e = def.nonterminal("E")?;
            let t = def.nonterminal("T")?;

            def.start_symbol(a)?;

            def.rule(a, [N(e), T(equal), N(e)], None)?;
            def.rule(a, [T(ident)], None)?;
            def.rule(e, [N(e), T(plus), N(t)], None)?;
            def.rule(e, [N(t)], None)?;
            def.rule(t, [T(num)], None)?;
            def.rule(t, [T(ident)], None)?;

            Ok(())
        })
        .unwrap();
        eprintln!("{}", grammar);

        let table = compute(&grammar).unwrap();
        eprintln!("table:\n---\n{}", table.display(&grammar));
    }

    #[test]
    fn smoketest2() {
        let grammar = Grammar::define(|g| {
            // declare terminal symbols.
            let lparen = g.terminal("LPAREN", None)?;
            let rparen = g.terminal("RPAREN", None)?;
            let plus = g.terminal("PLUS", None)?;
            let minus = g.terminal("MINUS", None)?;
            let star = g.terminal("STAR", None)?;
            let slash = g.terminal("SLASH", None)?;
            let num = g.terminal("NUM", None)?;
            let _ = g.terminal("UNUSED_0", None)?;

            // declare nonterminal symbols.
            let expr = g.nonterminal("EXPR")?;
            let term = g.nonterminal("TERM")?;
            let factor = g.nonterminal("FACTOR")?;
            let _ = g.nonterminal("UNUSED_1")?;

            // declare syntax rules.
            g.rule(expr, [N(expr), T(plus), N(term)], None)?;
            g.rule(expr, [N(expr), T(minus), N(term)], None)?;
            g.rule(expr, [N(term)], None)?;

            g.rule(term, [N(term), T(star), N(factor)], None)?;
            g.rule(term, [N(term), T(slash), N(factor)], None)?;
            g.rule(term, [N(factor)], None)?;

            g.rule(factor, [T(num)], None)?;
            g.rule(factor, [T(lparen), N(expr), T(rparen)], None)?;

            g.start_symbol(expr)?;

            Ok(())
        })
        .unwrap();
        eprintln!("{}", grammar);

        let table = compute(&grammar).unwrap();
        eprintln!("table:\n---\n{}", table.display(&grammar));
    }
}
