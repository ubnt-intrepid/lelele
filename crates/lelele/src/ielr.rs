//! The implementation of IELR(1) method and related algorithms.

pub mod annotation;
pub mod cfg;
pub mod digraph;
pub mod lalr;
pub mod lr0;
pub mod reachability;
pub mod split;
pub mod table;

use self::{
    cfg::{Grammar, TerminalID},
    lalr::LASet,
    lr0::LR0Automaton,
    table::ParseTable,
};

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

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Mode {
    LALR,
    #[default]
    IELR,
    // Canonical,
}

/// Compute the IELR(1) automaton from the specified grammar.
pub fn compute(g: &Grammar, mode: Mode) -> Result<ParseTable, table::DFAError> {
    let (lr0, lalr) = compute_automaton(g, mode);
    table::generate(g, &lr0, &lalr)
}

fn compute_automaton(g: &Grammar, mode: Mode) -> (LR0Automaton, LASet) {
    let lr0 = lr0::lr0(&g);
    let la_set = lalr::lalr(&g, &lr0);
    if mode == Mode::LALR {
        return (lr0, la_set);
    }

    let annotation_list = annotation::annotation_lists(&g, &lr0, &la_set);
    if annotation_list
        .annotations
        .iter()
        .all(|(_id, annotations)| annotations.is_empty())
    {
        // The grammar is LALR(1) and hence state state splitting is not required.
        return (lr0, la_set);
    }

    let ielr = split::split_states(&g, &lr0, &la_set, &annotation_list);
    let ielr_lookaheads = lalr::lalr(&g, &ielr);

    (ielr, ielr_lookaheads)
}

#[cfg(test)]
mod tests {
    use super::cfg::{GrammarDef, SymbolID::*};
    use super::*;

    #[test]
    fn smoketest1() {
        let mut def = GrammarDef::default();

        let equal = def.terminal(None);
        let plus = def.terminal(None);
        let ident = def.terminal(None);
        let num = def.terminal(None);

        let a = def.nonterminal();
        let e = def.nonterminal();
        let t = def.nonterminal();

        def.start_symbol(a);

        def.rule(a, [N(e), T(equal), N(e)], None);
        def.rule(a, [T(ident)], None);
        def.rule(e, [N(e), T(plus), N(t)], None);
        def.rule(e, [N(t)], None);
        def.rule(t, [T(num)], None);
        def.rule(t, [T(ident)], None);

        let grammar = def.end();

        let _table = compute(&grammar, Default::default()).unwrap();
    }

    #[test]
    fn smoketest2() {
        let mut g = GrammarDef::default();

        // declare terminal symbols.
        let lparen = g.terminal(None);
        let rparen = g.terminal(None);
        let plus = g.terminal(None);
        let minus = g.terminal(None);
        let star = g.terminal(None);
        let slash = g.terminal(None);
        let num = g.terminal(None);
        let _ = g.terminal(None);

        // declare nonterminal symbols.
        let expr = g.nonterminal();
        let term = g.nonterminal();
        let factor = g.nonterminal();
        let _ = g.nonterminal();

        // declare syntax rules.
        g.rule(expr, [N(expr), T(plus), N(term)], None);
        g.rule(expr, [N(expr), T(minus), N(term)], None);
        g.rule(expr, [N(term)], None);

        g.rule(term, [N(term), T(star), N(factor)], None);
        g.rule(term, [N(term), T(slash), N(factor)], None);
        g.rule(term, [N(factor)], None);

        g.rule(factor, [T(num)], None);
        g.rule(factor, [T(lparen), N(expr), T(rparen)], None);

        g.start_symbol(expr);

        let grammar = g.end();
        let _table = compute(&grammar, Default::default()).unwrap();
    }
}
