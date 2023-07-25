//! Context-Free Grammars.

use super::types::{Map, Set};
use std::fmt;

// ==== Context-Free Grammars =====

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TerminalID(u16);
impl TerminalID {
    pub const EOI: Self = Self(0);
    const OFFSET: u16 = 1;
}
impl fmt::Debug for TerminalID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &Self::EOI => write!(f, "T#End"),
            _ => write!(f, "T#{:03}", self.0),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct TerminalSet {
    inner: bit_set::BitSet,
}
impl TerminalSet {
    pub fn contains(&self, id: TerminalID) -> bool {
        self.inner.contains(id.0.into())
    }
    pub fn insert(&mut self, id: TerminalID) -> bool {
        self.inner.insert(id.0.into())
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
            .map(|raw| raw.try_into().map(TerminalID).unwrap())
    }
}
impl FromIterator<TerminalID> for TerminalSet {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = TerminalID>,
    {
        Self {
            inner: iter.into_iter().map(|t| t.0.into()).collect(),
        }
    }
}
impl super::digraph::Set for TerminalSet {
    fn union_with(&mut self, other: &Self) {
        self.union_with(other)
    }
}

#[derive(Debug)]
pub struct TerminalData {
    pub name: String,
    pub precedence: Option<Precedence>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct NonterminalID(u16);
impl NonterminalID {
    pub const START: Self = Self(0);
    const OFFSET: u16 = 1;

    pub const fn from_raw(raw: u16) -> Self {
        debug_assert!(raw >= Self::OFFSET);
        Self(raw)
    }
}
impl fmt::Debug for NonterminalID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &Self::START => write!(f, "N#Start"),
            _ => write!(f, "N#{:03}", self.0),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum SymbolID {
    T(TerminalID),
    N(NonterminalID),
}
impl fmt::Debug for SymbolID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::T(t) => write!(f, "{:?}", t),
            Self::N(n) => write!(f, "{:?}", n),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ProductionID(u16);
impl ProductionID {
    pub const ACCEPT: Self = Self(u16::MAX);
}
impl fmt::Debug for ProductionID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &Self::ACCEPT => write!(f, "P#Accept"),
            _ => write!(f, "P#{:03}", self.0),
        }
    }
}

#[derive(Debug)]
pub struct Production {
    pub left: NonterminalID,
    pub right: Vec<SymbolID>,
    pub precedence: Option<Precedence>,
}
impl Production {
    pub fn display<'g>(&'g self, g: &'g Grammar) -> impl fmt::Display + 'g {
        crate::util::display_fn(|f| {
            write!(f, "{} -> ", g.nonterminals[&self.left])?;
            if self.right.is_empty() {
                f.write_str("ε")?;
            } else {
                for (i, r) in self.right.iter().enumerate() {
                    if i > 0 {
                        f.write_str(" ")?;
                    }
                    match r {
                        SymbolID::N(n) => f.write_str(&*g.nonterminals[n])?,
                        SymbolID::T(t) => f.write_str(&*g.terminals[t].name)?,
                    }
                }
            }
            Ok(())
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Precedence {
    pub priority: u16,
    pub assoc: Assoc,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Assoc {
    Left,
    Right,
    Nonassoc,
}

#[derive(Debug)]
#[non_exhaustive]
pub struct Grammar {
    pub terminals: Map<TerminalID, TerminalData>,
    pub nonterminals: Map<NonterminalID, String>,
    pub productions: Map<ProductionID, Production>,
    pub start_symbol: NonterminalID,
    pub nullables: Set<NonterminalID>,
}

impl Grammar {
    pub fn define<F>(g: F) -> Self
    where
        F: FnOnce(&mut GrammarDef),
    {
        let mut def = GrammarDef {
            terminals: Map::default(),
            nonterminals: Map::default(),
            productions: Map::default(),
            start_symbol: None,
            next_terminal: TerminalID::OFFSET,
            next_nonterminal: NonterminalID::OFFSET,
            next_production: 0,
        };

        def.terminals.insert(
            TerminalID::EOI,
            TerminalData {
                name: "#EOI".into(),
                precedence: None,
            },
        );
        def.nonterminals
            .insert(NonterminalID::START, "#Start".into());

        g(&mut def);

        let start_symbol = def
            .start_symbol
            .or_else(|| def.nonterminals.first().map(|(k, _v)| *k))
            .expect("The start symbol is not specified");

        def.productions.insert(
            ProductionID::ACCEPT,
            Production {
                left: NonterminalID::START,
                right: vec![SymbolID::N(start_symbol), SymbolID::T(TerminalID::EOI)],
                precedence: None,
            },
        );

        let mut nullables = Set::default();
        loop {
            let mut changed = false;
            for p in def.productions.values() {
                if p.right
                    .iter()
                    .all(|s| matches!(s, SymbolID::N(n) if nullables.contains(n)))
                {
                    changed |= nullables.insert(p.left);
                }
            }
            if !changed {
                break;
            }
        }

        Self {
            terminals: def.terminals,
            nonterminals: def.nonterminals,
            productions: def.productions,
            start_symbol,
            nullables,
        }
    }

    pub fn production(&self, id: ProductionID) -> &Production {
        &self.productions[&id]
    }
}

impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#### terminals: ")?;
        for (i, t) in self.terminals.values().enumerate() {
            if i > 0 {
                f.write_str(", ")?;
            }
            write!(f, "{}", t.name)?;
        }
        write!(f, "\n#### nonterminals: ")?;
        for (i, n) in self.nonterminals.values().enumerate() {
            if i > 0 {
                f.write_str(", ")?;
            }
            write!(f, "{}", n)?;
        }
        writeln!(f, "\n#### productions:")?;
        for p in self.productions.values() {
            writeln!(f, "- {}", p.display(self))?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct GrammarDef {
    terminals: Map<TerminalID, TerminalData>,
    nonterminals: Map<NonterminalID, String>,
    productions: Map<ProductionID, Production>,
    start_symbol: Option<NonterminalID>,
    next_terminal: u16,
    next_nonterminal: u16,
    next_production: u16,
}
impl GrammarDef {
    pub fn terminal(&mut self, precedence: Option<Precedence>, s: &str) -> TerminalID {
        let terminal = TerminalID(self.next_terminal);
        self.terminals.insert(
            terminal,
            TerminalData {
                name: s.into(),
                precedence,
            },
        );
        self.next_terminal += 1;
        terminal
    }

    pub fn nonterminal(&mut self, s: &str) -> NonterminalID {
        let nonterminal = NonterminalID(self.next_nonterminal);
        self.nonterminals.insert(nonterminal, s.into());
        self.next_nonterminal += 1;
        nonterminal
    }

    pub fn production<I>(&mut self, precedence: Option<Precedence>, left: NonterminalID, right: I)
    where
        I: IntoIterator<Item = SymbolID>,
    {
        let right: Vec<_> = right.into_iter().collect();
        for (_, p) in &self.productions {
            assert!(
                p.left != left || p.right != right,
                "production rule duplicated"
            );
        }

        let id = ProductionID(self.next_production);
        self.productions.insert(
            id,
            Production {
                left,
                right,
                precedence,
            },
        );
        self.next_production += 1;
    }

    pub fn start_symbol(&mut self, start: NonterminalID) {
        self.start_symbol.replace(start);
    }
}

pub mod examples {
    use super::*;
    use SymbolID::*;

    pub fn with_nullable(g: &mut GrammarDef) {
        // E → E + T n | T
        // T → a | ( E n ) | n a
        // n → ϵ | num

        let lparen = g.terminal(None, "`(`");
        let rparen = g.terminal(None, "`)`");
        let plus = g.terminal(None, "`+`");
        let a = g.terminal(None, "`-`");
        let num = g.terminal(None, "NUM");

        let expr = g.nonterminal("expr");
        let term = g.nonterminal("term");
        let nullable = g.nonterminal("nullable");

        g.production(None, expr, [N(expr), T(plus), N(nullable)]);
        g.production(None, expr, [N(term)]);

        g.production(None, term, [T(a)]);
        g.production(None, term, [T(lparen), N(expr), N(nullable), T(rparen)]);
        g.production(None, term, [N(nullable), T(a)]);

        g.production(None, nullable, []);
        g.production(None, nullable, [T(num)]);
    }

    pub fn arithmetic(g: &mut GrammarDef) {
        let lparen = g.terminal(None, "`(`");
        let rparen = g.terminal(None, "`)`");
        let plus = g.terminal(None, "`+`");
        let minus = g.terminal(None, "`-`");
        let star = g.terminal(None, "`*`");
        let slash = g.terminal(None, "`/`");
        let num = g.terminal(None, "NUM");

        let expr = g.nonterminal("expr");
        let term = g.nonterminal("term");
        let factor = g.nonterminal("factor");
        let atom = g.nonterminal("atom");

        g.start_symbol(expr);

        g.production(None, expr, [N(expr), T(plus), N(term)]);
        g.production(None, expr, [N(expr), T(minus), N(term)]);
        g.production(None, expr, [N(term)]);

        g.production(None, term, [N(term), T(star), N(factor)]);
        g.production(None, term, [N(term), T(slash), N(factor)]);
        g.production(None, term, [N(factor)]);

        g.production(None, factor, [T(minus), N(factor)]);
        g.production(None, factor, [N(atom)]);

        g.production(None, atom, [T(num)]);
        g.production(None, atom, [T(lparen), N(expr), T(rparen)]);
    }

    pub fn arithmetic2(g: &mut GrammarDef) {
        let prec_add = Some(Precedence {
            priority: 0,
            assoc: Assoc::Left,
        });
        let prec_mul = Some(Precedence {
            priority: 1,
            assoc: Assoc::Left,
        });
        let prec_neg = Some(Precedence {
            priority: 2,
            assoc: Assoc::Right,
        });

        let lparen = g.terminal(None, "`(`");
        let rparen = g.terminal(None, "`)`");
        let plus = g.terminal(prec_add, "`+`");
        let minus = g.terminal(prec_add, "`-`");
        let star = g.terminal(prec_mul, "`*`");
        let slash = g.terminal(prec_mul, "`/`");
        let num = g.terminal(None, "NUM");

        let expr = g.nonterminal("expr");

        g.start_symbol(expr);

        g.production(None, expr, [N(expr), T(plus), N(expr)]);
        g.production(None, expr, [N(expr), T(minus), N(expr)]);
        g.production(None, expr, [N(expr), T(star), N(expr)]);
        g.production(None, expr, [N(expr), T(slash), N(expr)]);
        g.production(prec_neg, expr, [T(minus), N(expr)]);
        g.production(None, expr, [T(num)]);
        g.production(None, expr, [T(lparen), N(expr), T(rparen)]);
    }

    pub fn min_caml(g: &mut GrammarDef) {
        // TODO: add precedence.

        let lparen = g.terminal(None, "`(`");
        let rparen = g.terminal(None, "`)`");
        let r#true = g.terminal(None, "`true`");
        let r#false = g.terminal(None, "`false`");
        let not = g.terminal(None, "`not`");
        let r#if = g.terminal(None, "`if`");
        let then = g.terminal(None, "`then`");
        let r#else = g.terminal(None, "`else`");
        let r#let = g.terminal(None, "`let`");
        let rec = g.terminal(None, "`rec`");
        let r#in = g.terminal(None, "`in`");
        let dot = g.terminal(None, "`.`");
        let less_minus = g.terminal(None, "`<-`");
        let comma = g.terminal(None, "`,`");
        let semicolon = g.terminal(None, "`;`");
        let array_make = g.terminal(None, "`Array.make`");

        let integer = g.terminal(None, "INT");
        let float = g.terminal(None, "FLOAT");
        let ident = g.terminal(None, "IDENT");

        let plus = g.terminal(None, "`+`");
        let plus_dot = g.terminal(None, "`+.`");
        let minus = g.terminal(None, "`-`");
        let minus_dot = g.terminal(None, "`-.`");
        let star_dot = g.terminal(None, "`*.`");
        let slash_dot = g.terminal(None, "`/.`");

        let equal = g.terminal(None, "`=`");
        let less_greater = g.terminal(None, "`<>`");
        let less = g.terminal(None, "`<`");
        let greater = g.terminal(None, "`>`");
        let less_equal = g.terminal(None, "`<=`");
        let greater_equal = g.terminal(None, "`>=`");

        let simple_exp = g.nonterminal("simple_exp");
        let exp = g.nonterminal("exp");
        let fundef = g.nonterminal("fundef");
        let formal_args = g.nonterminal("formal_args");
        let actual_args = g.nonterminal("actual_args");
        let elems = g.nonterminal("elems");
        let pat = g.nonterminal("pat");

        g.start_symbol(exp);

        g.production(None, simple_exp, [T(lparen), N(exp), T(rparen)]);
        g.production(None, simple_exp, [T(lparen), T(rparen)]);
        g.production(None, simple_exp, [T(r#true)]);
        g.production(None, simple_exp, [T(r#false)]);
        g.production(None, simple_exp, [T(integer)]);
        g.production(None, simple_exp, [T(float)]);
        g.production(None, simple_exp, [T(ident)]);
        g.production(
            None,
            simple_exp,
            [N(simple_exp), T(dot), T(lparen), N(exp), T(rparen)],
        );

        g.production(None, exp, [N(simple_exp)]);
        g.production(None, exp, [T(not), N(exp)]);
        g.production(None, exp, [T(minus), N(exp)]);
        g.production(None, exp, [T(minus_dot), N(exp)]);
        g.production(None, exp, [N(exp), T(plus), N(exp)]);
        g.production(None, exp, [N(exp), T(minus), N(exp)]);
        g.production(None, exp, [N(exp), T(plus_dot), N(exp)]);
        g.production(None, exp, [N(exp), T(minus_dot), N(exp)]);
        g.production(None, exp, [N(exp), T(star_dot), N(exp)]);
        g.production(None, exp, [N(exp), T(slash_dot), N(exp)]);
        g.production(None, exp, [N(exp), T(equal), N(exp)]);
        g.production(None, exp, [N(exp), T(less_greater), N(exp)]);
        g.production(None, exp, [N(exp), T(less), N(exp)]);
        g.production(None, exp, [N(exp), T(greater), N(exp)]);
        g.production(None, exp, [N(exp), T(less_equal), N(exp)]);
        g.production(None, exp, [N(exp), T(greater_equal), N(exp)]);
        g.production(
            None,
            exp,
            [T(r#if), N(exp), T(then), N(exp), T(r#else), N(exp)],
        );
        g.production(
            None,
            exp,
            [T(r#let), T(ident), T(equal), N(exp), T(r#in), N(exp)],
        );
        g.production(None, exp, [T(r#let), T(rec), N(fundef), T(r#in), N(exp)]);
        g.production(
            None,
            exp,
            [
                T(r#let),
                T(lparen),
                N(pat),
                T(rparen),
                T(equal),
                N(exp),
                T(r#in),
                N(exp),
            ],
        );
        g.production(None, exp, [N(simple_exp), N(actual_args)]);
        g.production(None, exp, [N(elems)]);
        g.production(
            None,
            exp,
            [
                N(simple_exp),
                T(dot),
                T(lparen),
                N(exp),
                T(rparen),
                T(less_minus),
                N(exp),
            ],
        );
        g.production(None, exp, [N(exp), T(semicolon), N(exp)]);
        g.production(None, exp, [T(array_make), N(simple_exp), N(simple_exp)]);

        g.production(None, fundef, [T(ident), N(formal_args), T(equal), N(exp)]);

        g.production(None, formal_args, [T(ident), N(formal_args)]);
        g.production(None, formal_args, [T(ident)]);

        g.production(None, actual_args, [N(actual_args), N(simple_exp)]);
        g.production(None, actual_args, [N(simple_exp)]);

        g.production(None, elems, [N(elems), T(comma), N(exp)]);
        g.production(None, elems, [N(exp), T(comma), N(exp)]);

        g.production(None, pat, [N(pat), T(comma), T(ident)]);
        g.production(None, pat, [T(ident), T(comma), T(ident)]);
    }

    // test grammars described in the IELR(1) paper.

    #[allow(nonstandard_style)]
    pub fn fig1(g: &mut GrammarDef) {
        // Note: this grammar is unambiguous, but the generated LALR(1) automaton has S/R conflict.
        let a = g.terminal(None, "a");
        let b = g.terminal(None, "b");
        let S = g.nonterminal("S");
        let A = g.nonterminal("A");

        g.start_symbol(S);

        g.production(None, S, [T(a), N(A), T(a)]);
        g.production(None, S, [T(b), N(A), T(b)]);
        g.production(None, A, [T(a)]);
        g.production(None, A, [T(a), T(a)]);
    }

    #[allow(nonstandard_style)]
    pub fn fig2(g: &mut GrammarDef) {
        // Note: this grammar is ambiguous, and both of canonical LR(1) and LALR(1) automaton have conflict(s).
        let a = g.terminal(None, "a");
        let b = g.terminal(None, "b");
        let c = g.terminal(None, "c");

        let S = g.nonterminal("S");
        let A = g.nonterminal("A");
        let B = g.nonterminal("B");
        let C = g.nonterminal("C");

        g.start_symbol(S);

        g.production(None, S, [T(a), N(A), T(a)]);
        g.production(None, S, [T(a), N(B), T(b)]);
        g.production(None, S, [T(a), N(C), T(c)]);
        g.production(None, S, [T(b), N(A), T(b)]);
        g.production(None, S, [T(b), N(B), T(a)]);
        g.production(None, S, [T(b), N(C), T(a)]);

        g.production(None, A, [T(a), T(a)]);
        g.production(None, B, [T(a), T(a)]);
        g.production(None, C, [T(a), T(a)]);
    }

    #[allow(nonstandard_style)]
    pub fn fig3(g: &mut GrammarDef) {
        let a = g.terminal(None, "a");
        let b = g.terminal(None, "b");

        let S = g.nonterminal("S");
        let A = g.nonterminal("A");
        let B = g.nonterminal("B");
        let C = g.nonterminal("C");

        g.start_symbol(S);

        g.production(None, S, [T(a), N(A), T(a)]);
        g.production(None, S, [T(a), N(B), T(a)]);
        g.production(None, S, [T(a), N(C), T(a)]);
        g.production(None, S, [T(b), N(A), T(b)]);
        g.production(None, S, [T(b), N(B), T(a)]);
        g.production(None, S, [T(b), N(C), T(a)]);

        g.production(None, A, [T(a), T(a)]);
        g.production(None, B, [T(a), T(a)]);
        g.production(None, C, [T(a), T(a)]);
    }

    #[allow(nonstandard_style)]
    pub fn fig4(g: &mut GrammarDef) {
        let a = g.terminal(None, "a");
        let b = g.terminal(None, "b");

        let S = g.nonterminal("S");
        let A = g.nonterminal("A");
        let B = g.nonterminal("B");

        g.start_symbol(S);

        g.production(None, S, [T(a), N(A), T(a)]);
        g.production(None, S, [T(a), N(A), T(b)]);
        g.production(None, S, [T(a), N(B), T(a)]);
        g.production(None, S, [T(b), N(A), T(a)]);
        g.production(None, S, [T(b), N(B), T(b)]);
        g.production(None, A, [T(a)]);
        g.production(None, B, [T(a)]);
    }

    #[allow(nonstandard_style)]
    pub fn fig5(g: &mut GrammarDef) {
        let prec_shift = Some(Precedence {
            priority: 0,
            assoc: Assoc::Left,
        });
        let prec_reduce = Some(Precedence {
            priority: 1,
            assoc: Assoc::Right,
        });

        let a = g.terminal(prec_shift, "a");
        let b = g.terminal(prec_shift, "b");
        let c = g.terminal(prec_shift, "c");

        let S = g.nonterminal("S");
        let A = g.nonterminal("A");
        let B = g.nonterminal("B");
        let C = g.nonterminal("C");
        let D = g.nonterminal("D");
        let E = g.nonterminal("E");

        g.start_symbol(S);

        g.production(prec_reduce, S, [T(a), N(A), N(B), T(a)]);
        g.production(prec_reduce, S, [T(b), N(A), N(B), T(b)]);
        g.production(prec_reduce, A, [T(a), N(C), N(D), N(E)]);
        g.production(prec_reduce, B, [T(c)]);
        g.production(prec_reduce, B, []);
        g.production(prec_reduce, C, [N(D)]);
        g.production(prec_reduce, D, [T(a)]);
        g.production(prec_reduce, E, [T(a)]);
        g.production(prec_reduce, E, []);
    }

    #[allow(nonstandard_style)]
    pub fn fig6(g: &mut GrammarDef) {
        let a = g.terminal(None, "a");
        let b = g.terminal(None, "b");
        let S = g.nonterminal("S");
        let A = g.nonterminal("A");
        let B = g.nonterminal("B");
        let C = g.nonterminal("C");
        let D = g.nonterminal("D");

        g.start_symbol(S);

        g.production(None, S, [T(a), N(A), T(a)]);
        g.production(None, S, [T(a), T(a), T(b)]);
        g.production(None, S, [T(b), N(A), T(b)]);
        g.production(None, A, [N(B), N(C)]);
        g.production(None, B, [T(a)]);
        g.production(None, C, [N(D)]);
        g.production(None, D, []);
    }
}
