use crate::types::{Map, Set};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct TerminalID {
    raw: u16,
}

impl TerminalID {
    pub const EOI: Self = Self::from_raw(0);
    pub const ERROR: Self = Self::from_raw(1);

    const OFFSET: u16 = 2;

    #[inline]
    pub const fn from_raw(raw: u16) -> Self {
        Self { raw }
    }

    #[inline]
    pub const fn into_raw(self) -> u16 {
        self.raw
    }
}

#[derive(Debug, Default, Clone)]
pub struct TerminalIDSet {
    inner: bit_set::BitSet,
}

impl TerminalIDSet {
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

impl FromIterator<TerminalID> for TerminalIDSet {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = TerminalID>,
    {
        Self {
            inner: iter.into_iter().map(|t| t.into_raw().into()).collect(),
        }
    }
}

impl super::digraph::Set for TerminalIDSet {
    fn union_with(&mut self, other: &Self) {
        self.union_with(other)
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub struct Terminal {
    pub precedence: Option<Precedence>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct NonterminalID {
    raw: u16,
}

impl NonterminalID {
    pub const START: Self = Self::new(0);

    const OFFSET: u16 = 1;

    #[inline]
    const fn new(raw: u16) -> Self {
        Self { raw }
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub struct Nonterminal {}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum SymbolID {
    T(TerminalID),
    N(NonterminalID),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct RuleID {
    raw: u16,
}

impl RuleID {
    pub const ACCEPT: Self = Self::new(0);

    const OFFSET: u16 = 1;

    #[inline]
    const fn new(raw: u16) -> Self {
        Self { raw }
    }
}

/// The type that represents a production rule in grammar.
#[derive(Debug)]
#[non_exhaustive]
pub struct Rule {
    pub left: NonterminalID,
    pub right: Vec<SymbolID>,
    pub precedence: Option<Precedence>,
}

impl Rule {
    pub fn precedence(&self, g: &Grammar) -> Option<Precedence> {
        match self.precedence {
            Some(prec) => Some(prec),
            None => {
                for symbol in self.right.iter().rev() {
                    if let SymbolID::T(t) = symbol {
                        return g.terminals[t].precedence;
                    }
                }
                None
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
#[non_exhaustive]
pub struct Precedence {
    pub priority: u16,
    pub assoc: Assoc,
}

impl Precedence {
    pub const fn new(priority: u16, assoc: Assoc) -> Self {
        Self { priority, assoc }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
#[non_exhaustive]
pub enum Assoc {
    Left,
    Right,
    Nonassoc,
}

/// The grammar definition used to derive the parser tables.
#[derive(Debug)]
#[non_exhaustive]
pub struct Grammar {
    pub terminals: Map<TerminalID, Terminal>,
    pub nonterminals: Map<NonterminalID, Nonterminal>,
    pub rules: Map<RuleID, Rule>,
    pub start_symbol: NonterminalID,
    pub nullables: Set<NonterminalID>,
}

/// The contextural values for building a `Grammar`.
#[derive(Debug)]
pub struct GrammarDef {
    terminals: Map<TerminalID, Terminal>,
    nonterminals: Map<NonterminalID, Nonterminal>,
    rules: Map<RuleID, Rule>,
    start: Option<NonterminalID>,
    next_terminal_id: u16,
    next_nonterminal_id: u16,
    next_rule_id: u16,
}

impl Default for GrammarDef {
    fn default() -> Self {
        let mut def = GrammarDef {
            terminals: Map::default(),
            nonterminals: Map::default(),
            rules: Map::default(),
            start: None,
            next_terminal_id: TerminalID::OFFSET,
            next_nonterminal_id: NonterminalID::OFFSET,
            next_rule_id: RuleID::OFFSET,
        };

        def.terminals
            .insert(TerminalID::EOI, Terminal { precedence: None });
        def.terminals
            .insert(TerminalID::ERROR, Terminal { precedence: None });

        def.nonterminals
            .insert(NonterminalID::START, Nonterminal {});

        def
    }
}

impl GrammarDef {
    /// Declare a terminal symbol used in this grammar.
    pub fn terminal(&mut self, precedence: Option<Precedence>) -> TerminalID {
        let id = TerminalID::from_raw(self.next_terminal_id);
        self.next_terminal_id += 1;
        self.terminals.insert(id, Terminal { precedence });
        id
    }

    /// Declare a nonterminal symbol used in this grammar.
    pub fn nonterminal(&mut self) -> NonterminalID {
        let id = NonterminalID::new(self.next_nonterminal_id);
        self.next_nonterminal_id += 1;
        self.nonterminals.insert(id, Nonterminal {});
        id
    }

    /// Specify a production rule into this grammer.
    pub fn rule<I>(
        &mut self,
        left: NonterminalID,
        right: I,
        precedence: Option<Precedence>,
    ) -> RuleID
    where
        I: IntoIterator<Item = SymbolID>,
    {
        let right_ = right.into_iter().collect();
        for rule in self.rules.values() {
            assert!(
                rule.left != left || rule.right != right_,
                "Duplicate production rule"
            );
        }
        let id = RuleID::new(self.next_rule_id);
        self.next_rule_id += 1;
        self.rules.insert(
            id,
            Rule {
                left,
                right: right_,
                precedence,
            },
        );
        id
    }

    /// Specify the start symbol for this grammar.
    pub fn start_symbol(&mut self, symbol: NonterminalID) {
        self.start.replace(symbol);
    }

    pub fn end(mut self) -> Grammar {
        // start symbolのID変換
        // 指定されていない場合は最初に登録されたnonterminal symbolを用いる
        let start = match self.start.take() {
            Some(start) => start,
            None => self
                .nonterminals
                .keys()
                .find(|id| **id != NonterminalID::START)
                .copied()
                .expect("empty nonterminal symbols"),
        };

        self.rules.insert(
            RuleID::ACCEPT,
            Rule {
                left: NonterminalID::START,
                right: vec![SymbolID::N(start), SymbolID::T(TerminalID::EOI)],
                precedence: None,
            },
        );

        let mut nullables = Set::default();
        loop {
            let mut changed = false;
            for p in self.rules.values() {
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

        Grammar {
            terminals: self.terminals,
            nonterminals: self.nonterminals,
            rules: self.rules,
            start_symbol: start,
            nullables,
        }
    }
}
