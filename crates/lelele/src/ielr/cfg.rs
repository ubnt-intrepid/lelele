use crate::types::{Map, Set};
use std::marker::PhantomData;

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

impl Grammar {
    /// Define a grammar using the specified function.
    pub fn define<F>(f: F) -> Result<Self, GrammarDefError>
    where
        F: FnOnce(&mut GrammarDef) -> Result<(), GrammarDefError>,
    {
        let mut def = GrammarDef {
            terminals: Map::default(),
            nonterminals: Map::default(),
            rules: Map::default(),
            start: None,
            next_terminal_id: TerminalID::OFFSET,
            next_nonterminal_id: NonterminalID::OFFSET,
            next_rule_id: RuleID::OFFSET,
            _marker: PhantomData,
        };

        def.terminals
            .insert(TerminalID::EOI, Terminal { precedence: None });
        def.terminals
            .insert(TerminalID::ERROR, Terminal { precedence: None });

        def.nonterminals
            .insert(NonterminalID::START, Nonterminal {});

        f(&mut def)?;

        def.end()
    }
}

/// The contextural values for building a `Grammar`.
#[derive(Debug)]
pub struct GrammarDef<'def> {
    terminals: Map<TerminalID, Terminal>,
    nonterminals: Map<NonterminalID, Nonterminal>,
    rules: Map<RuleID, Rule>,
    start: Option<NonterminalID>,
    next_terminal_id: u16,
    next_nonterminal_id: u16,
    next_rule_id: u16,
    _marker: PhantomData<&'def mut ()>,
}

impl<'def> GrammarDef<'def> {
    /// Declare a terminal symbol used in this grammar.
    pub fn terminal(
        &mut self,
        precedence: Option<Precedence>,
    ) -> Result<TerminalID, GrammarDefError> {
        let id = TerminalID::from_raw(self.next_terminal_id);
        self.next_terminal_id += 1;
        self.terminals.insert(id, Terminal { precedence });
        Ok(id)
    }

    /// Declare a nonterminal symbol used in this grammar.
    pub fn nonterminal(&mut self) -> Result<NonterminalID, GrammarDefError> {
        let id = NonterminalID::new(self.next_nonterminal_id);
        self.next_nonterminal_id += 1;
        self.nonterminals.insert(id, Nonterminal {});
        Ok(id)
    }

    /// Specify a production rule into this grammer.
    pub fn rule<I>(
        &mut self,
        left: NonterminalID,
        right: I,
        precedence: Option<Precedence>,
    ) -> Result<RuleID, GrammarDefError>
    where
        I: IntoIterator<Item = SymbolID>,
    {
        let right_ = right.into_iter().collect();
        for rule in self.rules.values() {
            if rule.left == left && rule.right == right_ {
                return Err(GrammarDefError::Other {
                    msg: "Duplicate production rule detected".into(),
                });
            }
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
        Ok(id)
    }

    /// Specify the start symbol for this grammar.
    pub fn start_symbol(&mut self, symbol: NonterminalID) -> Result<(), GrammarDefError> {
        self.start.replace(symbol);
        Ok(())
    }

    fn end(mut self) -> Result<Grammar, GrammarDefError> {
        // start symbolのID変換
        // 指定されていない場合は最初に登録されたnonterminal symbolを用いる
        let start = match self.start.take() {
            Some(start) => start,
            None => self
                .nonterminals
                .keys()
                .find(|id| **id != NonterminalID::START)
                .copied()
                .ok_or_else(|| GrammarDefError::Other {
                    msg: "empty nonterminal symbols".into(),
                })?,
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

        Ok(Grammar {
            terminals: self.terminals,
            nonterminals: self.nonterminals,
            rules: self.rules,
            start_symbol: start,
            nullables,
        })
    }
}

#[derive(Debug, thiserror::Error)]
pub enum GrammarDefError {
    #[error("Other error: {}", msg)]
    Other { msg: String },
}
impl From<&str> for GrammarDefError {
    fn from(msg: &str) -> Self {
        Self::Other { msg: msg.into() }
    }
}
impl From<String> for GrammarDefError {
    fn from(msg: String) -> Self {
        Self::Other { msg }
    }
}
