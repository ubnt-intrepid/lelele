//! Grammar types.

use crate::IndexSet;
use std::{
    borrow::{Borrow, Cow},
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
};

const TERMINAL_ID_OFFSET: u64 = 0x4;
const NONTERMINAL_ID_OFFSET: u64 = 0x4;
const RULE_ID_OFFSET: u64 = 0x4;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct TerminalID {
    raw: u64,
}
impl TerminalID {
    /// Reserved symbol used as a terminal symbol that means the end of input.
    pub(crate) const EOI: Self = Self::new(0);
    #[inline]
    const fn new(raw: u64) -> Self {
        assert!(raw < u64::MAX / 2, "too large TerminalID");
        Self { raw }
    }
    #[inline]
    pub(crate) const fn raw(self) -> u64 {
        self.raw
    }
}

#[derive(Debug)]
pub struct Terminal {
    id: TerminalID,
    export_name: Option<Cow<'static, str>>,
    precedence: Option<Precedence>,
}
impl Terminal {
    pub fn id(&self) -> TerminalID {
        self.id
    }
    pub fn export_name(&self) -> Option<&str> {
        self.export_name.as_deref()
    }

    pub fn precedence(&self) -> Option<&Precedence> {
        self.precedence.as_ref()
    }
}
impl fmt::Display for Terminal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.id {
            TerminalID::EOI => f.write_str("$eoi"),
            _ => f.write_str(self.export_name().unwrap_or("<unknown>")),
        }
    }
}
impl PartialEq for Terminal {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for Terminal {}
impl Hash for Terminal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
impl Borrow<TerminalID> for Terminal {
    #[inline]
    fn borrow(&self) -> &TerminalID {
        &self.id
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct NonterminalID {
    raw: u64,
}
impl NonterminalID {
    /// Reserved symbol used as a nonterminal symbol to match when the parsing is complete.
    pub(crate) const ACCEPT: Self = Self::new(1);
    #[inline]
    const fn new(raw: u64) -> Self {
        assert!(raw < u64::MAX / 2, "too large NonterminalID");
        Self { raw }
    }
    #[inline]
    pub(crate) const fn raw(self) -> u64 {
        self.raw
    }
}
#[derive(Debug)]
pub struct Nonterminal {
    id: NonterminalID,
    export_name: Option<Cow<'static, str>>,
}
impl Nonterminal {
    pub fn id(&self) -> NonterminalID {
        self.id
    }
    pub fn export_name(&self) -> Option<&str> {
        self.export_name.as_deref()
    }
}
impl fmt::Display for Nonterminal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.id {
            NonterminalID::ACCEPT => f.write_str("$accept"),
            _ => f.write_str(self.export_name().unwrap_or("<unknown>")),
        }
    }
}
impl PartialEq for Nonterminal {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for Nonterminal {}
impl Hash for Nonterminal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
impl Borrow<NonterminalID> for Nonterminal {
    fn borrow(&self) -> &NonterminalID {
        &self.id
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {
    T(TerminalID),
    N(NonterminalID),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct RuleID {
    raw: u64,
}
impl RuleID {
    /// Reserved ID that represents the top-level production rule:
    /// `$accept : <start-symbol> $eoi`.
    pub(crate) const ACCEPT: Self = Self::new(0);

    #[inline]
    const fn new(raw: u64) -> Self {
        assert!(raw < u64::MAX / 2, "too large RuleID");
        Self { raw }
    }
}

/// The type that represents a production rule in grammar.
#[derive(Debug)]
pub struct Rule {
    id: RuleID,
    left: NonterminalID,
    right: Vec<Symbol>,
    precedence: Option<Precedence>,
}
impl Rule {
    pub fn id(&self) -> RuleID {
        self.id
    }

    /// Return the left-hand side of this production.
    pub fn left(&self) -> NonterminalID {
        self.left
    }

    /// Return the right-hand side of this production.
    pub fn right(&self) -> &[Symbol] {
        &self.right[..]
    }

    pub fn precedence<'g>(&'g self, g: &'g Grammar) -> Option<&'g Precedence> {
        match self.precedence {
            Some(ref prec) => Some(prec),
            None => {
                for symbol in self.right.iter().rev() {
                    if let Symbol::T(t) = symbol {
                        return g.terminal(t).precedence();
                    }
                }
                None
            }
        }
    }
}
impl PartialEq for Rule {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for Rule {}
impl Hash for Rule {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
impl Borrow<RuleID> for Rule {
    fn borrow(&self) -> &RuleID {
        &self.id
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
#[non_exhaustive]
pub struct Precedence {
    pub priority: usize,
    pub assoc: Assoc,
}

impl Precedence {
    pub const fn new(priority: usize, assoc: Assoc) -> Self {
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

impl fmt::Display for Assoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Left => write!(f, "left"),
            Self::Right => write!(f, "right"),
            Self::Nonassoc => write!(f, "nonassoc"),
        }
    }
}

/// The grammar definition used to derive the parser tables.
#[derive(Debug)]
pub struct Grammar {
    terminals: IndexSet<Terminal>,
    nonterminals: IndexSet<Nonterminal>,
    rules: IndexSet<Rule>,
    start_symbol: NonterminalID,
    accept_rule: Rule,
}

impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "## terminals:")?;
        for terminal in self.terminals() {
            write!(f, "{}", terminal)?;
            if let Some(prec) = terminal.precedence() {
                write!(f, " (priority={}, assoc={})", prec.priority, prec.assoc)?;
            }
            writeln!(f)?;
        }

        writeln!(f, "\n## nonterminals:")?;
        for nonterminal in self.nonterminals() {
            write!(f, "{}", nonterminal)?;
            if nonterminal.id() == self.start_symbol {
                write!(f, " (start)")?;
            }
            writeln!(f)?;
        }

        writeln!(f, "\n## rules:")?;
        for rule in self.rules() {
            write!(f, "{} := ", self.nonterminal(&rule.left()))?;
            for (i, symbol) in rule.right().iter().enumerate() {
                if i > 0 {
                    write!(f, " ")?;
                }
                match symbol {
                    Symbol::T(t) => write!(f, "{}", self.terminal(t))?,
                    Symbol::N(n) => write!(f, "{}", self.nonterminal(n))?,
                }
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

impl Grammar {
    /// Define a grammar using the specified function.
    pub fn define<F>(f: F) -> Result<Self, GrammarDefError>
    where
        F: FnOnce(&mut GrammarDef) -> Result<(), GrammarDefError>,
    {
        let mut def = GrammarDef {
            terminals: IndexSet::default(),
            nonterminals: IndexSet::default(),
            rules: IndexSet::default(),
            start: None,
            next_terminal_id: TERMINAL_ID_OFFSET,
            next_nonterminal_id: NONTERMINAL_ID_OFFSET,
            next_rule_id: RULE_ID_OFFSET,
            _marker: PhantomData,
        };

        def.terminals.insert(Terminal {
            id: TerminalID::EOI,
            export_name: None,
            precedence: None,
        });
        def.nonterminals.insert(Nonterminal {
            id: NonterminalID::ACCEPT,
            export_name: None,
        });

        f(&mut def)?;

        def.end()
    }

    /// Returns the terminal symbol declared in this grammar.
    pub fn terminals(&self) -> impl Iterator<Item = &Terminal> + '_ {
        self.terminals.iter()
    }

    pub fn terminal<Q: ?Sized>(&self, key: &Q) -> &Terminal
    where
        Q: Borrow<TerminalID>,
    {
        self.terminals.get(key.borrow()).unwrap()
    }

    /// Returns the nonterminal symbol declared in this grammar.
    pub fn nonterminals(&self) -> impl Iterator<Item = &Nonterminal> + '_ {
        self.nonterminals.iter()
    }

    pub fn nonterminal<Q: ?Sized>(&self, key: &Q) -> &Nonterminal
    where
        Q: Borrow<NonterminalID>,
    {
        self.nonterminals.get(key.borrow()).unwrap()
    }

    pub fn rules(&self) -> impl Iterator<Item = &Rule> + '_ {
        Some(&self.accept_rule).into_iter().chain(&self.rules)
    }

    pub fn rule<Q: ?Sized>(&self, key: &Q) -> &Rule
    where
        Q: Borrow<RuleID>,
    {
        match key.borrow() {
            &RuleID::ACCEPT => &self.accept_rule,
            key => self.rules.get(key).unwrap(),
        }
    }
}

/// The contextural values for building a `Grammar`.
#[derive(Debug)]
pub struct GrammarDef<'def> {
    terminals: IndexSet<Terminal>,
    nonterminals: IndexSet<Nonterminal>,
    rules: IndexSet<Rule>,
    start: Option<NonterminalID>,
    next_terminal_id: u64,
    next_nonterminal_id: u64,
    next_rule_id: u64,
    _marker: PhantomData<&'def mut ()>,
}

impl<'def> GrammarDef<'def> {
    /// Declare a terminal symbol used in this grammar.
    pub fn terminal(
        &mut self,
        export_name: impl Into<Cow<'static, str>>,
        precedence: Option<Precedence>,
    ) -> Result<SymbolRef<'def>, GrammarDefError> {
        let export_name = verify_ident(export_name.into()).ok_or_else(|| GrammarDefError {
            msg: "incorrect token name".into(),
        })?;

        for terminal in &self.terminals {
            if matches!(terminal.export_name(), Some(name) if name == export_name) {
                return Err(GrammarDefError {
                    msg: format!(
                        "The terminal `{}' has already been exported",
                        terminal.export_name().unwrap_or("<bogus>")
                    ),
                });
            }
        }

        let id = TerminalID::new(self.next_terminal_id);
        self.next_terminal_id += 1;

        self.terminals.insert(Terminal {
            id,
            export_name: Some(export_name),
            precedence,
        });

        Ok(SymbolRef {
            inner: SymbolRefInner::T(id),
            _marker: PhantomData,
        })
    }

    /// Declare a nonterminal symbol used in this grammar.
    pub fn nonterminal(
        &mut self,
        export_name: impl Into<Cow<'static, str>>,
    ) -> Result<SymbolRef<'def>, GrammarDefError> {
        let export_name = verify_ident(export_name.into()).ok_or_else(|| GrammarDefError {
            msg: "incorrect symbol name".into(),
        })?;

        for nonterminal in &self.nonterminals {
            if matches!(nonterminal.export_name(), Some(name) if *name == export_name) {
                return Err(GrammarDefError {
                    msg: format!(
                        "The nonterminal export `{}' has already been used",
                        nonterminal.export_name().unwrap_or("<bogus>")
                    ),
                });
            }
        }

        let id = NonterminalID::new(self.next_nonterminal_id);
        self.next_nonterminal_id += 1;

        self.nonterminals.insert(Nonterminal {
            id,
            export_name: Some(export_name),
        });

        Ok(SymbolRef {
            inner: SymbolRefInner::N(id),
            _marker: PhantomData,
        })
    }

    /// Specify a production rule into this grammer.
    pub fn rule<I>(
        &mut self,
        left: SymbolRef,
        right: I,
        precedence: Option<Precedence>,
    ) -> Result<(), GrammarDefError>
    where
        I: IntoIterator<Item = SymbolRef<'def>>,
    {
        let left = match left.inner {
            SymbolRefInner::T(..) => {
                return Err(GrammarDefError {
                    msg: "The starting symbol in production rule must be nonterminal".into(),
                });
            }
            SymbolRefInner::N(left) => left,
        };

        let mut right_ = vec![];
        for id in right {
            let sym = match id.inner {
                SymbolRefInner::T(t) => Symbol::T(t),
                SymbolRefInner::N(n) => Symbol::N(n),
            };
            right_.push(sym);
        }

        let id = RuleID::new(self.next_rule_id);
        self.next_rule_id += 1;
        self.rules.insert(Rule {
            id,
            left,
            right: right_,
            precedence,
        });

        Ok(())
    }

    /// Specify the start symbol for this grammar.
    pub fn start_symbol(&mut self, symbol: SymbolRef) -> Result<(), GrammarDefError> {
        match symbol.inner {
            SymbolRefInner::T(..) => {
                return Err(GrammarDefError {
                    msg: "the start symbol must be nonterminal".into(),
                });
            }
            SymbolRefInner::N(symbol) => {
                self.start.replace(symbol);
            }
        }
        Ok(())
    }

    fn end(mut self) -> Result<Grammar, GrammarDefError> {
        // start symbolのID変換
        // 指定されていない場合は最初に登録されたnonterminal symbolを用いる
        let start = match self.start.take() {
            Some(start) => start,
            None => self
                .nonterminals
                .iter()
                .map(|n| n.id())
                .next()
                .ok_or_else(|| GrammarDefError {
                    msg: "empty nonterminal symbols".into(),
                })?,
        };

        Ok(Grammar {
            terminals: self.terminals,
            nonterminals: self.nonterminals,
            rules: self.rules,
            start_symbol: start,
            accept_rule: Rule {
                id: RuleID::ACCEPT,
                left: NonterminalID::ACCEPT,
                right: vec![Symbol::N(start)],
                precedence: None,
            },
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SymbolRef<'def> {
    inner: SymbolRefInner,
    _marker: PhantomData<&'def mut ()>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum SymbolRefInner {
    T(TerminalID),
    N(NonterminalID),
}

#[derive(Debug)]
pub struct GrammarDefError {
    #[allow(dead_code)]
    msg: String,
}

fn verify_ident<T: AsRef<str>>(t: T) -> Option<T> {
    let mut s = t.as_ref();

    if s.is_empty() {
        // The identifier must not be empty.
        return None;
    }

    if s.bytes().all(|b| b >= b'0' && b <= b'9') {
        // The number must not be identifer.
        return None;
    }

    if s.starts_with("r#") {
        s = &s[2..];
        if matches!(s, "crate" | "self" | "super" | "Self") {
            // unexpected raw identifier
            return None;
        }
    } else if is_strict_keyword(s) || is_reserved(s) {
        // Reserved keyword specified.
        return None;
    }

    let mut chars = s.chars();
    let first = chars.next().unwrap();
    if !is_ident_start(first) {
        // The identifier must be started with XID-Start.
        return None;
    }
    if chars.any(|ch| !is_ident_continue(ch)) {
        // The idenfier must be continued with XID-Continue.
        return None;
    }

    Some(t)
}

fn is_ident_start(ch: char) -> bool {
    ch == '_' || unicode_ident::is_xid_start(ch)
}

fn is_ident_continue(ch: char) -> bool {
    unicode_ident::is_xid_continue(ch)
}

fn is_strict_keyword(s: &str) -> bool {
    matches!(
        s,
        "as" | "break" | "const" | "continue" | "crate" | "else" | "enum" | "extern"
        | "false" | "fn" | "for" | "if" | "impl" | "in" | "let" | "loop" | "match" | "mod"
        | "move" | "mut" | "pub" | "ref" | "return" | "self" | "Self" | "static" | "struct"
        | "super" | "trait" | "true" | "type" | "unsafe" | "use" | "where" | "while"
        // since Rust 2018
        | "async" | "await" | "dyn"
    )
}

fn is_reserved(s: &str) -> bool {
    matches!(
        s,
        "abstract" | "become" | "box" | "do" | "final" | "macro" | "override" | "priv"
        | "typeof" | "unsized" | "virtual" | "yield"
        // since Rust 2018
        | "try"
    )
}
