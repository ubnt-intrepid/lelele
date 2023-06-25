//! Grammar types.

use crate::IndexSet;
use std::{
    borrow::{Borrow, Cow},
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    rc::Rc,
};

const SYMBOL_ID_OFFSET: u64 = 0x4;
const RULE_ID_OFFSET: u64 = 0x4;

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct SymbolID {
    raw: u64,
}

impl SymbolID {
    /// Reserved symbol used as a terminal symbol that means the end of input.
    pub(crate) const EOI: Self = Self::new(0);
    /// Reserved symbol used as a nonterminal symbol to match when the parsing is complete.
    pub(crate) const ACCEPT: Self = Self::new(1);

    #[inline]
    const fn new(raw: u64) -> Self {
        assert!(raw < u64::MAX / 2, "too large SymbolID");
        Self { raw }
    }

    #[inline]
    pub(crate) const fn raw(self) -> u64 {
        self.raw
    }
}

impl fmt::Debug for SymbolID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for SymbolID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &Self::EOI => write!(f, "$end"),
            &Self::ACCEPT => write!(f, "$accept"),
            Self { raw } => write!(f, "SymbolID({})", raw),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    inner: Rc<SymbolInner>,
}

#[derive(Debug)]
struct SymbolInner {
    id: SymbolID,
    export_name: Option<Cow<'static, str>>,
    kind: SymbolKind,
    precedence: Option<Precedence>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum SymbolKind {
    Terminal,
    Nonterminal,
}

impl Symbol {
    pub fn id(&self) -> SymbolID {
        self.inner.id
    }

    pub fn export_name(&self) -> Option<&str> {
        self.inner.export_name.as_deref()
    }

    pub fn is_terminal(&self) -> bool {
        matches!(self.inner.kind, SymbolKind::Terminal)
    }

    pub fn precedence(&self) -> Option<&Precedence> {
        self.inner.precedence.as_ref()
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.inner.id == other.inner.id
    }
}
impl Eq for Symbol {}
impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.id.hash(state);
    }
}

impl Borrow<SymbolID> for Symbol {
    fn borrow(&self) -> &SymbolID {
        &self.inner.id
    }
}

impl Borrow<SymbolID> for &Symbol {
    fn borrow(&self) -> &SymbolID {
        &self.inner.id
    }
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

    #[inline]
    pub(crate) const fn raw(self) -> u64 {
        self.raw
    }
}
impl fmt::Display for RuleID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &Self::ACCEPT => write!(f, "$accept"),
            Self { raw } => fmt::Display::fmt(raw, f),
        }
    }
}

/// The type that represents a production rule in grammar.
#[derive(Debug, Clone)]
pub struct Rule {
    inner: Rc<RuleInner>,
}

#[derive(Debug)]
struct RuleInner {
    id: RuleID,
    left: Symbol,
    right: Vec<Symbol>,
    export_name: Option<Cow<'static, str>>,
    precedence: Option<Precedence>,
}

impl Rule {
    pub fn id(&self) -> RuleID {
        self.inner.id
    }

    /// Return the left-hand side of this production.
    pub fn left(&self) -> &Symbol {
        &self.inner.left
    }

    /// Return the right-hand side of this production.
    pub fn right(&self) -> &[Symbol] {
        &self.inner.right[..]
    }

    pub fn export_name(&self) -> Option<&str> {
        self.inner.export_name.as_deref()
    }

    pub fn precedence(&self) -> Option<&Precedence> {
        match self.inner.precedence {
            Some(ref prec) => Some(prec),
            None => {
                for symbol in self.inner.right.iter().rev() {
                    if symbol.is_terminal() {
                        return symbol.precedence();
                    }
                }
                None
            }
        }
    }
}

impl PartialEq for Rule {
    fn eq(&self, other: &Self) -> bool {
        self.inner.id == other.inner.id
    }
}

impl Eq for Rule {}

impl Hash for Rule {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.id.hash(state);
    }
}

impl PartialOrd for Rule {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        PartialOrd::partial_cmp(&self.inner.id, &other.inner.id)
    }
}

impl Ord for Rule {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Ord::cmp(&self.inner.id, &other.inner.id)
    }
}

impl Borrow<RuleID> for Rule {
    fn borrow(&self) -> &RuleID {
        &self.inner.id
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
    terminals: IndexSet<Symbol>,
    nonterminals: IndexSet<Symbol>,
    rules: IndexSet<Rule>,
    start_symbol: Symbol,
    accept_rule: Rule,
}

impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "## terminals:")?;
        for sym in self.terminals() {
            let name = sym.export_name().unwrap_or("<bogus>");
            write!(f, "- {}", name)?;
            if let Some(prec) = sym.precedence() {
                write!(f, " (priority={}, assoc={})", prec.priority, prec.assoc)?;
            }
            writeln!(f)?;
        }

        writeln!(f, "\n## nonterminals:")?;
        for sym in self.nonterminals() {
            write!(f, "- {}", sym.export_name().unwrap_or("<bogus>"))?;
            if sym.id() == self.start_symbol.id() {
                write!(f, " (start)")?;
            }
            writeln!(f)?;
        }

        writeln!(f, "\n## rules:")?;
        for rule in self.rules() {
            write!(f, "- ")?;
            if let Some(name) = rule.export_name() {
                write!(f, "[{}] ", name)?;
            }
            write!(f, "{} : ", rule.left().export_name().unwrap_or("<bogus>"))?;
            for (i, symbol) in rule.right().iter().enumerate() {
                if i > 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}", symbol.export_name().unwrap_or("<bogus>"))?;
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
            next_symbol_id: SYMBOL_ID_OFFSET,
            next_rule_id: RULE_ID_OFFSET,
            _marker: PhantomData,
        };

        def.terminals.insert(Symbol {
            inner: Rc::new(SymbolInner {
                id: SymbolID::EOI,
                export_name: None,
                kind: SymbolKind::Terminal,
                precedence: None,
            }),
        });
        def.nonterminals.insert(Symbol {
            inner: Rc::new(SymbolInner {
                id: SymbolID::ACCEPT,
                export_name: None,
                kind: SymbolKind::Nonterminal,
                precedence: None,
            }),
        });

        f(&mut def)?;

        def.end()
    }

    /// Returns the terminal symbol declared in this grammar.
    pub fn terminals(&self) -> impl Iterator<Item = &Symbol> + '_ {
        self.terminals
            .iter()
            .inspect(|s| debug_assert!(s.is_terminal()))
    }

    /// Returns the nonterminal symbol declared in this grammar.
    pub fn nonterminals(&self) -> impl Iterator<Item = &Symbol> + '_ {
        self.nonterminals
            .iter()
            .inspect(|sym| debug_assert!(!sym.is_terminal()))
    }

    pub fn start_symbol(&self) -> &Symbol {
        &self.start_symbol
    }

    pub fn rules(&self) -> impl Iterator<Item = &Rule> + '_ {
        Some(&self.accept_rule).into_iter().chain(&self.rules)
    }

    pub(crate) fn eoi(&self) -> &Symbol {
        &self.terminals.get(&SymbolID::EOI).unwrap()
    }

    pub(crate) fn accept_rule(&self) -> &Rule {
        &self.accept_rule
    }
}

/// The contextural values for building a `Grammar`.
#[derive(Debug)]
pub struct GrammarDef<'def> {
    terminals: IndexSet<Symbol>,
    nonterminals: IndexSet<Symbol>,
    rules: IndexSet<Rule>,
    start: Option<Symbol>,
    next_symbol_id: u64,
    next_rule_id: u64,
    _marker: PhantomData<&'def mut ()>,
}

impl GrammarDef<'_> {
    /// Declare a terminal symbol used in this grammar.
    pub fn terminal(
        &mut self,
        export_name: impl Into<Cow<'static, str>>,
        precedence: Option<Precedence>,
    ) -> Result<SymbolID, GrammarDefError> {
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

        let id = SymbolID::new(self.next_symbol_id);
        self.next_symbol_id += 1;

        self.terminals.insert(Symbol {
            inner: Rc::new(SymbolInner {
                id,
                kind: SymbolKind::Terminal,
                export_name: Some(export_name),
                precedence,
            }),
        });

        Ok(id)
    }

    /// Declare a nonterminal symbol used in this grammar.
    pub fn nonterminal(
        &mut self,
        export_name: impl Into<Cow<'static, str>>,
    ) -> Result<SymbolID, GrammarDefError> {
        let export_name = verify_ident(export_name.into()).ok_or_else(|| GrammarDefError {
            msg: "incorrect symbol name".into(),
        })?;

        for sym in &self.nonterminals {
            if matches!(sym.export_name(), Some(name) if *name == export_name) {
                return Err(GrammarDefError {
                    msg: format!(
                        "The nonterminal export `{}' has already been used",
                        sym.export_name().unwrap_or("<bogus>")
                    ),
                });
            }
        }

        let id = SymbolID::new(self.next_symbol_id);
        self.next_symbol_id += 1;

        self.nonterminals.insert(Symbol {
            inner: Rc::new(SymbolInner {
                id,
                kind: SymbolKind::Nonterminal,
                export_name: Some(export_name),
                precedence: None,
            }),
        });

        Ok(id)
    }

    /// Specify a production rule into this grammer.
    pub fn rule<I>(
        &mut self,
        left: SymbolID,
        right: I,
        export_name: impl Into<Cow<'static, str>>,
        precedence: Option<Precedence>,
    ) -> Result<RuleID, GrammarDefError>
    where
        I: IntoIterator<Item = SymbolID>,
    {
        let left = self
            .nonterminals
            .get(&left)
            .cloned()
            .ok_or_else(|| GrammarDefError {
                msg: "The starting symbol in production rule must be nonterminal".into(),
            })?;

        let mut right_ = vec![];
        for id in right {
            right_.push(
                self.terminals
                    .get(&id)
                    .or_else(|| self.nonterminals.get(&id))
                    .cloned()
                    .expect("incorrect symbol id"),
            );
        }

        let export_name = verify_ident(export_name.into()).ok_or_else(|| GrammarDefError {
            msg: "incorrect rule name".into(),
        })?;

        let id = RuleID::new(self.next_rule_id);
        self.next_rule_id += 1;
        self.rules.insert(Rule {
            inner: Rc::new(RuleInner {
                id,
                left,
                right: right_,
                export_name: Some(export_name),
                precedence,
            }),
        });

        Ok(id)
    }

    /// Specify the start symbol for this grammar.
    pub fn start_symbol(&mut self, symbol: SymbolID) -> Result<(), GrammarDefError> {
        let symbol = self
            .nonterminals
            .get(&symbol)
            .ok_or_else(|| GrammarDefError {
                msg: "the start symbol must be nonterminal".into(),
            })?;
        self.start.replace(symbol.clone());
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
                .cloned()
                .next()
                .ok_or_else(|| GrammarDefError {
                    msg: "empty nonterminal symbols".into(),
                })?,
        };

        let accept_rule = Rule {
            inner: Rc::new(RuleInner {
                id: RuleID::ACCEPT,
                left: self.nonterminals.get(&SymbolID::ACCEPT).cloned().unwrap(),
                right: vec![start.clone()],
                export_name: None,
                precedence: None,
            }),
        };

        Ok(Grammar {
            terminals: self.terminals,
            nonterminals: self.nonterminals,
            rules: self.rules,
            start_symbol: start,
            accept_rule,
        })
    }
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
