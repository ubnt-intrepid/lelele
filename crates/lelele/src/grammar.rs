//! Grammar types.

use crate::IndexMap;
use std::{
    borrow::{Borrow, Cow},
    fmt,
    marker::PhantomData,
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

#[derive(Debug)]
pub struct Symbol {
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
    const EOI: Self = Self {
        id: SymbolID::EOI,
        export_name: None,
        kind: SymbolKind::Terminal,
        precedence: None,
    };

    const ACCEPT: Self = Self {
        id: SymbolID::ACCEPT,
        export_name: None,
        kind: SymbolKind::Nonterminal,
        precedence: None,
    };
}

impl Symbol {
    pub const fn id(&self) -> SymbolID {
        self.id
    }

    pub fn export_name(&self) -> Option<&str> {
        self.export_name.as_deref()
    }

    pub fn is_terminal(&self) -> bool {
        matches!(self.kind, SymbolKind::Terminal)
    }

    pub fn precedence(&self) -> Option<&Precedence> {
        self.precedence.as_ref()
    }
}

impl Borrow<SymbolID> for Symbol {
    fn borrow(&self) -> &SymbolID {
        &self.id
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
#[non_exhaustive]
pub struct Precedence {
    pub priority: usize,
    pub assoc: Assoc,
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
#[derive(Debug)]
pub struct Rule {
    id: RuleID,
    left: SymbolID,
    right: Vec<SymbolID>,
    export_name: Option<Cow<'static, str>>,
    prec_token: Option<SymbolID>,
}

impl Rule {
    pub const fn id(&self) -> RuleID {
        self.id
    }

    /// Return the left-hand side of this production.
    pub fn left(&self) -> SymbolID {
        self.left
    }

    /// Return the right-hand side of this production.
    pub fn right(&self) -> &[SymbolID] {
        &self.right[..]
    }

    pub fn export_name(&self) -> Option<&str> {
        self.export_name.as_deref()
    }

    pub fn precedence<'g>(&'g self, grammar: &'g Grammar) -> Option<&'g Precedence> {
        match self.prec_token {
            Some(ref tok) => grammar.terminals[tok].precedence(),
            None => {
                for id in self.right.iter().rev() {
                    let symbol = grammar.symbol(id);
                    if symbol.is_terminal() {
                        return symbol.precedence();
                    }
                }
                None
            }
        }
    }
}

impl Borrow<RuleID> for Rule {
    fn borrow(&self) -> &RuleID {
        &self.id
    }
}

/// The grammar definition used to derive the parser tables.
#[derive(Debug)]
pub struct Grammar {
    terminals: IndexMap<SymbolID, Symbol>,
    nonterminals: IndexMap<SymbolID, Symbol>,
    rules: IndexMap<RuleID, Rule>,
    start_symbol: SymbolID,
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
            if sym.id() == self.start_symbol {
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
            write!(
                f,
                "{} : ",
                self.symbol(&rule.left()).export_name().unwrap_or("<bogus>")
            )?;
            for (i, symbol) in rule.right().iter().enumerate() {
                if i > 0 {
                    write!(f, " ")?;
                }
                write!(
                    f,
                    "{}",
                    self.symbol(symbol).export_name().unwrap_or("<bogus>")
                )?;
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
            terminals: IndexMap::default(),
            nonterminals: IndexMap::default(),
            rules: IndexMap::default(),
            start: None,
            next_symbol_id: SYMBOL_ID_OFFSET,
            next_rule_id: RULE_ID_OFFSET,
            next_priority: 0,
            _marker: PhantomData,
        };

        f(&mut def)?;

        def.end()
    }

    /// Returns the terminal symbol declared in this grammar.
    pub fn terminals(&self) -> impl Iterator<Item = &Symbol> + '_ {
        Some(&Symbol::EOI)
            .into_iter()
            .chain(self.terminals.values())
            .inspect(|s| debug_assert!(s.is_terminal()))
    }

    /// Returns the nonterminal symbol declared in this grammar.
    pub fn nonterminals(&self) -> impl Iterator<Item = &Symbol> + '_ {
        Some(&Symbol::ACCEPT)
            .into_iter()
            .chain(self.nonterminals.values())
            .inspect(|sym| debug_assert!(!sym.is_terminal()))
    }

    pub fn symbol<Q: ?Sized>(&self, key: &Q) -> &Symbol
    where
        Q: Borrow<SymbolID>,
    {
        match key.borrow() {
            &SymbolID::EOI => &Symbol::EOI,
            &SymbolID::ACCEPT => &Symbol::ACCEPT,
            id => self
                .terminals
                .get(id)
                .or_else(|| self.nonterminals.get(id))
                .expect("unexpected symbol id"),
        }
    }

    pub fn start_symbol(&self) -> &Symbol {
        self.symbol(&self.start_symbol)
    }

    pub fn rules(&self) -> impl Iterator<Item = &Rule> + '_ {
        Some(&self.accept_rule)
            .into_iter()
            .chain(self.rules.values())
    }

    pub fn rule<Q: ?Sized>(&self, key: &Q) -> &Rule
    where
        Q: Borrow<RuleID>,
    {
        match *key.borrow() {
            RuleID::ACCEPT => &self.accept_rule,
            id => &self.rules[&id],
        }
    }
}

/// The contextural values for building a `Grammar`.
#[derive(Debug)]
pub struct GrammarDef<'def> {
    terminals: IndexMap<SymbolID, Symbol>,
    nonterminals: IndexMap<SymbolID, Symbol>,
    rules: IndexMap<RuleID, Rule>,
    start: Option<SymbolID>,
    next_symbol_id: u64,
    next_rule_id: u64,
    next_priority: usize,
    _marker: PhantomData<&'def mut ()>,
}

impl GrammarDef<'_> {
    /// Specify a terminal symbol used in this grammar.
    pub fn token(
        &mut self,
        export_name: impl Into<Cow<'static, str>>,
    ) -> Result<SymbolID, GrammarDefError> {
        let export_name = verify_ident(export_name.into()).ok_or_else(|| GrammarDefError {
            msg: "incorrect token name".into(),
        })?;
        self.add_token(Some(export_name))
    }

    /// Add a "bogus" token symbol into this grammar.
    pub fn bogus_token(&mut self) -> Result<SymbolID, GrammarDefError> {
        self.add_token(None)
    }

    fn add_token(
        &mut self,
        export_name: Option<Cow<'static, str>>,
    ) -> Result<SymbolID, GrammarDefError> {
        for terminal in self.terminals.values() {
            if matches!((&terminal.export_name, &export_name), (Some(n1), Some(n2)) if n1 == n2) {
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

        self.terminals.insert(
            id,
            Symbol {
                id,
                kind: SymbolKind::Terminal,
                export_name,
                precedence: None,
            },
        );

        Ok(id)
    }

    /// Specify a nonterminal symbol used in this grammar.
    pub fn symbol(
        &mut self,
        export_name: impl Into<Cow<'static, str>>,
    ) -> Result<SymbolID, GrammarDefError> {
        let export_name = verify_ident(export_name.into()).ok_or_else(|| GrammarDefError {
            msg: "incorrect symbol name".into(),
        })?;

        for sym in self.nonterminals.values() {
            if matches!(&sym.export_name, Some(name) if *name == export_name) {
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

        self.nonterminals.insert(
            id,
            Symbol {
                id,
                kind: SymbolKind::Nonterminal,
                export_name: Some(export_name),
                precedence: None,
            },
        );

        Ok(id)
    }

    /// Specify a production rule into this grammer.
    pub fn rule<I>(
        &mut self,
        name: impl Into<Cow<'static, str>>,
        left: SymbolID,
        right: I,
    ) -> Result<RuleID, GrammarDefError>
    where
        I: IntoIterator<Item = SymbolID>,
    {
        self.rule_with_prec(name, left, right, None)
    }

    pub fn rule_with_prec<I>(
        &mut self,
        name: impl Into<Cow<'static, str>>,
        left: SymbolID,
        right: I,
        prec_token: Option<SymbolID>,
    ) -> Result<RuleID, GrammarDefError>
    where
        I: IntoIterator<Item = SymbolID>,
    {
        if !self.nonterminals.contains_key(&left) {
            return Err(GrammarDefError {
                msg: "The starting symbol in production rule must be nonterminal".into(),
            });
        }

        let export_name = verify_ident(name.into()).ok_or_else(|| GrammarDefError {
            msg: "incorrect rule name".into(),
        })?;

        if prec_token.map_or(false, |prec| !self.terminals.contains_key(&prec)) {
            return Err(GrammarDefError {
                msg: "The prec symbol must be terminal".into(),
            });
        }

        let id = RuleID::new(self.next_rule_id);
        self.next_rule_id += 1;
        self.rules.insert(
            id,
            Rule {
                id,
                left,
                right: right.into_iter().collect(),
                export_name: Some(export_name),
                prec_token,
            },
        );

        Ok(id)
    }

    /// Specify the start symbol for this grammar.
    pub fn start_symbol(&mut self, symbol: SymbolID) -> Result<(), GrammarDefError> {
        if !self.nonterminals.contains_key(&symbol) {
            return Err(GrammarDefError {
                msg: "the start symbol must be nonterminal".into(),
            });
        }
        self.start.replace(symbol);
        Ok(())
    }

    /// Specify the precedence for the terminal symbols.
    pub fn precedence<I>(&mut self, assoc: Assoc, tokens: I) -> Result<(), GrammarDefError>
    where
        I: IntoIterator<Item = SymbolID>,
    {
        let priority = self.next_priority;
        let mut changed = false;
        for token in tokens {
            let symbol = &mut self
                .terminals
                .get_mut(&token)
                .ok_or_else(|| GrammarDefError {
                    msg: "nonterminal cannot have precedence".into(),
                })?;
            match symbol.precedence {
                Some(..) => {
                    return Err(GrammarDefError {
                        msg: format!(
                            "The token {} has already been set the precedence",
                            symbol.export_name().unwrap_or("<bogus>")
                        ),
                    })
                }
                None => symbol.precedence = Some(Precedence { priority, assoc }),
            }
            changed = true;
        }

        if changed {
            self.next_priority += 1;
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
                .keys()
                .copied()
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
                left: SymbolID::ACCEPT,
                right: vec![start],
                export_name: None,
                prec_token: None,
            },
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
