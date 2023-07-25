//! Grammar types.

use crate::{
    syntax as s,
    types::{Map, Set},
    util::display_fn,
};
use std::{
    borrow::{Borrow, Cow},
    fmt, fs,
    hash::{Hash, Hasher},
    io,
    marker::PhantomData,
    path::Path,
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

    /// Reserved symbol used as an error token.
    pub(crate) const ERROR: Self = Self::new(1);

    #[inline]
    const fn new(raw: u64) -> Self {
        assert!(raw < u64::MAX / 2, "too large TerminalID");
        Self { raw }
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
            TerminalID::ERROR => f.write_str("$error"),
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
    pub const START: Self = Self::new(0);

    #[inline]
    const fn new(raw: u64) -> Self {
        assert!(raw < u64::MAX / 2, "too large NonterminalID");
        Self { raw }
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
            NonterminalID::START => f.write_str("$start"),
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
    pub const ACCEPT: Self = Self::new(0);

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

    // `"LHS := R1 R2 R3"`
    pub fn display<'g>(&'g self, g: &'g Grammar) -> impl fmt::Display + 'g {
        display_fn(|f| {
            write!(f, "{} := ", g.nonterminal(&self.left()))?;
            for (i, symbol) in self.right().iter().enumerate() {
                if i > 0 {
                    write!(f, " ")?;
                }
                match symbol {
                    Symbol::T(t) => write!(f, "{}", g.terminal(t))?,
                    Symbol::N(n) => write!(f, "{}", g.nonterminal(n))?,
                }
            }
            Ok(())
        })
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
    pub(crate) terminals: Set<Terminal>,
    nonterminals: Set<Nonterminal>,
    rules: Set<Rule>,
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
            write!(f, "{}", rule.display(self))?;
            if let Some(prec) = &rule.precedence {
                write!(f, " (priority={}, assoc={})", prec.priority, prec.assoc)?;
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

impl Grammar {
    pub fn from_file(path: impl AsRef<Path>) -> Result<Grammar, GrammarDefError> {
        let source = fs::read_to_string(path).map_err(GrammarDefError::IO)?;
        Self::from_str(&source)
    }

    pub fn from_str(source: &str) -> Result<Grammar, GrammarDefError> {
        let grammar = crate::syntax::parse(source).map_err(GrammarDefError::Syntax)?;
        Grammar::define(|g| define_grammar_from_syntax(g, grammar))
    }

    /// Define a grammar using the specified function.
    pub fn define<F>(f: F) -> Result<Self, GrammarDefError>
    where
        F: FnOnce(&mut GrammarDef) -> Result<(), GrammarDefError>,
    {
        let mut def = GrammarDef {
            terminals: Set::default(),
            nonterminals: Set::default(),
            rules: Set::default(),
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
        def.terminals.insert(Terminal {
            id: TerminalID::ERROR,
            export_name: None,
            precedence: None,
        });

        def.nonterminals.insert(Nonterminal {
            id: NonterminalID::START,
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

    pub fn start(&self) -> NonterminalID {
        self.start_symbol
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

fn define_grammar_from_syntax(
    g: &mut GrammarDef<'_>,
    mut grammar: s::Grammar,
) -> Result<(), GrammarDefError> {
    //
    grammar.stmts.sort_by(|s1, s2| {
        use std::cmp::Ordering::*;
        let cmp = match s1 {
            s::Stmt::PrecDesc(..) => match s2 {
                s::Stmt::PrecDesc(..) => Equal,
                _ => Greater,
            },
            s::Stmt::TerminalDesc(..) => match s2 {
                s::Stmt::PrecDesc(..) => Less,
                s::Stmt::TerminalDesc(..) => Equal,
                _ => Greater,
            },
            s::Stmt::NonterminalDesc(..) => match s2 {
                s::Stmt::RuleDesc(..) | s::Stmt::StartDesc(..) => Greater,
                s::Stmt::NonterminalDesc(..) => Equal,
                s::Stmt::TerminalDesc(..) | s::Stmt::PrecDesc(..) => Less,
            },
            s::Stmt::RuleDesc(..) => match s2 {
                s::Stmt::StartDesc(..) => Greater,
                s::Stmt::RuleDesc(..) => Equal,
                _ => Less,
            },
            s::Stmt::StartDesc(..) => match s2 {
                s::Stmt::StartDesc(..) => Equal,
                _ => Less,
            },
        };
        cmp.reverse()
    });

    let mut precedences = Map::default();
    let mut next_priority = 0;
    let mut terminals = Map::default();
    let mut nonterminals = Map::default();

    for desc in &grammar.stmts {
        match desc {
            s::Stmt::PrecDesc(s::PrecDesc { configs, ident }) => {
                let mut assoc = None;
                for config in configs {
                    match (&*config.key, &*config.value) {
                        ("assoc", "left") => assoc = Some(Assoc::Left),
                        ("assoc", "right") => assoc = Some(Assoc::Right),
                        ("assoc", "none" | "nonassoc") => assoc = Some(Assoc::Nonassoc),
                        _ => return Err("unexpected config in @prec desc".into()),
                    }
                }
                let assoc = assoc.unwrap_or(Assoc::Nonassoc);
                precedences.insert(ident.to_string(), Precedence::new(next_priority, assoc));
                next_priority += 1;
            }

            s::Stmt::TerminalDesc(s::TerminalDesc { configs, idents }) => {
                let mut prec = None;
                for config in configs {
                    if config.key == "prec" {
                        prec = Some(config.value.clone());
                    }
                }
                let prec = match prec {
                    Some(prec) => Some(
                        precedences
                            .get(&prec)
                            .copied()
                            .ok_or_else(|| format!("missing precedence name: `{}'", prec))?,
                    ),
                    None => None,
                };
                for name in idents {
                    let symbol = g.terminal(&*name, prec)?;
                    terminals.insert(name, symbol);
                }
            }

            s::Stmt::NonterminalDesc(s::NonterminalDesc { idents }) => {
                for name in idents {
                    let symbol = g.nonterminal(&*name)?;
                    nonterminals.insert(name, symbol);
                }
            }

            s::Stmt::StartDesc(s::StartDesc { name }) => {
                let start_symbol = nonterminals
                    .get(name)
                    .copied()
                    .ok_or_else(|| format!("unknown start symbol: `{}'", name))?;
                g.start_symbol(start_symbol)?;
            }

            s::Stmt::RuleDesc(s::RuleDesc { left, productions }) => {
                let left = match nonterminals.get(left) {
                    Some(s) => *s,
                    None => {
                        // 未登場の記号は非終端記号と解釈する
                        let id = g.nonterminal(left)?;
                        nonterminals.insert(left, id);
                        id
                    }
                };

                for production in productions {
                    let mut prec = None;
                    for config in &production.configs {
                        match &*config.key {
                            "prec" => {
                                prec = Some(precedences.get(&config.value).copied().ok_or_else(
                                    || format!("unknown precedence name: `{}'", config.value),
                                )?);
                            }
                            _ => (),
                        }
                    }

                    let mut right = vec![];
                    for symbol in &production.elems {
                        use s::ProductionElem::*;
                        let symbol = match symbol {
                            Ident(symbol) => {
                                let s = terminals.get(symbol).or_else(|| nonterminals.get(symbol));
                                match s {
                                    Some(s) => *s,
                                    None => {
                                        // 未登場の記号は非終端記号と解釈する
                                        let id = g.nonterminal(symbol)?;
                                        nonterminals.insert(symbol, id);
                                        id
                                    }
                                }
                            }
                            ErrorToken => SymbolRef::error_token(),
                        };
                        right.push(symbol);
                    }

                    g.rule(left, right, prec)?;
                }
            }
        }
    }

    Ok(())
}

/// The contextural values for building a `Grammar`.
#[derive(Debug)]
pub struct GrammarDef<'def> {
    terminals: Set<Terminal>,
    nonterminals: Set<Nonterminal>,
    rules: Set<Rule>,
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
        export_name: &str,
        precedence: Option<Precedence>,
    ) -> Result<SymbolRef, GrammarDefError> {
        if !verify_ident(export_name) {
            return Err(GrammarDefError::Other {
                msg: "incorrect token name".into(),
            });
        }

        for terminal in &self.terminals {
            if matches!(terminal.export_name(), Some(name) if name == export_name) {
                return Err(GrammarDefError::Other {
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
            export_name: Some(export_name.to_owned().into()),
            precedence,
        });

        Ok(SymbolRef {
            inner: SymbolRefInner::T(id),
        })
    }

    /// Declare a nonterminal symbol used in this grammar.
    pub fn nonterminal(&mut self, export_name: &str) -> Result<SymbolRef, GrammarDefError> {
        if !verify_ident(export_name) {
            return Err(GrammarDefError::Other {
                msg: "incorrect symbol name".into(),
            });
        }

        for nonterminal in &self.nonterminals {
            if matches!(nonterminal.export_name(), Some(name) if name == export_name) {
                return Err(GrammarDefError::Other {
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
            export_name: Some(export_name.to_owned().into()),
        });

        Ok(SymbolRef {
            inner: SymbolRefInner::N(id),
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
        I: IntoIterator<Item = SymbolRef>,
    {
        let left = match left.inner {
            SymbolRefInner::T(..) => {
                return Err(GrammarDefError::Other {
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
                return Err(GrammarDefError::Other {
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
                .find(|n| n.id() != NonterminalID::START)
                .ok_or_else(|| GrammarDefError::Other {
                    msg: "empty nonterminal symbols".into(),
                })?
                .id(),
        };

        let accept_rule = Rule {
            id: RuleID::ACCEPT,
            left: NonterminalID::START,
            right: vec![Symbol::N(start)],
            precedence: None,
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SymbolRef {
    inner: SymbolRefInner,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum SymbolRefInner {
    T(TerminalID),
    N(NonterminalID),
}

impl SymbolRef {
    pub const fn error_token() -> Self {
        Self {
            inner: SymbolRefInner::T(TerminalID::ERROR),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum GrammarDefError {
    #[error("IO error: {}", _0)]
    IO(io::Error),

    #[error("Syntax error: {}", _0)]
    Syntax(anyhow::Error),

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

fn verify_ident(mut s: &str) -> bool {
    if s.is_empty() {
        // The identifier must not be empty.
        return false;
    }

    if s.bytes().all(|b| b >= b'0' && b <= b'9') {
        // The number must not be identifer.
        return false;
    }

    if s.starts_with("r#") {
        s = &s[2..];
        if matches!(s, "crate" | "self" | "super" | "Self") {
            // unexpected raw identifier
            return false;
        }
    } else if is_strict_keyword(s) || is_reserved(s) {
        // Reserved keyword specified.
        return false;
    }

    let mut chars = s.chars();
    let first = chars.next().unwrap();
    if !is_ident_start(first) {
        // The identifier must be started with XID-Start.
        return false;
    }
    if chars.any(|ch| !is_ident_continue(ch)) {
        // The idenfier must be continued with XID-Continue.
        return false;
    }

    true
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
