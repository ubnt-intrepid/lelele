//! Grammar types.

use crate::{syntax as s, types::Map, util::display_fn};
use std::{borrow::Cow, fmt, fs, hash::Hash, io, marker::PhantomData, path::Path};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct TerminalID {
    raw: u16,
}
impl TerminalID {
    /// Reserved symbol used as a terminal symbol that means the end of input.
    pub const EOI: Self = Self::new(0);

    /// Reserved symbol used as an error token.
    pub const ERROR: Self = Self::new(1);

    const OFFSET: u16 = 2;

    #[inline]
    const fn new(raw: u16) -> Self {
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

    pub fn precedence(&self) -> Option<Precedence> {
        self.precedence
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
pub struct Rule {
    id: RuleID,
    left: NonterminalID,
    right: Vec<SymbolID>,
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
    pub fn right(&self) -> &[SymbolID] {
        &self.right[..]
    }

    pub fn precedence(&self, g: &Grammar) -> Option<Precedence> {
        match self.precedence {
            Some(prec) => Some(prec),
            None => {
                for symbol in self.right.iter().rev() {
                    if let SymbolID::T(t) = symbol {
                        return g.terminals[t].precedence();
                    }
                }
                None
            }
        }
    }

    // `"LHS := R1 R2 R3"`
    pub fn display<'g>(&'g self, g: &'g Grammar) -> impl fmt::Display + 'g {
        display_fn(|f| {
            write!(f, "{} := ", g.nonterminals[&self.left()])?;
            for (i, symbol) in self.right().iter().enumerate() {
                if i > 0 {
                    write!(f, " ")?;
                }
                match symbol {
                    SymbolID::T(t) => write!(f, "{}", g.terminals[t])?,
                    SymbolID::N(n) => write!(f, "{}", g.nonterminals[n])?,
                }
            }
            Ok(())
        })
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
#[non_exhaustive]
pub struct Grammar {
    pub terminals: Map<TerminalID, Terminal>,
    pub nonterminals: Map<NonterminalID, Nonterminal>,
    pub rules: Map<RuleID, Rule>,
    pub start_symbol: NonterminalID,
}

impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "## terminals:")?;
        for terminal in self.terminals.values() {
            write!(f, "{}", terminal)?;
            if let Some(prec) = terminal.precedence() {
                write!(f, " (priority={}, assoc={})", prec.priority, prec.assoc)?;
            }
            writeln!(f)?;
        }

        writeln!(f, "\n## nonterminals:")?;
        for nonterminal in self.nonterminals.values() {
            write!(f, "{}", nonterminal)?;
            if nonterminal.id() == self.start_symbol {
                write!(f, " (start)")?;
            }
            writeln!(f)?;
        }

        writeln!(f, "\n## rules:")?;
        for rule in self.rules.values() {
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
            terminals: Map::default(),
            nonterminals: Map::default(),
            rules: Map::default(),
            start: None,
            next_terminal_id: TerminalID::OFFSET,
            next_nonterminal_id: NonterminalID::OFFSET,
            next_rule_id: RuleID::OFFSET,
            _marker: PhantomData,
        };

        def.terminals.insert(
            TerminalID::EOI,
            Terminal {
                id: TerminalID::EOI,
                export_name: None,
                precedence: None,
            },
        );
        def.terminals.insert(
            TerminalID::ERROR,
            Terminal {
                id: TerminalID::ERROR,
                export_name: None,
                precedence: None,
            },
        );

        def.nonterminals.insert(
            NonterminalID::START,
            Nonterminal {
                id: NonterminalID::START,
                export_name: None,
            },
        );

        f(&mut def)?;

        def.end()
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
                                let s = terminals
                                    .get(symbol)
                                    .map(|t| SymbolID::T(*t)) //
                                    .or_else(|| nonterminals.get(symbol).map(|n| SymbolID::N(*n)));
                                match s {
                                    Some(s) => s,
                                    None => {
                                        // 未登場の記号は非終端記号と解釈する
                                        let id = g.nonterminal(symbol)?;
                                        nonterminals.insert(symbol, id);
                                        SymbolID::N(id)
                                    }
                                }
                            }
                            ErrorToken => SymbolID::T(TerminalID::ERROR),
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
        export_name: &str,
        precedence: Option<Precedence>,
    ) -> Result<TerminalID, GrammarDefError> {
        if !verify_ident(export_name) {
            return Err(GrammarDefError::Other {
                msg: "incorrect token name".into(),
            });
        }

        for terminal in self.terminals.values() {
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

        self.terminals.insert(
            id,
            Terminal {
                id,
                export_name: Some(export_name.to_owned().into()),
                precedence,
            },
        );

        Ok(id)
    }

    /// Declare a nonterminal symbol used in this grammar.
    pub fn nonterminal(&mut self, export_name: &str) -> Result<NonterminalID, GrammarDefError> {
        if !verify_ident(export_name) {
            return Err(GrammarDefError::Other {
                msg: "incorrect symbol name".into(),
            });
        }

        for nonterminal in self.nonterminals.values() {
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

        self.nonterminals.insert(
            id,
            Nonterminal {
                id,
                export_name: Some(export_name.to_owned().into()),
            },
        );

        Ok(id)
    }

    /// Specify a production rule into this grammer.
    pub fn rule<I>(
        &mut self,
        left: NonterminalID,
        right: I,
        precedence: Option<Precedence>,
    ) -> Result<(), GrammarDefError>
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
                id,
                left,
                right: right_,
                precedence,
            },
        );

        Ok(())
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
                id: RuleID::ACCEPT,
                left: NonterminalID::START,
                right: vec![SymbolID::N(start)],
                precedence: None,
            },
        );

        Ok(Grammar {
            terminals: self.terminals,
            nonterminals: self.nonterminals,
            rules: self.rules,
            start_symbol: start,
        })
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
