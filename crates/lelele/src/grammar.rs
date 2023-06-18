//! Grammar types.

use crate::IndexMap;
use std::{borrow::Cow, fmt, marker::PhantomData};

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
    export_name: Option<Cow<'static, str>>,
    kind: SymbolKind,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum SymbolKind {
    Terminal,
    Nonterminal,
}

impl Symbol {
    const EOI: Self = Self {
        export_name: None,
        kind: SymbolKind::Terminal,
    };
    const ACCEPT: Self = Self {
        export_name: None,
        kind: SymbolKind::Nonterminal,
    };
}

impl Symbol {
    pub fn export_name(&self) -> Option<&str> {
        self.export_name.as_deref()
    }

    pub fn is_terminal(&self) -> bool {
        matches!(self.kind, SymbolKind::Terminal)
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
    left: SymbolID,
    right: Vec<SymbolID>,
    export_name: Option<Cow<'static, str>>,
}

impl Rule {
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

    pub fn display<'g>(&'g self, grammar: &'g Grammar) -> impl fmt::Display + '_ {
        RuleDisplay {
            grammar,
            rule: self,
        }
    }
}

struct RuleDisplay<'g> {
    grammar: &'g Grammar,
    rule: &'g Rule,
}

impl fmt::Display for RuleDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            grammar,
            rule: Rule { left, right, .. },
        } = self;
        write!(
            f,
            "{} : ",
            grammar.symbol(*left).export_name().unwrap_or("<bogus>")
        )?;
        for (i, symbol) in right.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(
                f,
                "{}",
                grammar.symbol(*symbol).export_name().unwrap_or("<bogus>")
            )?;
        }
        Ok(())
    }
}

/// The grammar definition used to derive the parser tables.
#[derive(Debug)]
pub struct Grammar {
    symbols: IndexMap<SymbolID, Symbol>,
    rules: IndexMap<RuleID, Rule>,
    start_symbol: SymbolID,
    accept_rule: Rule,
}

impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "terminals: ")?;
        for (i, (_, sym)) in self.terminals().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", sym.export_name().unwrap_or("<bogus>"))?;
        }
        write!(f, "\nnonterminals: ")?;
        for (i, (_, sym)) in self.nonterminals().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", sym.export_name().unwrap_or("<bogus>"))?;
        }
        writeln!(
            f,
            "\nstart_symbol: {}",
            self.symbol(self.start_symbol)
                .export_name()
                .unwrap_or("<bogus>")
        )?;
        write!(f, "rules:\n")?;
        for (id, rule) in self.rules() {
            writeln!(f, "  [{:02}] {}", id, rule.display(self))?;
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
            symbols: IndexMap::default(),
            rules: IndexMap::default(),
            start: None,
            next_symbol_id: SYMBOL_ID_OFFSET,
            next_rule_id: RULE_ID_OFFSET,
            _marker: PhantomData,
        };

        f(&mut def)?;

        def.end()
    }

    pub fn symbols(&self) -> impl Iterator<Item = (SymbolID, &Symbol)> + '_ {
        [
            (SymbolID::EOI, &Symbol::EOI),
            (SymbolID::ACCEPT, &Symbol::ACCEPT),
        ]
        .into_iter()
        .chain(self.symbols.iter().map(|(id, sym)| (*id, sym)))
    }

    pub fn terminals(&self) -> impl Iterator<Item = (SymbolID, &Symbol)> + '_ {
        self.symbols().filter(|(_id, sym)| sym.is_terminal())
    }

    pub fn nonterminals(&self) -> impl Iterator<Item = (SymbolID, &Symbol)> + '_ {
        self.symbols().filter(|(_id, sym)| !sym.is_terminal())
    }

    pub fn symbol(&self, id: SymbolID) -> &Symbol {
        match id {
            SymbolID::EOI => &Symbol::EOI,
            SymbolID::ACCEPT => &Symbol::ACCEPT,
            id => &self.symbols[&id],
        }
    }

    pub fn start_symbol(&self) -> (SymbolID, &Symbol) {
        (self.start_symbol, self.symbol(self.start_symbol))
    }

    pub fn rules(&self) -> impl Iterator<Item = (RuleID, &Rule)> + '_ {
        Some((RuleID::ACCEPT, &self.accept_rule))
            .into_iter()
            .chain(self.rules.iter().map(|(id, rule)| (*id, rule)))
    }

    pub fn rule(&self, id: RuleID) -> &Rule {
        match id {
            RuleID::ACCEPT => &self.accept_rule,
            id => &self.rules[&id],
        }
    }
}

/// The contextural values for building a `Grammar`.
#[derive(Debug)]
pub struct GrammarDef<'def> {
    symbols: IndexMap<SymbolID, Symbol>,
    rules: IndexMap<RuleID, Rule>,
    start: Option<SymbolID>,
    next_symbol_id: u64,
    next_rule_id: u64,
    _marker: PhantomData<&'def mut ()>,
}

impl GrammarDef<'_> {
    fn add_symbol(&mut self, added: Symbol) -> Result<SymbolID, GrammarDefError> {
        match self
            .symbols
            .iter_mut()
            .find(|(_, sym)| sym.export_name == added.export_name)
        {
            Some((id, sym)) => {
                if sym.kind != added.kind {
                    return Err(GrammarDefError {
                        msg: format!(
                            "conflict symbol kind (name={})",
                            sym.export_name().unwrap_or("<bogus>")
                        ),
                    });
                }
                Ok(*id)
            }
            None => {
                let id = SymbolID::new(self.next_symbol_id);
                self.next_symbol_id += 1;
                self.symbols.insert(id, added);
                Ok(id)
            }
        }
    }

    /// Specify a terminal symbol used in this grammar.
    pub fn token(
        &mut self,
        export_name: impl Into<Cow<'static, str>>,
    ) -> Result<SymbolID, GrammarDefError> {
        let export_name = verify_ident(export_name.into()).ok_or_else(|| GrammarDefError {
            msg: "incorrect token name".into(),
        })?;
        self.add_symbol(Symbol {
            export_name: Some(export_name),
            kind: SymbolKind::Terminal,
        })
    }

    /// Add a "bogus" token symbol into this grammar.
    pub fn bogus_token(&mut self) -> Result<SymbolID, GrammarDefError> {
        self.add_symbol(Symbol {
            export_name: None,
            kind: SymbolKind::Terminal,
        })
    }

    /// Specify a nonterminal symbol used in this grammar.
    pub fn symbol(
        &mut self,
        export_name: impl Into<Cow<'static, str>>,
    ) -> Result<SymbolID, GrammarDefError> {
        let export_name = verify_ident(export_name.into()).ok_or_else(|| GrammarDefError {
            msg: "incorrect symbol name".into(),
        })?;
        self.add_symbol(Symbol {
            export_name: Some(export_name),
            kind: SymbolKind::Nonterminal,
        })
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
        if self
            .symbols
            .get(&left)
            .map_or(true, |lhs| lhs.kind != SymbolKind::Nonterminal)
        {
            return Err(GrammarDefError {
                msg: "The starting symbol in production rule must be nonterminal".into(),
            });
        }

        let export_name = verify_ident(name.into()).ok_or_else(|| GrammarDefError {
            msg: "incorrect rule name".into(),
        })?;

        let id = RuleID::new(self.next_rule_id);
        self.next_rule_id += 1;
        self.rules.insert(
            id,
            Rule {
                left,
                right: right.into_iter().collect(),
                export_name: Some(export_name),
            },
        );

        Ok(id)
    }

    /// Specify the start symbol.
    pub fn start_symbol(&mut self, symbol: SymbolID) -> Result<(), GrammarDefError> {
        if self
            .symbols
            .get(&symbol)
            .map_or(true, |s| s.kind != SymbolKind::Nonterminal)
        {
            return Err(GrammarDefError {
                msg: "the start symbol must be nonterminal".into(),
            });
        }
        self.start.replace(symbol);
        Ok(())
    }

    fn end(mut self) -> Result<Grammar, GrammarDefError> {
        // start symbolのID変換
        // 指定されていない場合は最初に登録されたnonterminal symbolを用いる
        let start = match self.start.take() {
            Some(start) => start,
            None => self
                .symbols
                .iter()
                .find_map(|(id, sym)| (sym.kind == SymbolKind::Nonterminal).then_some(*id))
                .unwrap(),
        };

        Ok(Grammar {
            symbols: self.symbols,
            rules: self.rules,
            start_symbol: start,
            accept_rule: Rule {
                left: SymbolID::ACCEPT,
                right: vec![start],
                export_name: None,
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
