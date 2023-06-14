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
    name: Cow<'static, str>,
    kind: SymbolKind,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum SymbolKind {
    Terminal,
    Nonterminal,
}

impl Symbol {
    const EOI: Self = Self {
        name: Cow::Borrowed("$eoi"),
        kind: SymbolKind::Terminal,
    };
    const ACCEPT: Self = Self {
        name: Cow::Borrowed("$accept"),
        kind: SymbolKind::Nonterminal,
    };
}

impl Symbol {
    pub fn name(&self) -> &str {
        &*self.name
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
pub struct Rule<'g> {
    grammar: &'g Grammar,
    inner: &'g RuleInner,
}

#[derive(Debug)]
struct RuleInner {
    left: SymbolID,
    right: Vec<SymbolID>,
    export_name: String,
}

impl fmt::Display for Rule<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            grammar,
            inner: RuleInner { left, right, .. },
        } = self;
        write!(f, "{} : ", grammar.symbol(*left).name())?;
        for (i, symbol) in right.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", grammar.symbol(*symbol).name())?;
        }
        Ok(())
    }
}

impl Rule<'_> {
    /// Return the left-hand side of this production.
    pub fn left(&self) -> SymbolID {
        self.inner.left
    }

    /// Return the right-hand side of this production.
    pub fn right(&self) -> &[SymbolID] {
        &self.inner.right[..]
    }

    pub(crate) fn export_name(&self) -> &str {
        &*self.inner.export_name
    }
}

/// The grammar definition used to derive the parser tables.
#[derive(Debug)]
pub struct Grammar {
    symbols: IndexMap<SymbolID, Symbol>,
    rules: IndexMap<RuleID, RuleInner>,
    start_symbol: SymbolID,
    accept_rule: RuleInner,
}

impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "terminals: ")?;
        for (i, (_, sym)) in self.terminals().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", sym.name())?;
        }
        write!(f, "\nnonterminals: ")?;
        for (i, (_, sym)) in self.nonterminals().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", sym.name())?;
        }
        writeln!(
            f,
            "\nstart_symbol: {}",
            self.symbol(self.start_symbol).name()
        )?;
        write!(f, "rules:\n")?;
        for (id, production) in self.rules() {
            writeln!(f, "  [{:02}] {}", id, production)?;
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

    pub fn rules(&self) -> impl Iterator<Item = (RuleID, Rule<'_>)> + '_ {
        Some((
            RuleID::ACCEPT,
            Rule {
                grammar: self,
                inner: &self.accept_rule,
            },
        ))
        .into_iter()
        .chain(self.rules.iter().map(|(id, rule)| {
            (
                *id,
                Rule {
                    grammar: self,
                    inner: rule,
                },
            )
        }))
    }

    pub fn rule(&self, id: RuleID) -> Rule<'_> {
        let inner = match id {
            RuleID::ACCEPT => &self.accept_rule,
            id => &self.rules.get(&id).unwrap(),
        };
        Rule {
            grammar: self,
            inner,
        }
    }
}

/// The contextural values for building a `Grammar`.
#[derive(Debug)]
pub struct GrammarDef<'def> {
    symbols: IndexMap<SymbolID, Symbol>,
    rules: IndexMap<RuleID, RuleInner>,
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
            .find(|(_, sym)| sym.name == added.name)
        {
            Some((id, sym)) => {
                if sym.kind != added.kind {
                    return Err(GrammarDefError {
                        msg: format!("conflict symbol kind (name={})", sym.name),
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
        name: impl Into<Cow<'static, str>>,
    ) -> Result<SymbolID, GrammarDefError> {
        self.add_symbol(Symbol {
            name: name.into(),
            kind: SymbolKind::Terminal,
        })
    }

    /// Specify a nonterminal symbol used in this grammar.
    pub fn symbol(
        &mut self,
        name: impl Into<Cow<'static, str>>,
    ) -> Result<SymbolID, GrammarDefError> {
        self.add_symbol(Symbol {
            name: name.into(),
            kind: SymbolKind::Nonterminal,
        })
    }

    /// Register a production rule into this grammer.
    pub fn rule<I>(&mut self, left: SymbolID, right: I) -> Result<RuleID, GrammarDefError>
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

        let export_name = format!(
            "{symbol}_{num}",
            symbol = self.symbols[&left].name,
            num = self.rules.values().filter(|r| r.left == left).count()
        );

        let id = RuleID::new(self.next_rule_id);
        self.next_rule_id += 1;
        self.rules.insert(
            id,
            RuleInner {
                left,
                right: right.into_iter().collect(),
                export_name,
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
            accept_rule: RuleInner {
                left: SymbolID::ACCEPT,
                right: vec![start],
                export_name: "*****".into(), // never used
            },
        })
    }
}

#[derive(Debug)]
pub struct GrammarDefError {
    #[allow(dead_code)]
    msg: String,
}
