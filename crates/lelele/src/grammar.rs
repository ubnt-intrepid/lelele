//! Grammar types.

use indexmap::IndexMap;
use std::{borrow::Cow, fmt};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SymbolID {
    raw: u64,
}

impl SymbolID {
    pub const EOI: Self = Self::new(u64::MAX);
    pub const START: Self = Self::new(u64::MAX - 1);

    const fn new(raw: u64) -> Self {
        Self { raw }
    }

    pub(crate) const fn raw(self) -> u64 {
        self.raw
    }
}

impl fmt::Display for SymbolID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &Self::EOI => write!(f, "$end"),
            &Self::START => write!(f, "$start"),
            Self { raw } => fmt::Display::fmt(raw, f),
        }
    }
}

#[derive(Debug)]
pub struct Symbol<'g> {
    name: Cow<'g, str>,
    kind: SymbolKind,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum SymbolKind {
    Terminal,
    Nonterminal,
}

impl<'g> Symbol<'g> {
    const EOI: Self = Self {
        name: Cow::Borrowed("$EOI"),
        kind: SymbolKind::Terminal,
    };
    const START: Self = Self {
        name: Cow::Borrowed("$START"),
        kind: SymbolKind::Nonterminal,
    };

    pub fn name(&self) -> &str {
        &*self.name
    }

    pub fn is_terminal(&self) -> bool {
        matches!(self.kind, SymbolKind::Terminal)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct RuleID {
    raw: u64,
}
impl RuleID {
    pub const START: Self = Self::new(u64::MAX);
    const fn new(raw: u64) -> Self {
        Self { raw }
    }
    pub(crate) const fn raw(self) -> u64 {
        self.raw
    }
}
impl fmt::Display for RuleID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &Self::START => write!(f, "START"),
            Self { raw } => fmt::Display::fmt(raw, f),
        }
    }
}

#[derive(Debug)]
pub struct Rule<'g> {
    pub name: Cow<'g, str>,
    pub lhs: SymbolID,
    pub rhs: Vec<SymbolID>,
}
impl Rule<'_> {
    pub fn display<'r>(&'r self, grammar: &'r Grammar<'r>) -> impl fmt::Display + 'r {
        struct RuleDisplay<'r> {
            rule: &'r Rule<'r>,
            grammar: &'r Grammar<'r>,
        }
        impl fmt::Display for RuleDisplay<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let Self { rule, grammar } = self;
                write!(f, "{} : ", grammar.symbol(rule.lhs).name)?;
                for (i, symbol) in rule.rhs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", grammar.symbol(*symbol).name)?;
                }
                Ok(())
            }
        }
        RuleDisplay {
            rule: self,
            grammar,
        }
    }
}

#[derive(Debug)]
pub struct Grammar<'g> {
    symbols: IndexMap<SymbolID, Symbol<'g>>,
    rules: IndexMap<RuleID, Rule<'g>>,
    start: SymbolID,
    start_rule: Rule<'g>,
}

impl fmt::Display for Grammar<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "terminals: ")?;
        for (i, (_, sym)) in self.terminals().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", sym.name)?;
        }
        write!(f, "\nnonterminals: ")?;
        for (i, (_, sym)) in self.nonterminals().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", sym.name)?;
        }
        write!(f, "\nrules:\n")?;
        for (id, rule) in self.rules() {
            writeln!(f, "  [{:02}] {}", id, rule.display(self))?;
        }
        write!(f, "start: {}", self.symbol(self.start).name)?;
        Ok(())
    }
}

impl<'g> Grammar<'g> {
    pub fn definition() -> GrammarDef<'g> {
        GrammarDef {
            symbols: IndexMap::new(),
            rules: IndexMap::new(),
            start: None,
            next_symbol_id: 0,
            next_rule_id: 0,
        }
    }

    pub fn symbols(&self) -> impl Iterator<Item = (SymbolID, &Symbol<'g>)> + '_ {
        [
            (SymbolID::EOI, &Symbol::EOI),
            (SymbolID::START, &Symbol::START),
        ]
        .into_iter()
        .chain(self.symbols.iter().map(|(id, sym)| (*id, sym)))
    }

    pub fn terminals(&self) -> impl Iterator<Item = (SymbolID, &Symbol<'g>)> + '_ {
        self.symbols()
            .filter(|(_id, sym)| sym.kind == SymbolKind::Terminal)
    }

    pub fn nonterminals(&self) -> impl Iterator<Item = (SymbolID, &Symbol<'g>)> + '_ {
        self.symbols()
            .filter(|(_id, sym)| sym.kind == SymbolKind::Nonterminal)
    }

    pub fn symbol(&self, id: SymbolID) -> &Symbol<'g> {
        match id {
            SymbolID::EOI => &Symbol::EOI,
            SymbolID::START => &Symbol::START,
            id => &self.symbols[&id],
        }
    }

    pub fn symbol_id(&self, name: &str) -> Option<SymbolID> {
        self.symbols
            .iter()
            .find_map(|(id, sym)| (sym.name == name).then_some(*id))
    }

    pub fn rules(&self) -> impl Iterator<Item = (RuleID, &Rule)> + '_ {
        Some((RuleID::START, &self.start_rule))
            .into_iter()
            .chain(self.rules.iter().map(|(id, rule)| (*id, rule)))
    }

    pub fn rule(&self, id: RuleID) -> &Rule {
        match id {
            RuleID::START => &self.start_rule,
            id => &self.rules.get(&id).unwrap(),
        }
    }
}

/// A builder object for `Grammar`.
#[derive(Debug)]
pub struct GrammarDef<'g> {
    symbols: IndexMap<SymbolID, Symbol<'g>>,
    rules: IndexMap<RuleID, Rule<'g>>,
    start: Option<SymbolID>,
    next_symbol_id: u64,
    next_rule_id: u64,
}

impl<'g> GrammarDef<'g> {
    fn add_symbol(&mut self, added: Symbol<'g>) -> SymbolID {
        match self
            .symbols
            .iter_mut()
            .find(|(_, sym)| sym.name == added.name)
        {
            Some((id, sym)) => {
                debug_assert!(
                    sym.kind == added.kind,
                    "conflict symbol kind (name={})",
                    sym.name
                );
                *id
            }
            None => {
                let id = SymbolID::new(self.next_symbol_id);
                self.next_symbol_id += 1;
                self.symbols.insert(id, added);
                id
            }
        }
    }

    fn add_rule(&mut self, rule: Rule<'g>) -> RuleID {
        let id = RuleID::new(self.next_rule_id);
        self.next_rule_id += 1;
        self.rules.insert(id, rule);
        id
    }

    /// Specify the terminal symbol used in this grammar.
    pub fn token(&mut self, name: impl Into<Cow<'g, str>>) -> SymbolID {
        self.add_symbol(Symbol {
            name: name.into(),
            kind: SymbolKind::Terminal,
        })
    }

    /// Specify the nonterminal symbol used in this grammar.
    pub fn symbol(&mut self, name: impl Into<Cow<'g, str>>) -> SymbolID {
        self.add_symbol(Symbol {
            name: name.into(),
            kind: SymbolKind::Nonterminal,
        })
    }

    /// Register a syntax rule into this grammer.
    ///
    /// The first argument `name` means the name of a non-terminal symbol,
    /// and the remaining `args` is
    pub fn rule<I>(&mut self, name: impl Into<Cow<'g, str>>, lhs: SymbolID, rhs: I) -> RuleID
    where
        I: IntoIterator<Item = SymbolID>,
    {
        debug_assert!(
            self.symbols
                .get(&lhs)
                .map_or(false, |lhs| lhs.kind == SymbolKind::Nonterminal),
            "lhs must be nonterminal symbol"
        );
        self.add_rule(Rule {
            name: name.into(),
            lhs,
            rhs: rhs.into_iter().collect(),
        })
    }

    /// Specify the start symbol.
    pub fn start(&mut self, symbol: SymbolID) {
        self.start.replace(symbol.into());
    }

    pub fn end(mut self) -> Grammar<'g> {
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

        Grammar {
            symbols: self.symbols,
            rules: self.rules,
            start,
            start_rule: Rule {
                name: "$START".into(),
                lhs: SymbolID::START,
                rhs: vec![start],
            },
        }
    }
}
