//! Grammar types.

use indexmap::IndexMap;
use std::{borrow::Cow, fmt};

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SymbolID {
    raw: u64,
}

impl SymbolID {
    /// Reserved symbol used as a terminal symbol that means the end of input.
    pub(crate) const EOI: Self = Self { raw: u64::MAX };
    /// Reserved symbol used as a nonterminal symbol to match when the parsing is complete.
    pub(crate) const ACCEPT: Self = Self { raw: u64::MAX - 1 };

    #[inline]
    const fn new(raw: u64) -> Self {
        assert!(raw < u64::MAX / 2, "too large SymbolID");
        Self { raw }
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
        name: Cow::Borrowed("$eoi"),
        kind: SymbolKind::Terminal,
    };
    const ACCEPT: Self = Self {
        name: Cow::Borrowed("$accept"),
        kind: SymbolKind::Nonterminal,
    };
}

impl<'g> Symbol<'g> {
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
    /// Reserved ID that represents the top-level rule
    /// `$accept : <start-symbol> $`.
    pub(crate) const ACCEPT: Self = Self { raw: u64::MAX };

    #[inline]
    const fn new(raw: u64) -> Self {
        assert!(raw < u64::MAX / 2, "too large RuleID");
        Self { raw }
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

#[derive(Debug)]
pub struct Rule<'g> {
    grammar: &'g Grammar<'g>,
    inner: &'g RuleInner,
}

#[derive(Debug)]
struct RuleInner {
    start: SymbolID,
    production: Vec<SymbolID>,
}

impl fmt::Display for Rule<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            grammar,
            inner: RuleInner { start, production },
        } = self;
        write!(f, "{} : ", grammar.symbol(*start).name())?;
        for (i, symbol) in production.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", grammar.symbol(*symbol).name())?;
        }
        Ok(())
    }
}

impl Rule<'_> {
    pub fn start(&self) -> SymbolID {
        self.inner.start
    }

    pub fn production(&self) -> &[SymbolID] {
        &self.inner.production[..]
    }
}

/// The grammar definition used to derive the parser tables.
#[derive(Debug)]
pub struct Grammar<'g> {
    symbols: IndexMap<SymbolID, Symbol<'g>>,
    rules: IndexMap<RuleID, RuleInner>,
    start: SymbolID,
    start_rule: RuleInner,
}

impl fmt::Display for Grammar<'_> {
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
        write!(f, "\nrules:\n")?;
        for (id, rule) in self.rules() {
            writeln!(f, "  [{:02}] {}", id, rule)?;
        }
        write!(f, "start: {}", self.symbol(self.start).name())?;
        Ok(())
    }
}

impl<'g> Grammar<'g> {
    /// Define a grammar using the specified function.
    pub fn define<F>(f: F) -> Self
    where
        F: FnOnce(&mut GrammarDef<'g>),
    {
        let mut def = GrammarDef {
            symbols: IndexMap::new(),
            rules: IndexMap::new(),
            start: None,
            next_symbol_id: 0,
            next_rule_id: 0,
        };

        f(&mut def);

        def.end()
    }

    pub fn symbols(&self) -> impl Iterator<Item = (SymbolID, &Symbol<'g>)> + '_ {
        [
            (SymbolID::EOI, &Symbol::EOI),
            (SymbolID::ACCEPT, &Symbol::ACCEPT),
        ]
        .into_iter()
        .chain(self.symbols.iter().map(|(id, sym)| (*id, sym)))
    }

    pub fn terminals(&self) -> impl Iterator<Item = (SymbolID, &Symbol<'g>)> + '_ {
        self.symbols().filter(|(_id, sym)| sym.is_terminal())
    }

    pub fn nonterminals(&self) -> impl Iterator<Item = (SymbolID, &Symbol<'g>)> + '_ {
        self.symbols().filter(|(_id, sym)| !sym.is_terminal())
    }

    pub fn symbol(&'g self, id: SymbolID) -> &Symbol<'g> {
        match id {
            SymbolID::EOI => &Symbol::EOI,
            SymbolID::ACCEPT => &Symbol::ACCEPT,
            id => &self.symbols[&id],
        }
    }

    pub fn symbol_id(&self, name: &str) -> Option<SymbolID> {
        self.symbols
            .iter()
            .find_map(|(id, sym)| (sym.name == name).then_some(*id))
    }

    pub fn rules(&self) -> impl Iterator<Item = (RuleID, Rule<'_>)> + '_ {
        Some((
            RuleID::ACCEPT,
            Rule {
                grammar: self,
                inner: &self.start_rule,
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
            RuleID::ACCEPT => &self.start_rule,
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
pub struct GrammarDef<'g> {
    symbols: IndexMap<SymbolID, Symbol<'g>>,
    rules: IndexMap<RuleID, RuleInner>,
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

    fn add_rule(&mut self, rule: RuleInner) -> RuleID {
        let id = RuleID::new(self.next_rule_id);
        self.next_rule_id += 1;
        self.rules.insert(id, rule);
        id
    }

    /// Specify a terminal symbol used in this grammar.
    pub fn token(&mut self, name: impl Into<Cow<'g, str>>) -> SymbolID {
        self.add_symbol(Symbol {
            name: name.into(),
            kind: SymbolKind::Terminal,
        })
    }

    /// Specify a nonterminal symbol used in this grammar.
    pub fn symbol(&mut self, name: impl Into<Cow<'g, str>>) -> SymbolID {
        self.add_symbol(Symbol {
            name: name.into(),
            kind: SymbolKind::Nonterminal,
        })
    }

    /// Register a production rule into this grammer.
    pub fn rule<I>(&mut self, start: SymbolID, production: I) -> RuleID
    where
        I: IntoIterator<Item = SymbolID>,
    {
        debug_assert!(
            self.symbols
                .get(&start)
                .map_or(false, |lhs| lhs.kind == SymbolKind::Nonterminal),
            "The starting symbol in production rule must be nonterminal"
        );
        self.add_rule(RuleInner {
            start,
            production: production.into_iter().collect(),
        })
    }

    /// Specify the start symbol.
    pub fn start_symbol(&mut self, symbol: SymbolID) {
        debug_assert!(
            self.symbols
                .get(&symbol)
                .map_or(false, |s| s.kind == SymbolKind::Nonterminal),
            "the start symbol must be nonterminal"
        );
        self.start.replace(symbol);
    }

    fn end(mut self) -> Grammar<'g> {
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
            start_rule: RuleInner {
                start: SymbolID::ACCEPT,
                production: vec![start],
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::ParserDefinition;

    #[test]
    fn smoketest() {
        let grammar = Grammar::define(|def| {
            let lparen = def.token("LPAREN");
            let rparen = def.token("RPAREN");
            let plus = def.token("PLUS");
            let minus = def.token("MINUS");
            let star = def.token("STAR");
            let slash = def.token("SLASH");
            let num = def.token("NUM");

            let expr = def.symbol("EXPR");
            let factor = def.symbol("FACTOR");
            let term = def.symbol("TERM");

            def.start_symbol(expr);

            def.rule(expr, [expr, plus, factor]);
            def.rule(expr, [expr, minus, factor]);
            def.rule(expr, [factor]);

            def.rule(factor, [factor, star, term]);
            def.rule(factor, [factor, slash, term]);
            def.rule(factor, [term]);

            def.rule(term, [num]);
            def.rule(term, [lparen, expr, rparen]);
        });
        eprintln!("{}", grammar);

        let parser_def = ParserDefinition::new(&grammar);
        eprintln!("{:#?}", parser_def);
    }
}
