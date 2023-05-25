//! Grammar types.

use indexmap::IndexMap;
use std::{borrow::Cow, fmt, mem};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SymbolID {
    inner: usize,
}

impl SymbolID {
    pub const EOI: Self = Self::new(usize::MAX);
    pub const START: Self = Self::new(usize::MAX - 1);

    const fn new(i: usize) -> Self {
        Self { inner: i }
    }
}

impl fmt::Display for SymbolID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &Self::EOI => write!(f, "$end"),
            &Self::START => write!(f, "$start"),
            Self { inner } => fmt::Display::fmt(inner, f),
        }
    }
}

#[derive(Debug)]
pub struct Symbol<'g> {
    pub name: Cow<'g, str>,
    pub kind: SymbolKind,
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
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum SymbolKind {
    Terminal,
    Nonterminal,
    Unspecified,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct RuleID {
    inner: usize,
}
impl RuleID {
    pub const START: Self = Self::new(usize::MAX);
    const fn new(i: usize) -> Self {
        Self { inner: i }
    }
}
impl fmt::Display for RuleID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &Self::START => write!(f, "START"),
            Self { inner } => fmt::Display::fmt(inner, f),
        }
    }
}

#[derive(Debug)]
pub struct Rule {
    pub lhs: SymbolID,
    pub rhs: Vec<SymbolID>,
}
impl Rule {
    pub fn display<'r>(&'r self, grammar: &'r Grammar) -> impl fmt::Display + 'r {
        struct RuleDisplay<'r> {
            rule: &'r Rule,
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
    rules: IndexMap<RuleID, Rule>,
    start: SymbolID,
    start_rule: Rule,
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
    pub fn builder() -> Builder<'g> {
        Builder::default()
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
#[derive(Debug, Default)]
pub struct Builder<'g> {
    terminals: Vec<Cow<'g, str>>,
    rules: Vec<(Cow<'g, str>, Vec<Cow<'g, str>>)>,
    start: Option<Cow<'g, str>>,
}

impl<'g> Builder<'g> {
    /// Register some terminal symbols into this grammar.
    pub fn terminals<I>(&mut self, symbols: I) -> &mut Self
    where
        I: IntoIterator,
        I::Item: Into<Cow<'g, str>>,
    {
        self.terminals.extend(symbols.into_iter().map(Into::into));
        self
    }

    /// Register a syntax rule into this grammer.
    ///
    /// The first argument `name` means the name of a non-terminal symbol,
    /// and the remaining `args` is
    pub fn rule<I, T>(&mut self, lhs: T, rhs: I) -> &mut Self
    where
        T: Into<Cow<'static, str>>,
        I: IntoIterator,
        I::Item: Into<Cow<'g, str>>,
    {
        self.rules
            .push((lhs.into(), rhs.into_iter().map(Into::into).collect()));
        self
    }

    /// Specify the start symbol.
    pub fn start<T>(&mut self, symbol: T) -> &mut Self
    where
        T: Into<Cow<'static, str>>,
    {
        self.start.replace(symbol.into());
        self
    }

    pub fn build(&mut self) -> Grammar<'g> {
        let Self {
            terminals,
            rules: rules_vec,
            start,
            ..
        } = mem::take(self);

        // 文法内に登場するsymbolとIDの対応表
        let mut symbols: IndexMap<SymbolID, Symbol> = IndexMap::new();
        // symbolがなければ追加し、IDと紐付ける。そうでない場合は登録されたsymbolのIDを返す
        let mut add_symbol = {
            let mut next_symbol_id = 0;
            move |symbols: &mut IndexMap<SymbolID, Symbol<'g>>, added: Symbol<'g>| {
                //
                match symbols.iter_mut().find(|(_, sym)| sym.name == added.name) {
                    Some((id, sym)) => {
                        match (sym.kind, added.kind) {
                            (SymbolKind::Unspecified, k) => sym.kind = k, // overwrite
                            (_, SymbolKind::Unspecified) => (),           // do nothing
                            (a, b) => {
                                debug_assert!(a == b, "conflict symbol kind ({:?}, {:?})", a, b)
                            }
                        }
                        *id
                    }
                    None => {
                        let id = SymbolID::new(next_symbol_id);
                        next_symbol_id += 1;
                        symbols.insert(id, added);
                        id
                    }
                }
            }
        };

        // terminal symbolsの登録
        for name in terminals {
            add_symbol(
                &mut symbols,
                Symbol {
                    name,
                    kind: SymbolKind::Terminal,
                },
            );
        }

        // rulesの登録
        let mut rules: IndexMap<RuleID, Rule> = IndexMap::new();
        let mut add_rule = {
            let mut next_rule_id = 0;
            move |rules: &mut IndexMap<RuleID, Rule>, rule: Rule| {
                let id = RuleID::new(next_rule_id);
                next_rule_id += 1;
                rules.insert(id, rule);
                id
            }
        };
        for (lhs_vec, rhs_vec) in rules_vec {
            let lhs = add_symbol(
                &mut symbols,
                Symbol {
                    name: lhs_vec,
                    kind: SymbolKind::Nonterminal, // 左辺は常にnonterminal symbol
                },
            );

            let mut rhs = vec![];
            for name in rhs_vec {
                rhs.push(add_symbol(
                    &mut symbols,
                    Symbol {
                        name,
                        kind: SymbolKind::Unspecified, // すべてのruleを走査し終えるまでは未定
                    },
                ));
            }

            add_rule(&mut rules, Rule { lhs, rhs });
        }

        // start symbolのID変換
        // 指定されていない場合は最初に登録されたnonterminal symbolを用いる
        let start = match start {
            Some(name) => add_symbol(
                &mut symbols,
                Symbol {
                    name,
                    kind: SymbolKind::Nonterminal,
                },
            ),
            None => symbols
                .iter()
                .find_map(|(id, sym)| (sym.kind == SymbolKind::Nonterminal).then_some(*id))
                .unwrap(),
        };

        // rulesの左辺に一度も登場しないsymbolはterminal symbolだとみなす
        for symbol in symbols.values_mut() {
            if symbol.kind == SymbolKind::Unspecified {
                symbol.kind = SymbolKind::Terminal;
            }
        }

        Grammar {
            symbols,
            rules,
            start,
            start_rule: Rule {
                lhs: SymbolID::START,
                rhs: vec![start],
            },
        }
    }
}
