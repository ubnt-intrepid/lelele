//! Grammar types.

use indexmap::{IndexMap, IndexSet};
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
            grammar: &'r Grammar,
        }
        impl fmt::Display for RuleDisplay<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let Self { rule, grammar } = self;
                write!(f, "{} : ", grammar.symbol_name(rule.lhs))?;
                for (i, symbol) in rule.rhs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", grammar.symbol_name(*symbol))?;
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
pub struct Grammar {
    pub symbols: IndexMap<SymbolID, Cow<'static, str>>,
    pub rules: IndexMap<RuleID, Rule>,
    pub terminals: IndexSet<SymbolID>,
    pub nonterminals: IndexSet<SymbolID>,
    pub start: SymbolID,
}

impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "terminals: ")?;
        for (i, sym) in self.terminals.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", self.symbols[sym])?;
        }
        write!(f, "\nnonterminals: ")?;
        for (i, sym) in self.nonterminals.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", self.symbol_name(*sym))?;
        }
        write!(f, "\nrules:\n")?;
        for (id, rule) in &self.rules {
            writeln!(f, "  [{:02}] {}", id, rule.display(self))?;
        }
        write!(f, "start: {}", self.symbol_name(self.start))?;
        Ok(())
    }
}

impl Grammar {
    pub fn builder() -> Builder {
        Builder::default()
    }

    pub fn symbol_name(&self, id: SymbolID) -> &str {
        match id {
            SymbolID::EOI => "$end",
            SymbolID::START => "$start",
            id => &*self.symbols[&id],
        }
    }
}

/// A builder object for `Grammar`.
#[derive(Debug, Default)]
pub struct Builder {
    terminals: Vec<Cow<'static, str>>,
    rules: Vec<(Cow<'static, str>, Vec<Cow<'static, str>>)>,
    start: Option<Cow<'static, str>>,
}

impl Builder {
    /// Register some terminal symbols into this grammar.
    pub fn terminals<I>(&mut self, symbols: I) -> &mut Self
    where
        I: IntoIterator,
        I::Item: Into<Cow<'static, str>>,
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
        I::Item: Into<Cow<'static, str>>,
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

    pub fn build(&mut self) -> Grammar {
        let Self {
            terminals: terminals_vec,
            rules: rules_vec,
            start,
            ..
        } = mem::take(self);

        // nonterminal symbolの個数はruleの数以下なので、terminals.len() + rules.len() <= usize::MAX になるようにすれば良い。
        assert!(
            terminals_vec.len() <= usize::MAX / 4,
            "too many terminal symbols"
        );
        assert!(rules_vec.len() <= usize::MAX / 4, "too many rules");

        let mut symbols: IndexMap<SymbolID, Cow<'static, str>> = IndexMap::new();
        symbols.insert(SymbolID::EOI, "$".into());
        let mut next_symbol_id = 0;
        let mut symbol_id = || {
            let id = next_symbol_id;
            next_symbol_id += 1;
            SymbolID::new(id)
        };

        let mut terminals: IndexSet<SymbolID> = IndexSet::new();
        terminals.insert(SymbolID::EOI);
        for terminal in terminals_vec {
            let id = symbol_id();
            symbols.insert(id, terminal);
            terminals.insert(id);
        }

        // ruleのlhsに登場するsymbolをnonterminalとして抽出
        let mut nonterminals: IndexSet<SymbolID> = IndexSet::new();
        // 右辺のsymbolはまだ登場していない可能性があるので保留する
        let mut rules_: Vec<(SymbolID, Vec<Cow<'static, str>>)> = vec![];
        for (lhs, rhs) in rules_vec {
            let id = symbols
                .iter()
                .filter_map(|(id, name)| (*name == lhs).then(|| *id))
                .next()
                .unwrap_or_else(&mut symbol_id);
            symbols.entry(id).or_insert(lhs);
            nonterminals.insert(id);
            rules_.push((id, rhs));
        }

        // start symbolのID変換
        // 指定されていない場合は最初のruleのlhsを使用する
        let start = match start {
            Some(s) => symbols
                .iter()
                .filter_map(|(id, name)| (*name == s).then(|| *id))
                .next()
                .expect("unspecified start symbol"),
            None => nonterminals.first().copied().expect("empty rule"),
        };

        //
        let mut rules: IndexMap<RuleID, Rule> = IndexMap::new();
        rules.insert(
            RuleID::START,
            Rule {
                lhs: SymbolID::START,
                rhs: vec![start],
            },
        );
        for (i, (lhs, rhs)) in rules_.into_iter().enumerate() {
            let rule_id = RuleID::new(i);
            let rhs: Vec<SymbolID> = rhs
                .into_iter()
                .map(|rhs| {
                    symbols
                        .iter()
                        .find_map(|(id, name)| (*name == rhs).then_some(*id))
                        .unwrap()
                })
                .collect();
            rules.insert(rule_id, Rule { lhs, rhs });
        }

        Grammar {
            symbols,
            terminals,
            nonterminals,
            rules,
            start,
        }
    }
}
