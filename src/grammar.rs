//! Grammar types.

use indexmap::{IndexMap, IndexSet};
use std::{fmt, mem};

pub type Symbol = &'static str;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct RuleID(usize);
impl RuleID {
    pub const START: Self = Self(usize::MAX);
    const fn new(i: usize) -> Self {
        Self(i)
    }
}
impl fmt::Display for RuleID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &Self::START => write!(f, "START"),
            Self(id) => fmt::Display::fmt(id, f),
        }
    }
}

#[derive(Debug)]
pub struct Rule {
    pub lhs: Symbol,
    pub rhs: Vec<Symbol>,
}
impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : ", self.lhs)?;
        for (i, symbol) in self.rhs.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", symbol)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Grammar {
    pub terminals: IndexSet<Symbol>,
    pub nonterminals: IndexSet<Symbol>,
    pub rules: IndexMap<RuleID, Rule>,
    pub start: Symbol,
}

impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "terminals: ")?;
        for (i, sym) in self.terminals.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", sym)?;
        }
        write!(f, "\nnonterminals: ")?;
        for (i, sym) in self.nonterminals.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", sym)?;
        }
        write!(f, "\nrules:\n")?;
        for (id, rule) in &self.rules {
            writeln!(f, "  [{:02}] {}", id, rule)?;
        }
        write!(f, "start: {}", self.start)?;
        Ok(())
    }
}

impl Grammar {
    pub fn builder() -> Builder {
        Builder::default()
    }
}

/// A builder object for `Grammar`.
#[derive(Debug, Default)]
pub struct Builder {
    // A collection of terminal symbols.
    terminals: Vec<Symbol>,
    rules: Vec<Rule>,
    start: Option<Symbol>,
}

impl Builder {
    /// Register some terminal symbols into this grammar.
    pub fn terminals(&mut self, tokens: &[Symbol]) -> &mut Self {
        self.terminals.extend(tokens);
        self
    }

    /// Register a syntax rule into this grammer.
    ///
    /// The first argument `name` means the name of a non-terminal symbol,
    /// and the remaining `args` is
    pub fn rule(&mut self, name: Symbol, symbols: &[Symbol]) -> &mut Self {
        self.rules.push(Rule {
            lhs: name,
            rhs: symbols.into_iter().copied().collect(),
        });
        self
    }

    /// Specify the start symbol.
    pub fn start(&mut self, name: Symbol) -> &mut Self {
        self.start.replace(name);
        self
    }

    pub fn build(&mut self) -> Grammar {
        let Self {
            terminals: terminals_vec,
            rules: rules_vec,
            start,
            ..
        } = mem::take(self);

        let mut terminals: IndexSet<Symbol> = IndexSet::new();
        terminals.insert("$");
        terminals.extend(terminals_vec);

        let nonterminals: IndexSet<_> = rules_vec.iter().map(|rule| rule.lhs).collect();
        let start = start.or_else(|| nonterminals.first().copied()).unwrap();

        // rule内にterminal symbolでもnonterminal symbolでもないものが含まれていないかをチェック
        assert!(rules_vec.len() <= usize::MAX / 2, "too many rules");
        for rule in &rules_vec {
            let mut unknown_symbols = rule.rhs.iter().filter_map(|arg| {
                (!terminals.contains(arg) && !nonterminals.contains(arg)).then(|| arg)
            });
            if let Some(arg) = unknown_symbols.next() {
                panic!("unexpected symbol found in syntax rule: {}", arg);
            }
        }

        let rules = Some((
            RuleID::START,
            Rule {
                lhs: "$START", // dummy symbol for representing the starting point.
                rhs: vec![start],
            },
        ))
        .into_iter()
        .chain(
            rules_vec
                .into_iter()
                .enumerate()
                .map(|(i, rule)| (RuleID::new(i), rule)),
        )
        .collect();

        Grammar {
            terminals,
            nonterminals,
            rules,
            start,
        }
    }
}
