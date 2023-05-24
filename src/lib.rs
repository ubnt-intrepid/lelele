//! An LR(1) parser generator.

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
        writeln!(f, "start: {}", self.start)?;
        Ok(())
    }
}

impl Grammar {
    pub fn builder() -> Builder {
        Builder::default()
    }

    /// Calculate the set of nullable symbols in this grammar.
    fn nulls_set(&self) -> IndexSet<Symbol> {
        // ruleからnullableであることが分かっている場合は追加する
        let mut nulls: IndexSet<Symbol> = self
            .rules
            .values()
            .filter_map(|rule| rule.rhs.is_empty().then(|| rule.lhs))
            .collect();

        // 値が更新されなくなるまで繰り返す
        let mut changed = true;
        while changed {
            changed = false;
            for rule in self.rules.values() {
                if nulls.contains(rule.lhs) {
                    continue;
                }
                // 右辺のsymbolsがすべてnullableかどうか
                let is_rhs_nullable = rule.rhs.iter().all(|t| nulls.contains(t));
                if is_rhs_nullable {
                    changed = true;
                    nulls.insert(rule.lhs);
                    continue;
                }
            }
        }

        nulls
    }

    /// Constructs the instance for calculating first sets in this grammar.
    pub fn first_set(&self) -> FirstSet {
        let mut map: IndexMap<Symbol, IndexSet<Symbol>> = IndexMap::new();
        let nulls = self.nulls_set();

        // terminal symbols については First(T) = {T} になる
        for tok in &self.terminals {
            map.insert(tok, Some(*tok).into_iter().collect());
        }

        // nonterminal symbols に関する初期値
        for tok in &self.nonterminals {
            map.insert(tok, IndexSet::new());
        }

        // 制約条件の抽出
        // X -> Y1 Y2 ... Yn という構文規則に対し、
        //  1. Y1,Y2,...と検索していき、最初に来る非nullableな記号を Yk とする
        //    - Y1 Y2 ... Y(k-1) が nullable で Yk が non-nullable となる
        //  2. Yi (i=1,2,..,k) それぞれに対し First(X) \supseteq First(Yi) という制約を追加する
        #[derive(Debug)]
        struct Constraint {
            sup: Symbol,
            sub: Symbol,
        }
        let mut constraints = vec![];
        for rule in self
            .rules
            .iter()
            .flat_map(|(id, rule)| (*id != RuleID::START).then(|| rule))
        {
            for symbol in &rule.rhs {
                if rule.lhs != *symbol {
                    constraints.push(Constraint {
                        sup: rule.lhs,
                        sub: symbol,
                    });
                }
                if !nulls.contains(symbol) {
                    break;
                }
            }
        }

        // 制約条件の解消
        // First(A) \subseteq First(B) が満たされるよう First(A) の要素を First(B) に追加するだけ。
        // これをすべての制約条件に対して繰り返す
        let mut changed = true;
        while changed {
            changed = false;

            for Constraint { sup, sub } in &constraints {
                let mut superset = map.remove(*sup).unwrap();
                let subset = map.get(*sub).unwrap();

                for tok in subset {
                    if !superset.contains(tok) {
                        superset.insert(*tok);
                        changed = true;
                    }
                }

                map.insert(sup, superset);
            }
        }

        FirstSet { map, nulls }
    }
}

#[derive(Debug)]
pub struct FirstSet {
    map: IndexMap<Symbol, IndexSet<Symbol>>,
    nulls: IndexSet<Symbol>,
}

impl FirstSet {
    /// Calculate the first set of specified symbols.
    pub fn get(&self, tokens: &[Symbol]) -> IndexSet<Symbol> {
        match tokens {
            [token] => self.map.get(token).expect("invalid token provided").clone(),
            tokens => {
                if tokens.iter().any(|t| !self.map.contains_key(t)) {
                    panic!("invalid token provided");
                }
                let mut res = IndexSet::new();
                for token in tokens {
                    let added = self.map.get(token).expect("invalid token provided");
                    res.extend(added.iter().copied());
                    if !self.nulls.contains(token) {
                        break;
                    }
                }
                res
            }
        }
    }
}

/// A builder object for `Grammar`.
#[derive(Debug, Default)]
pub struct Builder {
    // A collection of terminal symbols.
    terminals: IndexSet<Symbol>,
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
            terminals,
            rules: rules_vec,
            start,
            ..
        } = mem::take(self);

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
