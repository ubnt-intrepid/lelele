//! An LR(1) parser generator.

use indexmap::{IndexMap, IndexSet};
use std::{fmt, mem};

pub type Symbol = &'static str;

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
    pub rules: Vec<Rule>,
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
        for (i, rule) in self.rules.iter().enumerate() {
            writeln!(f, "  [{:02}] {}", i, rule)?;
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
            .iter()
            .filter_map(|rule| rule.rhs.is_empty().then(|| rule.lhs))
            .collect();

        // 値が更新されなくなるまで繰り返す
        let mut changed = true;
        while changed {
            changed = false;
            for rule in &self.rules {
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
        struct Constraint {
            sup: Symbol,
            sub: Symbol,
        }
        let mut constraints = vec![];
        for rule in &self.rules {
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
            rules,
            start,
            ..
        } = mem::take(self);

        let nonterminals: IndexSet<_> = rules.iter().map(|rule| rule.lhs).collect();
        let start = start.or_else(|| nonterminals.first().copied()).unwrap();

        // rule内にterminal symbolでもnonterminal symbolでもないものが含まれていないかをチェック
        for rule in &rules {
            let mut unknown_symbols = rule.rhs.iter().filter_map(|arg| {
                (!terminals.contains(arg) && !nonterminals.contains(arg)).then(|| arg)
            });
            if let Some(arg) = unknown_symbols.next() {
                panic!("unexpected symbol found in syntax rule: {}", arg);
            }
        }

        Grammar {
            terminals,
            nonterminals,
            rules,
            start,
        }
    }
}
