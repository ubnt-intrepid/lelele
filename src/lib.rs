//! An LR(1) parser generator.

use indexmap::{IndexMap, IndexSet};
use std::{fmt, mem};

#[derive(Debug)]
pub struct Grammar {
    pub terminals: IndexSet<&'static str>,
    pub nonterminals: IndexSet<&'static str>,
    pub rules: Vec<(&'static str, Vec<&'static str>)>,
    pub start: &'static str,
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
        for (name, args) in &self.rules {
            write!(f, "  - {} -> ", name)?;
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}", arg)?;
            }
            writeln!(f)?;
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
    fn nulls_set(&self) -> IndexSet<&'static str> {
        // ruleからnullableであることが分かっている場合は追加する
        let mut nulls: IndexSet<&str> = self
            .rules
            .iter()
            .filter_map(|(name, arg)| arg.is_empty().then(|| *name))
            .collect();

        // 値が更新されなくなるまで繰り返す
        let mut changed = true;
        while changed {
            changed = false;
            for (ltoken, pattern) in &self.rules {
                if nulls.contains(ltoken) {
                    continue;
                }
                // 右辺のsymbolsがすべてnullableかどうか
                let is_rhs_nullable = pattern.iter().all(|t| nulls.contains(t));
                if is_rhs_nullable {
                    changed = true;
                    nulls.insert(ltoken);
                    continue;
                }
            }
        }

        nulls
    }

    /// a
    pub fn first_set(&self) -> FirstSet<'_> {
        let mut map: IndexMap<&str, IndexSet<&str>> = IndexMap::new();
        // EOF
        map.insert("$", Some("$").into_iter().collect());

        // terminal symbols
        for tok in &self.terminals {
            map.insert(tok, Some(*tok).into_iter().collect());
        }

        // nonterminal symbols
        for tok in &self.nonterminals {
            map.insert(tok, IndexSet::new());
        }

        // create nulls set
        let nulls = self.nulls_set();

        // extract constraint
        let mut constraints = vec![];
        for (sup, subs) in &self.rules {
            for sub in subs {
                if sup != sub {
                    constraints.push((sup, sub));
                }
                if !nulls.contains(sub) {
                    break;
                }
            }
        }

        // resolve constraints
        let mut changed = true;
        while changed {
            changed = false;

            for (sup, sub) in &constraints {
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

        FirstSet {
            grammar: self,
            map,
            nulls,
        }
    }
}

#[derive(Debug)]
pub struct FirstSet<'g> {
    pub grammar: &'g Grammar,
    pub map: IndexMap<&'static str, IndexSet<&'static str>>,
    pub nulls: IndexSet<&'static str>,
}
impl FirstSet<'_> {
    pub fn get(&self, tokens: &[&'static str]) -> IndexSet<&'static str> {
        match tokens {
            [token] => self.map.get(token).expect("invalid token provided").clone(),
            tokens => {
                if tokens.iter().any(|t| !self.map.contains_key(t)) {
                    panic!("invalid token provided");
                }
                let mut res = IndexSet::new();
                for token in tokens {
                    let add = self.map.get(token).unwrap();
                    res.extend(add.iter().copied());
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
    terminals: IndexSet<&'static str>,
    rules: Vec<(&'static str, Vec<&'static str>)>,
    start: Option<&'static str>,
}

impl Builder {
    /// Register some terminal symbols into this grammar.
    pub fn terminals(&mut self, tokens: &[&'static str]) -> &mut Self {
        self.terminals.extend(tokens);
        self
    }

    /// Register a syntax rule into this grammer.
    ///
    /// The first argument `name` means the name of a non-terminal symbol,
    /// and the remaining `args` is
    pub fn rule(&mut self, name: &'static str, symbols: &[&'static str]) -> &mut Self {
        self.rules
            .push((name, symbols.into_iter().copied().collect()));
        self
    }

    /// Specify the start symbol.
    pub fn start(&mut self, name: &'static str) -> &mut Self {
        self.start.replace(name);
        self
    }

    pub fn build(&mut self) -> Grammar {
        let Self {
            terminals,
            rules,
            start: goal,
            ..
        } = mem::take(self);

        let terminals: IndexSet<_> = Some("$").into_iter().chain(terminals).collect();

        let nonterminals: IndexSet<_> = rules.iter().map(|(name, _)| *name).collect();

        // rule内にterminal symbolでもnonterminal symbolでもないものが含まれていないかをチェック
        for (_, args) in &rules {
            let mut unknown_symbols = args.iter().filter_map(|arg| {
                (!terminals.contains(arg) && !nonterminals.contains(arg)).then(|| arg)
            });
            if let Some(arg) = unknown_symbols.next() {
                panic!("unexpected symbol found in syntax rule: {}", arg);
            }
        }

        let goal = goal.or_else(|| nonterminals.first().copied()).unwrap();

        Grammar {
            terminals,
            nonterminals,
            rules,
            start: goal,
        }
    }
}
