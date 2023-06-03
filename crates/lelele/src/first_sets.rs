//! Calculation of first set function.

use crate::grammar::{Grammar, RuleID, SymbolID};
use crate::{IndexMap, IndexSet};

#[derive(Debug)]
pub struct FirstSets {
    nulls: IndexSet<SymbolID>,
    first_sets: IndexMap<SymbolID, IndexSet<SymbolID>>,
}

impl FirstSets {
    pub fn new(grammar: &Grammar) -> Self {
        let nulls = nulls_set(grammar);
        let first_sets = first_set(grammar, &nulls);
        Self { nulls, first_sets }
    }

    /// `First(prefix x)`
    pub fn get(&self, prefix: &[SymbolID], x: SymbolID) -> IndexSet<SymbolID> {
        let mut res = IndexSet::default();
        for token in prefix.iter().chain(Some(&x)) {
            let added = self.first_sets.get(token).expect("unexpected token");
            res.extend(added.iter().copied());
            if !self.nulls.contains(token) {
                break;
            }
        }
        res
    }
}

/// Calculate the set of nullable symbols in this grammar.
fn nulls_set(grammar: &Grammar) -> IndexSet<SymbolID> {
    // ruleからnullableであることが分かっている場合は追加する
    let mut nulls: IndexSet<SymbolID> = grammar
        .rules()
        .filter_map(|(_id, rule)| rule.right().is_empty().then_some(rule.left()))
        .collect();

    // 値が更新されなくなるまで繰り返す
    let mut changed = true;
    while changed {
        changed = false;
        for (_, rule) in grammar.rules() {
            if nulls.contains(&rule.left()) {
                continue;
            }
            // 右辺のsymbolsがすべてnullableかどうか
            let is_rhs_nullable = rule.right().iter().all(|t| nulls.contains(t));
            if is_rhs_nullable {
                changed = true;
                nulls.insert(rule.left());
                continue;
            }
        }
    }

    nulls
}

/// Constructs the instance for calculating first sets in this grammar.
fn first_set(
    grammar: &Grammar,
    nulls: &IndexSet<SymbolID>,
) -> IndexMap<SymbolID, IndexSet<SymbolID>> {
    let mut map: IndexMap<SymbolID, IndexSet<SymbolID>> = IndexMap::default();

    // terminal symbols については First(T) = {T} になる
    for (id, _) in grammar.terminals() {
        map.insert(id, Some(id).into_iter().collect());
    }

    // nonterminal symbols は First(T) = {} と初期化する
    for (id, _) in grammar.nonterminals() {
        map.insert(id, IndexSet::default());
    }

    // 制約条件の抽出
    // X -> Y1 Y2 ... Yn という構文規則に対し、
    //  1. Y1,Y2,...と検索していき、最初に来る非nullableな記号を Yk とする
    //    - Y1 Y2 ... Y(k-1) が nullable で Yk が non-nullable となる
    //  2. Yi (i=1,2,..,k) それぞれに対し First(X) \supseteq First(Yi) という制約を追加する
    #[derive(Debug)]
    struct Constraint {
        sup: SymbolID,
        sub: SymbolID,
    }
    let mut constraints = vec![];
    for rule in grammar
        .rules()
        .flat_map(|(id, rule)| (id != RuleID::ACCEPT).then_some(rule))
    {
        for symbol in rule.right() {
            if rule.left() != *symbol {
                constraints.push(Constraint {
                    sup: rule.left(),
                    sub: *symbol,
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
            let mut superset = map.remove(sup).unwrap();
            let subset = map.get(sub).unwrap();

            for tok in subset {
                if !superset.contains(tok) {
                    superset.insert(*tok);
                    changed = true;
                }
            }

            map.insert(*sup, superset);
        }
    }

    map
}
