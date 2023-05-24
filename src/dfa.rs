//! LR(1) DFA generation.

use crate::grammar::{Grammar, RuleID, SymbolID};
use indexmap::{IndexMap, IndexSet};
use std::{fmt, mem};

#[derive(Debug)]
pub struct DFA {
    nodes: IndexMap<NodeID, DFANode>,
}

impl DFA {
    pub fn display<'g>(&'g self, grammar: &'g Grammar) -> impl fmt::Display + 'g {
        struct DFADisplay<'g> {
            grammar: &'g Grammar,
            dfa: &'g DFA,
        }
        impl fmt::Display for DFADisplay<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                for (id, node) in &self.dfa.nodes {
                    writeln!(f, "- {:02}:", id)?;
                    writeln!(f, "  item_set:")?;
                    for item in &node.item_set {
                        let rule = &self.grammar.rules.get(&item.rule_id).unwrap();
                        write!(f, "  - [{} -> ", self.grammar.symbol_name(rule.lhs))?;
                        for (i, s) in rule.rhs.iter().enumerate() {
                            if i > 0 {
                                write!(f, " ")?;
                            }
                            if i == item.marker {
                                write!(f, "@ ")?;
                            }
                            write!(f, "{}", self.grammar.symbol_name(*s))?;
                        }
                        if item.marker == rule.rhs.len() {
                            write!(f, " @")?;
                        }
                        writeln!(f, "] {{ {} }}", self.grammar.symbol_name(item.lookahead))?;
                    }
                    if !node.edges.is_empty() {
                        writeln!(f, "  edges:")?;
                        for (symbol, id) in &node.edges {
                            writeln!(f, "  - {} -> {:02}", self.grammar.symbol_name(*symbol), id)?;
                        }
                    }
                }
                Ok(())
            }
        }
        DFADisplay { dfa: self, grammar }
    }

    pub fn transition_table(
        &self,
        grammar: &Grammar,
    ) -> IndexMap<NodeID, IndexMap<SymbolID, Action>> {
        let mut transition_table: IndexMap<NodeID, IndexMap<SymbolID, Action>> = IndexMap::new();
        for (id, node) in &self.nodes {
            let mut actions = IndexMap::new();
            // shift, goto
            for (label, target) in &node.edges {
                actions.insert(
                    *label,
                    if grammar.nonterminals.contains(label) {
                        Action::Goto(*target)
                    } else {
                        Action::Shift(*target)
                    },
                );
            }

            // reduce, accept
            for item in &node.item_set {
                let rule = &grammar.rules[&item.rule_id];
                if item.marker < rule.rhs.len() {
                    continue;
                }

                if item.rule_id == RuleID::START {
                    actions.insert(SymbolID::EOI, Action::Accept);
                } else {
                    actions.insert(item.lookahead, Action::Reduce(item.rule_id));
                }
            }

            transition_table.insert(*id, actions);
        }
        transition_table
    }
}

#[derive(Debug)]
pub enum Action {
    Shift(NodeID),
    Goto(NodeID),
    Reduce(RuleID),
    Accept,
}

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Shift(id) => write!(f, "shift({:02})", id),
            Self::Goto(id) => write!(f, "goto({:02})", id),
            Self::Reduce(id) => write!(f, "reduce({})", id),
            Self::Accept => write!(f, "accept"),
        }
    }
}

#[derive(Debug)]
pub struct DFANode {
    // 各DFA nodeに所属するLR item set
    pub item_set: LRItemSet,
    // 各DFAノード起点のedge
    pub edges: IndexMap<SymbolID, NodeID>,
}

// LR(1) item
// X: Y1 Y2 ... Yn という構文規則があったとき、それに
//  * マーカ位置
//  * 先読み記号
// を付与したもの
// example:
//   [ X -> @ Y1   Y2 ... Yn ]
//   [ X ->   Y1 @ Y2 ... Yn ]
//   [ X ->   Y1   Y2 ... Yn @ ]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LRItem {
    // grammer内におけるruleの識別子
    pub rule_id: RuleID,
    // marker位置
    pub marker: usize,
    // 先読み記号
    pub lookahead: SymbolID,
}

pub type LRItemSet = IndexSet<LRItem>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NodeID {
    inner: usize,
}
impl NodeID {
    const fn new(i: usize) -> Self {
        Self { inner: i }
    }
}
impl fmt::Display for NodeID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

#[derive(Debug)]
pub struct DFAGenerator<'g> {
    grammar: &'g Grammar,
    first_set: IndexMap<SymbolID, IndexSet<SymbolID>>,
    nulls: IndexSet<SymbolID>,
}

impl<'g> DFAGenerator<'g> {
    pub fn new(grammar: &'g Grammar) -> Self {
        let nulls = nulls_set(grammar);
        let first_set = first_set(grammar, &nulls);
        Self {
            grammar,
            first_set,
            nulls,
        }
    }

    pub fn process(&mut self) -> DFA {
        let mut nodes: IndexMap<NodeID, DFANode> = IndexMap::new();
        let mut next_node_id = 0;
        let mut node_id = || {
            let id = next_node_id;
            next_node_id += 1;
            NodeID::new(id)
        };

        // 遷移先の抽出が未完了なノード
        struct PendingItem {
            id: NodeID,
            item_set: LRItemSet,
        }
        let mut pending_items: Vec<PendingItem> = vec![];
        pending_items.push({
            // 初期ノードの構築
            let mut item_set = IndexSet::new();
            // [S' -> @ S]
            item_set.insert(LRItem {
                rule_id: RuleID::START,
                marker: 0,
                lookahead: SymbolID::EOI,
            });
            self.expand_closures(&mut item_set);
            PendingItem {
                id: node_id(),
                item_set,
            }
        });

        // 新規にノードが生成されなくなるまで繰り返す
        while !pending_items.is_empty() {
            // ループ内でpending_itemsに要素を追加するのでdrainは使用しない
            for PendingItem { id, item_set } in mem::take(&mut pending_items) {
                let mut edges = IndexMap::new();

                // 遷移先のitems setを生成する
                for (symbol, mut new_item_set) in self.extract_transitions(&item_set) {
                    self.expand_closures(&mut new_item_set);

                    // クロージャ展開後のitem setが同じなら同一のノードとみなし、新規にノードを生成しない
                    let found = nodes.iter().find(|(_, node)| node.item_set == new_item_set);
                    if let Some((id, _node)) = found {
                        edges.insert(symbol, *id);
                        continue;
                    }

                    // MEMO:
                    // 遷移先のノードに含まれるLR itemは元々のitemのmarkerをひとつずらしたものなので、遷移元のitem setと一致することはない（はず）
                    debug_assert_ne!(item_set, new_item_set);

                    // ノードを新規に作る
                    let id = node_id();
                    pending_items.push(PendingItem {
                        id,
                        item_set: new_item_set,
                    });
                    edges.insert(symbol, id);
                }

                nodes.insert(id, DFANode { item_set, edges });
            }
        }

        DFA { nodes }
    }

    /// クロージャ展開
    fn expand_closures(&self, items: &mut LRItemSet) {
        let mut changed = true;
        while changed {
            changed = false;

            let mut added = IndexSet::new();
            for LRItem {
                rule_id,
                marker,
                lookahead,
            } in &*items
            {
                let rule = self.grammar.rules.get(rule_id).unwrap();

                // [X -> ... @ Y beta]
                //  Y: one nonterminal symbol
                let (y_symbol, beta) = match &rule.rhs[*marker..] {
                    [y_symbol, beta @ ..] if self.grammar.nonterminals.contains(y_symbol) => {
                        (*y_symbol, beta)
                    }
                    _ => continue,
                };

                // First(beta x)
                let new_lookahead: Vec<SymbolID> = self.calc_first_set(beta, *lookahead);
                for x in new_lookahead {
                    // Y: ... という形式の構文規則から LR(0) item を生成し追加する
                    for (id, rule) in &self.grammar.rules {
                        if rule.lhs == y_symbol {
                            added.insert(LRItem {
                                rule_id: *id,
                                marker: 0,
                                lookahead: x,
                            });
                        }
                    }
                }
            }

            for item in added {
                changed |= items.insert(item);
            }
        }
    }

    /// 指定したLRアイテム集合から遷移先のLRアイテム集合（未展開）とラベルを抽出する
    fn extract_transitions(&self, items: &LRItemSet) -> IndexMap<SymbolID, LRItemSet> {
        let mut item_sets: IndexMap<SymbolID, LRItemSet> = IndexMap::new();
        for item in items {
            let rule = self.grammar.rules.get(&item.rule_id).unwrap();

            // markerが終わりまで到達していれば無視する
            if item.marker >= rule.rhs.len() {
                continue;
            }

            // edgeのラベルとなるsymbol
            let label = rule.rhs[item.marker];

            //
            item_sets.entry(label).or_default().insert(LRItem {
                marker: item.marker + 1,
                ..item.clone()
            });
        }
        item_sets
    }

    /// `First(prefix x)`
    fn calc_first_set(&self, prefix: &[SymbolID], x: SymbolID) -> Vec<SymbolID> {
        let mut res = IndexSet::new();
        for token in prefix.iter().chain(Some(&x)) {
            let added = self.first_set.get(token).expect("unexpected token");
            res.extend(added.iter().copied());
            if !self.nulls.contains(token) {
                break;
            }
        }
        res.into_iter().collect()
    }
}

/// Calculate the set of nullable symbols in this grammar.
fn nulls_set(grammar: &Grammar) -> IndexSet<SymbolID> {
    // ruleからnullableであることが分かっている場合は追加する
    let mut nulls: IndexSet<SymbolID> = grammar
        .rules
        .values()
        .filter_map(|rule| rule.rhs.is_empty().then(|| rule.lhs))
        .collect();

    // 値が更新されなくなるまで繰り返す
    let mut changed = true;
    while changed {
        changed = false;
        for rule in grammar.rules.values() {
            if nulls.contains(&rule.lhs) {
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
fn first_set(
    grammar: &Grammar,
    nulls: &IndexSet<SymbolID>,
) -> IndexMap<SymbolID, IndexSet<SymbolID>> {
    let mut map: IndexMap<SymbolID, IndexSet<SymbolID>> = IndexMap::new();

    // terminal symbols については First(T) = {T} になる
    for symbol in &grammar.terminals {
        map.insert(*symbol, Some(*symbol).into_iter().collect());
    }

    // nonterminal symbols に関する初期値
    for symbol in &grammar.nonterminals {
        map.insert(*symbol, IndexSet::new());
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
        .rules
        .iter()
        .flat_map(|(id, rule)| (*id != RuleID::START).then(|| rule))
    {
        for symbol in &rule.rhs {
            if rule.lhs != *symbol {
                constraints.push(Constraint {
                    sup: rule.lhs,
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
