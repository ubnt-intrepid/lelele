//! LR(1) DFA generation.

use crate::{
    first_sets::FirstSets,
    grammar::{Grammar, RuleID, SymbolID},
};
use indexmap::{map::Entry, IndexMap, IndexSet};
use std::{collections::VecDeque, fmt};

#[derive(Debug)]
pub struct DFA<'g> {
    grammar: &'g Grammar<'g>,
    nodes: IndexMap<NodeID, DFANode>,
    start_node: NodeID,
}

impl<'g> DFA<'g> {
    pub fn generate(grammar: &'g Grammar<'g>) -> Self {
        let first_sets = FirstSets::new(grammar);
        DFAGenerator {
            grammar,
            first_sets,
        }
        .generate()
    }

    pub fn nodes(&self) -> impl Iterator<Item = (NodeID, &DFANode)> + '_ {
        self.nodes.iter().map(|(id, node)| (*id, node))
    }

    pub fn node(&self, id: NodeID) -> &DFANode {
        &self.nodes[&id]
    }

    pub fn start_node(&self) -> (NodeID, &DFANode) {
        let (_, id, node) = self.nodes.get_full(&self.start_node).unwrap();
        (*id, node)
    }

    pub(crate) fn parse_table(&self) -> IndexMap<NodeID, IndexMap<SymbolID, Action>> {
        let mut transition_table: IndexMap<NodeID, IndexMap<SymbolID, Action>> = IndexMap::new();
        for (id, node) in self.nodes() {
            let mut actions = IndexMap::new();
            // shift, goto
            for (label, target) in &node.edges {
                match actions.entry(*label) {
                    Entry::Occupied(..) => panic!("conflict"),
                    Entry::Vacant(entry) => {
                        if self.grammar.symbol(*label).is_terminal() {
                            entry.insert(Action::Shift(*target));
                        } else {
                            entry.insert(Action::Goto(*target));
                        }
                    }
                }
            }

            // reduce, accept
            for item in &node.item_set {
                let rule = &self.grammar.rule(item.rule_id);
                if item.marker < rule.production().len() {
                    continue;
                }

                if item.rule_id == RuleID::ACCEPT {
                    match actions.entry(SymbolID::EOI) {
                        Entry::Occupied(..) => panic!("conflict"),
                        Entry::Vacant(e) => {
                            e.insert(Action::Accept);
                        }
                    }
                } else {
                    match actions.entry(item.lookahead) {
                        Entry::Occupied(..) => panic!("conflict"),
                        Entry::Vacant(e) => {
                            e.insert(Action::Reduce(item.rule_id));
                        }
                    }
                }
            }

            transition_table.insert(id, actions);
        }
        transition_table
    }
}

impl fmt::Display for DFA<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (id, node) in &self.nodes {
            writeln!(f, "- id: {:02}", id)?;
            writeln!(f, "  item_sets:")?;
            for item in &node.item_set {
                let LRItem {
                    rule_id,
                    marker,
                    lookahead,
                } = item;
                let rule = self.grammar.rule(*rule_id);
                let start = self.grammar.symbol(rule.start());
                let lookahead = self.grammar.symbol(*lookahead);
                write!(f, "  - {} :", start.name())?;
                for (i, prod) in rule.production().iter().enumerate() {
                    let prod = self.grammar.symbol(*prod);
                    if i == *marker {
                        f.write_str(" @")?;
                    }
                    write!(f, " {}", prod.name())?;
                }
                if *marker == rule.production().len() {
                    f.write_str(" @")?;
                }
                writeln!(f, " [{}]", lookahead.name())?;
            }
            writeln!(f, "  edges:")?;
            for (label, target) in &node.edges {
                let label = self.grammar.symbol(*label);
                writeln!(f, "  - {} -> {:02}", label.name(), target)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum Action {
    Shift(NodeID),
    Goto(NodeID),
    Reduce(RuleID),
    Accept,
}

#[derive(Debug)]
pub struct DFANode {
    // 各DFA nodeに所属するLR item set
    pub item_set: IndexSet<LRItem>,
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NodeID {
    raw: u64,
}
impl NodeID {
    const fn new(raw: u64) -> Self {
        Self { raw }
    }
}
impl fmt::Display for NodeID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.raw, f)
    }
}

#[derive(Debug)]
struct DFAGenerator<'g> {
    grammar: &'g Grammar<'g>,
    first_sets: FirstSets,
}

impl<'g> DFAGenerator<'g> {
    fn generate(&mut self) -> DFA<'g> {
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
            item_set: IndexSet<LRItem>,
        }
        let mut pending_items = VecDeque::new();

        // 初期ノードの構築
        let start_node_id = node_id();
        let mut item_set = IndexSet::new();
        // [S' -> @ S]
        item_set.insert(LRItem {
            rule_id: RuleID::ACCEPT,
            marker: 0,
            lookahead: SymbolID::EOI,
        });
        self.expand_closures(&mut item_set);
        pending_items.push_back(PendingItem {
            id: start_node_id,
            item_set,
        });

        // 新規にノードが生成されなくなるまで繰り返す
        while let Some(PendingItem { id, item_set }) = pending_items.pop_front() {
            let mut edges = IndexMap::new();

            // 遷移先のitems setを生成する
            for (symbol, mut new_item_set) in self.extract_transitions(&item_set) {
                self.expand_closures(&mut new_item_set);

                // クロージャ展開後のitem setが同じなら同一のノードとみなし、新規にノードを生成しない
                let found = nodes
                    .iter()
                    .map(|(id, node)| (*id, &node.item_set))
                    .chain(Some((id, &item_set))) // DFANodeが生成されていないが候補には入れる
                    .find_map(|(id, item_set)| (*item_set == new_item_set).then_some(id));
                if let Some(id) = found {
                    edges.insert(symbol, id);
                    continue;
                }

                // ノードを新規に作る
                let id = node_id();
                pending_items.push_back(PendingItem {
                    id,
                    item_set: new_item_set,
                });
                edges.insert(symbol, id);
            }

            nodes.insert(id, DFANode { item_set, edges });
        }

        DFA {
            grammar: self.grammar,
            nodes,
            start_node: start_node_id,
        }
    }

    /// クロージャ展開
    fn expand_closures(&self, items: &mut IndexSet<LRItem>) {
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
                let rule = self.grammar.rule(*rule_id);

                // [X -> ... @ Y beta]
                //  Y: one nonterminal symbol
                let (y_symbol, beta) = match &rule.production()[*marker..] {
                    [y_symbol, beta @ ..] if !self.grammar.symbol(*y_symbol).is_terminal() => {
                        (*y_symbol, beta)
                    }
                    _ => continue,
                };

                // x \in First(beta x)
                for x in self.first_sets.get(beta, *lookahead) {
                    // Y: ... という形式の構文規則から LR(0) item を生成し追加する
                    for (rule_id, rule) in self.grammar.rules() {
                        if rule.start() == y_symbol {
                            added.insert(LRItem {
                                rule_id,
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
    fn extract_transitions(
        &self,
        items: &IndexSet<LRItem>,
    ) -> IndexMap<SymbolID, IndexSet<LRItem>> {
        let mut item_sets: IndexMap<SymbolID, IndexSet<LRItem>> = IndexMap::new();
        for item in items {
            let rule = self.grammar.rule(item.rule_id);

            // markerが終わりまで到達していれば無視する
            if item.marker >= rule.production().len() {
                continue;
            }

            // edgeのラベルとなるsymbol
            let label = rule.production()[item.marker];

            //
            item_sets.entry(label).or_default().insert(LRItem {
                marker: item.marker + 1,
                ..item.clone()
            });
        }
        item_sets
    }
}
