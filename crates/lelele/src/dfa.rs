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
    nodes: IndexMap<NodeID, DFANodeInner>,
    start_node: NodeID,
}

impl<'g> DFA<'g> {
    pub fn generate(grammar: &'g Grammar<'g>) -> Self {
        let mut gen = DFAGenerator {
            grammar,
            first_sets: FirstSets::new(grammar),
            nodes: IndexMap::new(),
            next_node_id: 0,
            pending_nodes: VecDeque::new(),
        };

        // 初期ノードの構築
        // [S' -> @ S]
        let start_node = {
            let mut item_set = IndexSet::new();
            item_set.insert(PendingLRItem {
                rule_id: RuleID::ACCEPT,
                marker: 0,
                lookahead: SymbolID::EOI,
            });
            gen.expand_closures(&mut item_set);
            gen.enqueue_node(item_set)
        };

        // 変化がなくなるまでクロージャ展開とノード追加を繰り返す
        gen.populate_nodes();

        // TODO: LALR(1) への変換

        // 同じcoreを持つLR(1)アイテムを集計する
        let nodes = gen
            .nodes
            .into_iter()
            .map(|(id, (item_set, edges))| {
                let mut lr_items: IndexMap<LRCoreItem, IndexSet<SymbolID>> = IndexMap::new();
                for item in item_set {
                    lr_items
                        .entry(LRCoreItem {
                            rule_id: item.rule_id,
                            marker: item.marker,
                        })
                        .or_default()
                        .insert(item.lookahead);
                }
                (id, DFANodeInner { lr_items, edges })
            })
            .collect();

        Self {
            grammar,
            nodes,
            start_node,
        }
    }

    pub fn nodes(&self) -> impl Iterator<Item = (NodeID, DFANode<'_>)> + '_ {
        self.nodes.iter().map(|(id, node)| {
            (
                *id,
                DFANode {
                    inner: node,
                    grammar: self.grammar,
                },
            )
        })
    }

    pub fn node(&self, id: NodeID) -> DFANode<'_> {
        DFANode {
            inner: &self.nodes[&id],
            grammar: self.grammar,
        }
    }

    pub fn start_node(&self) -> (NodeID, DFANode<'_>) {
        let (_, id, node) = self.nodes.get_full(&self.start_node).unwrap();
        (
            *id,
            DFANode {
                inner: node,
                grammar: self.grammar,
            },
        )
    }
}

impl fmt::Display for DFA<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (id, node) in &self.nodes {
            writeln!(f, "- id: {:02}", id)?;
            writeln!(f, "  item_sets:")?;
            for (core_item, lookaheads) in &node.lr_items {
                let LRCoreItem { rule_id, marker } = core_item;
                let rule = self.grammar.rule(*rule_id);
                let start = self.grammar.symbol(rule.start());
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
                write!(f, " [")?;
                for (i, lookahead) in lookaheads.iter().enumerate() {
                    let lookahead = self.grammar.symbol(*lookahead);
                    if i > 0 {
                        f.write_str("/")?;
                    }
                    f.write_str(lookahead.name())?;
                }
                writeln!(f, "]")?;
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
pub struct DFANode<'g> {
    grammar: &'g Grammar<'g>,
    inner: &'g DFANodeInner,
}

#[derive(Debug)]
struct DFANodeInner {
    // 各DFA nodeに所属するLR(1) itemの集合
    //  - key: core item
    //  - value: 紐付けられた先読み記号 (Eq,Hashを実装できないので別に持つ)
    lr_items: IndexMap<LRCoreItem, IndexSet<SymbolID>>,
    // 各DFAノード起点のedge
    edges: IndexMap<SymbolID, NodeID>,
}

// LR(1) item
// X: Y1 Y2 ... Yn という構文規則があったとき、それにマーカ位置を付与したもの
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct LRCoreItem {
    // grammer内におけるruleの識別子
    rule_id: RuleID,
    // marker位置
    marker: usize,
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum Action {
    Shift(NodeID),
    Reduce(RuleID),
    Accept,
}

impl DFANode<'_> {
    pub(crate) fn parse_actions(&self) -> IndexMap<SymbolID, Action> {
        let mut actions = IndexMap::new();

        // shift, goto
        for (label, target) in &self.inner.edges {
            match actions.entry(*label) {
                Entry::Occupied(..) => panic!("conflict"),
                Entry::Vacant(entry) => {
                    entry.insert(Action::Shift(*target));
                }
            }
        }

        // reduce, accept
        for (core_item, lookaheads) in &self.inner.lr_items {
            let rule = &self.grammar.rule(core_item.rule_id);
            if core_item.marker < rule.production().len() {
                continue;
            }

            if core_item.rule_id == RuleID::ACCEPT {
                match actions.entry(SymbolID::EOI) {
                    Entry::Occupied(..) => panic!("conflict"),
                    Entry::Vacant(e) => {
                        e.insert(Action::Accept);
                    }
                }
            } else {
                for lookahead in lookaheads {
                    match actions.entry(*lookahead) {
                        Entry::Occupied(..) => panic!("conflict"),
                        Entry::Vacant(e) => {
                            e.insert(Action::Reduce(core_item.rule_id));
                        }
                    }
                }
            }
        }

        actions
    }
}

// === DFAGenerator ===

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct PendingLRItem {
    // grammer内におけるruleの識別子
    rule_id: RuleID,
    // marker位置
    marker: usize,
    // 先読み記号
    lookahead: SymbolID,
}

#[derive(Debug)]
struct DFAGenerator<'g> {
    grammar: &'g Grammar<'g>,
    first_sets: FirstSets,
    nodes: IndexMap<NodeID, (IndexSet<PendingLRItem>, IndexMap<SymbolID, NodeID>)>,
    next_node_id: u64,
    pending_nodes: VecDeque<(NodeID, IndexSet<PendingLRItem>)>,
}

impl<'g> DFAGenerator<'g> {
    fn enqueue_node(&mut self, item_set: IndexSet<PendingLRItem>) -> NodeID {
        let id = NodeID::new(self.next_node_id);
        self.next_node_id += 1;
        self.pending_nodes.push_back((id, item_set));
        id
    }

    fn fixed_nodes(&self) -> impl Iterator<Item = (NodeID, &IndexSet<PendingLRItem>)> {
        self.nodes.iter().map(|(id, node)| (*id, &node.0))
    }

    fn populate_nodes(&mut self) {
        // 新規にノードが生成されなくなるまで繰り返す
        while let Some((id, item_set)) = self.pending_nodes.pop_front() {
            let mut edges = IndexMap::new();

            // 遷移先のitems setを生成する
            for (symbol, mut new_item_set) in self.extract_transitions(&item_set) {
                self.expand_closures(&mut new_item_set);

                // クロージャ展開後のitem setが同じなら同一のノードとみなし、新規にノードを生成しない
                let found = self
                    .fixed_nodes()
                    .chain(Some((id, &item_set))) // DFANodeが生成されていないが候補には入れる
                    .find(|(_, item_set)| **item_set == new_item_set);
                if let Some((id, _)) = found {
                    edges.insert(symbol, id);
                    continue;
                }

                // ノードを新規に作る
                let id = self.enqueue_node(new_item_set);
                edges.insert(symbol, id);
            }

            self.nodes.insert(id, (item_set, edges));
        }
    }

    /// クロージャ展開
    fn expand_closures(&self, items: &mut IndexSet<PendingLRItem>) {
        let mut changed = true;
        while changed {
            changed = false;

            let mut added = IndexSet::new();
            for PendingLRItem {
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
                            added.insert(PendingLRItem {
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
        items: &IndexSet<PendingLRItem>,
    ) -> IndexMap<SymbolID, IndexSet<PendingLRItem>> {
        let mut item_sets: IndexMap<SymbolID, IndexSet<PendingLRItem>> = IndexMap::new();
        for item in items {
            let rule = self.grammar.rule(item.rule_id);

            // markerが終わりまで到達していれば無視する
            if item.marker >= rule.production().len() {
                continue;
            }

            // edgeのラベルとなるsymbol
            let label = rule.production()[item.marker];

            //
            item_sets.entry(label).or_default().insert(PendingLRItem {
                marker: item.marker + 1,
                ..item.clone()
            });
        }
        item_sets
    }
}
