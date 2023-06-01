//! LR(1) DFA generation.

use crate::{
    first_sets::FirstSets,
    grammar::{Grammar, RuleID, SymbolID},
};
use indexmap::{indexmap, indexset, map::Entry, IndexMap, IndexSet};
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
        // [S' -> @ S] {$eoi}
        let mut item_set = indexmap! {
            LRCoreItem {
                rule_id: RuleID::ACCEPT,
                marker: 0,
            } => indexset! { SymbolID::EOI },
        };
        gen.expand_closures(&mut item_set);
        let start_node = gen.enqueue_node(item_set);

        // 変化がなくなるまでクロージャ展開とノード追加を繰り返す
        gen.populate_nodes();

        // TODO: LALR(1) への変換

        Self {
            grammar,
            nodes: gen.nodes,
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
            for (core_item, lookaheads) in &node.item_set {
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
    item_set: LRItemSet,
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

//  - key: core item
//  - value: 紐付けられた先読み記号 (Eq,Hashを実装できないので別に持つ)
type LRItemSet = IndexMap<LRCoreItem, IndexSet<SymbolID>>;

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
        for (core_item, lookaheads) in &self.inner.item_set {
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

#[derive(Debug)]
struct DFAGenerator<'g> {
    grammar: &'g Grammar<'g>,
    first_sets: FirstSets,
    nodes: IndexMap<NodeID, DFANodeInner>,
    next_node_id: u64,
    pending_nodes: VecDeque<(NodeID, LRItemSet)>,
}

impl<'g> DFAGenerator<'g> {
    fn enqueue_node(&mut self, item_set: LRItemSet) -> NodeID {
        let id = NodeID::new(self.next_node_id);
        self.next_node_id += 1;
        self.pending_nodes.push_back((id, item_set));
        id
    }

    fn populate_nodes(&mut self) {
        // 新規にノードが生成されなくなるまで繰り返す
        while let Some((id, mut item_set)) = self.pending_nodes.pop_front() {
            let mut edges = IndexMap::new();

            // 遷移先のitems setを生成する
            'outer: for (symbol, mut new_item_set) in self.extract_transitions(&item_set) {
                self.expand_closures(&mut new_item_set);

                let same_nodes = self
                    .nodes
                    .iter_mut()
                    .map(|(id, node)| (*id, &mut node.item_set))
                    .chain(Some((id, &mut item_set))) // DFANodeが生成されていないが候補には入れる
                    .filter_map(|(id, item_set)| {
                        compare_item_sets(item_set, &new_item_set).map(|diff| (id, item_set, diff))
                    });

                for (id, item_set, diff) in same_nodes {
                    match diff {
                        ItemSetDiff::Canonical => {
                            // クロージャ展開後のitem setが同じなら同一のノードとみなし、新規にノードを生成しない
                            edges.insert(symbol, id);
                            continue 'outer;
                        }

                        ItemSetDiff::LALR => {
                            // マージ後のノードをクロージャ展開しても変化がないと仮定
                            // 新規にノードを生成せず、lookaheadsのマージのみを行う
                            for (new_core, new_lookaheads) in new_item_set {
                                let lookaheads =
                                    item_set.entry(new_core).or_insert_with(|| unreachable!());
                                lookaheads.extend(new_lookaheads);
                            }
                            edges.insert(symbol, id); // NodeIDは
                            continue 'outer;
                        }
                    }
                }

                // マージ候補がなければノードを新規に作る
                let id = self.enqueue_node(new_item_set);
                edges.insert(symbol, id);
            }

            self.nodes.insert(id, DFANodeInner { item_set, edges });
        }
    }

    /// クロージャ展開
    fn expand_closures(&self, items: &mut LRItemSet) {
        let mut changed = true;
        while changed {
            changed = false;

            // 候補の抽出
            let mut added: IndexMap<LRCoreItem, IndexSet<SymbolID>> = IndexMap::new();
            for (core, lookaheads) in &mut *items {
                let rule = self.grammar.rule(core.rule_id);

                // [X -> ... @ Y beta]
                //  Y: one nonterminal symbol
                let (y_symbol, beta) = match &rule.production()[core.marker..] {
                    [y_symbol, beta @ ..] if !self.grammar.symbol(*y_symbol).is_terminal() => {
                        (*y_symbol, beta)
                    }
                    _ => continue,
                };

                // lookaheads = {x1,x2,...,xk} としたとき、
                //   x \in First(beta x1) \cup ... \cup First(beta xk)
                // を満たすすべての終端記号を考える
                for x in lookaheads
                    .iter()
                    .flat_map(|l| self.first_sets.get(beta, *l))
                {
                    for (rule_id, rule) in self.grammar.rules() {
                        // Y: ... という形式の構文規則のみを対象にする
                        if rule.start() != y_symbol {
                            continue;
                        }

                        added
                            .entry(LRCoreItem { rule_id, marker: 0 })
                            .or_default()
                            .insert(x);
                    }
                }
            }

            for (core, lookaheads) in added {
                let slot = items.entry(core).or_insert_with(|| {
                    changed = true;
                    IndexSet::new()
                });
                for l in lookaheads {
                    changed |= slot.insert(l);
                }
            }
        }
    }

    /// 指定したLRアイテム集合から遷移先のLRアイテム集合（未展開）とラベルを抽出する
    fn extract_transitions(&self, items: &LRItemSet) -> IndexMap<SymbolID, LRItemSet> {
        let mut item_sets: IndexMap<SymbolID, LRItemSet> = IndexMap::new();
        for (core, lookaheads) in items {
            let rule = self.grammar.rule(core.rule_id);

            // markerが終わりまで到達していれば無視する
            if core.marker >= rule.production().len() {
                continue;
            }

            let label = rule.production()[core.marker];
            let new_item_set = item_sets.entry(label).or_default();
            new_item_set
                .entry(LRCoreItem {
                    marker: core.marker + 1,
                    ..core.clone()
                })
                .or_default()
                .extend(lookaheads);
        }
        item_sets
    }
}

enum ItemSetDiff {
    /// Items are equivalent in the sense of of Knuth's canonical LR(1) method,
    /// that is, each item sets have the same LR(0) cores and their lookahead symbols
    /// are also equal.
    Canonical,

    /// Items are compatible in the sense of DeRemer's LALR(1) method, that is,
    /// each item sets have the same LR(0) cores but different lookahead symbols.
    LALR,
}

fn compare_item_sets(left: &LRItemSet, right: &LRItemSet) -> Option<ItemSetDiff> {
    if left.len() != right.len() {
        return None;
    }

    let mut diff = ItemSetDiff::Canonical;

    for (key, value) in left {
        let v = right.get(key)?;
        if value != v {
            diff = ItemSetDiff::LALR;
        }
    }

    Some(diff)
}
