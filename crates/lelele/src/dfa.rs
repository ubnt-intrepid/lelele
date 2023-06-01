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
            extractor: NodeExtractor {
                grammar,
                first_sets: FirstSets::new(grammar),
            },
            pending_nodes: PendingNodes::new(),
            nodes: IndexMap::new(),
            remapped_nodes: IndexMap::new(),
        };

        // 初期ノード: [S' -> @ S] {$eoi}
        let start_node = gen.pending_nodes.enqueue(indexmap! {
            LRCoreItem {
                rule_id: RuleID::ACCEPT,
                marker: 0,
            } => indexset! { SymbolID::EOI },
        });

        // 変化がなくなるまでクロージャ展開とノード追加を繰り返す
        gen.populate_nodes();

        // a
        for node in gen.nodes.values_mut() {
            for target in node.edges.values_mut() {
                if let Some(remapped) = gen.remapped_nodes.get(target) {
                    *target = *remapped;
                }
            }
        }

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
                    grammar: self.grammar,
                    inner: node,
                },
            )
        })
    }

    pub fn node(&self, id: NodeID) -> DFANode<'_> {
        DFANode {
            grammar: self.grammar,
            inner: &self.nodes[&id],
        }
    }

    pub fn start_node(&self) -> (NodeID, DFANode<'_>) {
        (self.start_node, self.node(self.start_node))
    }
}

impl fmt::Display for DFA<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (id, node) in self.nodes() {
            writeln!(f, "- id: {:02}", id)?;
            writeln!(f, "  item_sets:")?;
            for (core_item, lookaheads) in &node.inner.item_set {
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
            for (label, target) in &node.inner.edges {
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
    item_set: LRItemSet,
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
struct PendingNodes {
    next_node_id: u64,
    pending_nodes: VecDeque<(NodeID, LRItemSet)>,
}
impl PendingNodes {
    fn new() -> Self {
        Self {
            next_node_id: 0,
            pending_nodes: VecDeque::new(),
        }
    }

    /// Push a LR(1) item set into the queue, and obtain registered NodeID.
    fn enqueue(&mut self, item_set: LRItemSet) -> NodeID {
        let id = NodeID::new(self.next_node_id);
        self.next_node_id += 1;
        self.pending_nodes.push_back((id, item_set));
        id
    }

    /// Pop a LR(1) item set from the queue.
    fn dequeue(&mut self) -> Option<(NodeID, LRItemSet)> {
        self.pending_nodes.pop_front()
    }
}

#[derive(Debug)]
struct NodeExtractor<'g> {
    grammar: &'g Grammar<'g>,
    first_sets: FirstSets,
}

impl NodeExtractor<'_> {
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

#[derive(Debug)]
struct DFAGenerator<'g> {
    extractor: NodeExtractor<'g>,
    pending_nodes: PendingNodes,
    nodes: IndexMap<NodeID, DFANodeInner>,
    remapped_nodes: IndexMap<NodeID, NodeID>,
}

impl<'g> DFAGenerator<'g> {
    fn populate_nodes(&mut self) {
        // 新規にノードが生成されなくなるまで繰り返す
        'dequeue: while let Some((new_id, mut new_item_set)) = self.pending_nodes.dequeue() {
            // クロージャ展開
            self.extractor.expand_closures(&mut new_item_set);

            // 互換性のあるノードを検索する
            // FIXME: hashを使って O(1) にする
            for (orig_id, orig_node) in &mut self.nodes {
                // 互換性のあるノードが既にある場合、そのノードを修正し新規にノードは生成しない
                match compare_item_sets(&orig_node.item_set, &new_item_set) {
                    Some(ItemSetDiff::Canonical) => {
                        // 完全に一致しているので修正すら不要
                    }
                    Some(ItemSetDiff::LALR) => {
                        // lookaheadsをマージする
                        let mut modified = false;
                        for (new_core, new_lookaheads) in &new_item_set {
                            let lookaheads = orig_node.item_set.get_mut(new_core).unwrap();
                            for l in new_lookaheads {
                                modified |= lookaheads.insert(*l);
                            }
                        }

                        // a
                        if modified {
                            for (symbol, new_item_set) in
                                self.extractor.extract_transitions(&new_item_set)
                            {
                                let id = self.pending_nodes.enqueue(new_item_set);
                                orig_node.edges.insert(symbol, id);
                            }
                        }
                    }

                    // LALR が無効化されている場合など
                    _ => continue,
                }

                // 使用する予定だったNodeIDはenqueueされた時点で遷移前のnodeに登録されているので、
                // 後で書き換えるためにメモしておく
                self.remapped_nodes.insert(new_id, *orig_id);
                continue 'dequeue;
            }

            // 遷移先のitems setを生成し、ノード生成のキューに登録する
            let mut edges = IndexMap::new();
            for (symbol, new_item_set) in self.extractor.extract_transitions(&new_item_set) {
                let id = self.pending_nodes.enqueue(new_item_set);
                edges.insert(symbol, id);
            }

            self.nodes.insert(
                new_id,
                DFANodeInner {
                    item_set: new_item_set,
                    edges,
                },
            );
        }
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

    for (key_l, value_l) in left {
        let value_r = right.get(key_l)?;
        if value_l != value_r {
            diff = ItemSetDiff::LALR;
        }
    }

    Some(diff)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::Choice;

    #[test]
    fn smoketest1() {
        let grammar = Grammar::define(|def| {
            let equal = def.token("EQUAL");
            let plus = def.token("PLUS");
            let ident = def.token("ID");
            let num = def.token("NUM");

            let a = def.symbol("A");
            let e = def.symbol("E");
            let t = def.symbol("T");

            def.start_symbol(a);

            def.rule(a, Choice(((e, equal, e), ident)));
            def.rule(e, Choice(((e, plus, t), t)));
            def.rule(t, Choice((num, ident)));
        });
        eprintln!("{}", grammar);

        let dfa = DFA::generate(&grammar);
        eprintln!("DFA Nodes:\n---\n{}", dfa);
    }

    #[test]
    fn smoketest2() {
        let grammar = Grammar::define(|def| {
            // declare terminal symbols.
            let lparen = def.token("LPAREN");
            let rparen = def.token("RPAREN");
            let plus = def.token("PLUS");
            let minus = def.token("MINUS");
            let star = def.token("STAR");
            let slash = def.token("SLASH");
            let num = def.token("NUM");
            let _ = def.token("UNUSED_0");

            // declare nonterminal symbols.
            let expr = def.symbol("EXPR");
            let factor = def.symbol("FACTOR");
            let term = def.symbol("TERM");
            let _ = def.symbol("UNUSED_1");

            def.start_symbol(expr);

            // declare syntax rules.
            def.rule(
                expr,
                Choice((
                    (expr, plus, factor),  // expr '+' factor
                    (expr, minus, factor), // expr '-' factor
                    factor,                // factor
                )),
            );
            def.rule(
                factor,
                Choice((
                    (factor, star, term),  // factor '*' term
                    (factor, slash, term), // factor '/' term
                    term,                  // term
                )),
            );
            def.rule(
                term,
                Choice((
                    num,                    // num
                    (lparen, expr, rparen), // '(' expr ')'
                )),
            );
        });
        eprintln!("{}", grammar);

        let dfa = DFA::generate(&grammar);
        eprintln!("DFA Nodes:\n---\n{}", dfa);
    }
}
