//! Definition and generation of LR(1) automata.

use crate::{
    grammar::{Grammar, RuleID, SymbolID},
    IndexMap, IndexSet,
};
use indexmap::map::Entry;
use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    fmt,
};

#[derive(Debug)]
pub struct Config {
    merge_mode: MergeMode,
}

impl Config {
    pub const fn new() -> Self {
        Self {
            merge_mode: MergeMode::PGM,
        }
    }

    /// Set the merge strategy of DFA nodes to match Knuth's Canonical LR(1) method.
    pub fn use_canonical(&mut self) -> &mut Self {
        self.merge_mode = MergeMode::Canonical;
        self
    }

    /// Set the merge strategy of DFA nodes to match Pager's Practical General Method (PGM).
    ///
    /// By default, this strategy is selected due to the trade-off between
    /// reducing the number of DFA nodes and avoiding reduce/reduce conflicts.
    pub fn use_pgm(&mut self) -> &mut Self {
        self.merge_mode = MergeMode::PGM;
        self
    }

    /// Set the merge strategy of DFA nodes to match DeRemer's LALR(1) method.
    pub fn use_lalr(&mut self) -> &mut Self {
        self.merge_mode = MergeMode::LALR;
        self
    }

    pub fn generate(&self, grammar: &Grammar) -> DFA {
        let mut gen = DFAGenerator::new(grammar, self.merge_mode);
        gen.populate_nodes();
        gen.finalize()
    }
}

#[derive(Debug)]
pub struct DFA {
    nodes: IndexMap<NodeID, DFANode>,
}

impl DFA {
    pub fn generate(grammar: &Grammar) -> Self {
        Config::new().generate(grammar)
    }

    pub fn nodes(&self) -> impl Iterator<Item = (NodeID, &DFANode)> + '_ {
        self.nodes.iter().map(|(id, node)| (*id, node))
    }

    pub fn node(&self, id: NodeID) -> &DFANode {
        &self.nodes[&id]
    }

    pub fn display<'g>(&'g self, grammar: &'g Grammar) -> impl fmt::Display + 'g {
        DFADisplay { grammar, dfa: self }
    }
}

struct DFADisplay<'g> {
    grammar: &'g Grammar,
    dfa: &'g DFA,
}

impl fmt::Display for DFADisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (id, node) in self.dfa.nodes() {
            writeln!(f, "- id: {:02}", id)?;
            writeln!(f, "  item_sets:")?;
            for (core_item, lookaheads) in &node.item_set {
                let LRCoreItem { rule_id, marker } = core_item;
                let rule = self.grammar.rule(*rule_id);
                let start = self.grammar.symbol(rule.left());
                write!(f, "  - {} :", start.name())?;
                for (i, prod) in rule.right().iter().enumerate() {
                    let prod = self.grammar.symbol(*prod);
                    if i == *marker {
                        f.write_str(" @")?;
                    }
                    write!(f, " {}", prod.name())?;
                }
                if *marker == rule.right().len() {
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
    pub(crate) const START: Self = Self::new(0);

    const fn new(raw: u64) -> Self {
        assert!(raw <= u64::MAX / 2, "too big node id");
        Self { raw }
    }
}

impl fmt::Display for NodeID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.raw, f)
    }
}

#[derive(Debug)]
pub struct DFANode {
    item_set: LRItemSet,
    edges: IndexMap<SymbolID, NodeID>,
}

// LR(1) item
// X: Y1 Y2 ... Yn という構文規則があったとき、それにマーカ位置を付与したもの
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct LRCoreItem {
    // grammer内におけるruleの識別子
    rule_id: RuleID,
    // marker位置
    marker: usize,
}

//  - key: core item
//  - value: 紐付けられた先読み記号 (Eq,Hashを実装できないので別に持つ)
type LRItemSet = BTreeMap<LRCoreItem, IndexSet<SymbolID>>;
type LRCoreItems = BTreeSet<LRCoreItem>;

#[derive(Debug, Copy, Clone)]
pub(crate) enum Action {
    Shift(NodeID),
    Reduce(RuleID),
    Accept,
}

impl DFANode {
    pub(crate) fn parse_actions(&self, grammar: &Grammar) -> IndexMap<SymbolID, Action> {
        let mut actions = IndexMap::default();

        // shift, goto
        for (label, target) in &self.edges {
            match actions.entry(*label) {
                Entry::Occupied(..) => panic!("conflict"),
                Entry::Vacant(entry) => {
                    entry.insert(Action::Shift(*target));
                }
            }
        }

        // reduce, accept
        for (core_item, lookaheads) in &self.item_set {
            let rule = &grammar.rule(core_item.rule_id);
            if core_item.marker < rule.right().len() {
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

#[derive(Debug, Copy, Clone)]
enum MergeMode {
    /// Items are equivalent in the sense of of Knuth's canonical LR(1) method,
    /// that is, each item sets have the same LR(0) cores and their lookahead symbols
    /// are also equal.
    Canonical,

    /// Items are weakly compatible in the sense of Pager's Practical General Method (PGM).
    PGM,

    /// Items are compatible in the sense of DeRemer's LALR(1) method, that is,
    /// each item sets have the same LR(0) cores but different lookahead symbols.
    LALR,
}

#[derive(Debug)]
struct PendingNodes {
    next_node_id: u64,
    queue: VecDeque<(NodeID, LRItemSet, Option<NodeID>)>,
}
impl PendingNodes {
    /// Push a LR(1) item set into the queue, and obtain registered NodeID.
    fn enqueue(&mut self, item_set: LRItemSet, prev_node: Option<NodeID>) -> NodeID {
        let id = NodeID::new(self.next_node_id);
        self.next_node_id += 1;
        self.queue.push_back((id, item_set, prev_node));
        id
    }

    /// Pop a LR(1) item set from the queue.
    fn dequeue(&mut self) -> Option<(NodeID, LRItemSet, Option<NodeID>)> {
        self.queue.pop_front()
    }
}

#[derive(Debug)]
struct NodeExtractor<'g> {
    grammar: &'g Grammar,
    first_sets: FirstSets,
}

impl NodeExtractor<'_> {
    /// クロージャ展開
    fn expand_closures(&self, items: &mut LRItemSet) {
        let mut changed = true;
        while changed {
            changed = false;

            // 候補の抽出
            let mut added: IndexMap<LRCoreItem, IndexSet<SymbolID>> = IndexMap::default();
            for (core, lookaheads) in &mut *items {
                let rule = self.grammar.rule(core.rule_id);

                // [X -> ... @ Y beta]
                //  Y: one nonterminal symbol
                let (y_symbol, beta) = match &rule.right()[core.marker..] {
                    [y_symbol, beta @ ..] if !self.grammar.symbol(*y_symbol).is_terminal() => {
                        (*y_symbol, beta)
                    }
                    _ => continue,
                };

                // lookaheads = {x1,x2,...,xk} としたとき、
                //   x \in First(beta x1) \cup ... \cup First(beta xk)
                // を満たすすべての終端記号を考える
                let x = self.first_sets.get(beta, &*lookaheads);
                for (rule_id, rule) in self.grammar.rules() {
                    // Y: ... という形式の構文規則のみを対象にする
                    if rule.left() != y_symbol {
                        continue;
                    }

                    added
                        .entry(LRCoreItem { rule_id, marker: 0 })
                        .or_default()
                        .extend(&x);
                }
            }

            for (core, lookaheads) in added {
                let slot = items.entry(core).or_insert_with(|| {
                    changed = true;
                    IndexSet::default()
                });
                for l in lookaheads {
                    changed |= slot.insert(l);
                }
            }
        }
    }

    /// 指定したLRアイテム集合から遷移先のLRアイテム集合（未展開）とラベルを抽出する
    fn extract_transitions(&self, items: &LRItemSet) -> IndexMap<SymbolID, LRItemSet> {
        let mut item_sets: IndexMap<SymbolID, LRItemSet> = IndexMap::default();
        for (core, lookaheads) in items {
            let rule = self.grammar.rule(core.rule_id);

            // markerが終わりまで到達していれば無視する
            if core.marker >= rule.right().len() {
                continue;
            }

            let label = rule.right()[core.marker];
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
    nodes: IndexMap<NodeID, (LRItemSet, IndexMap<SymbolID, NodeID>)>,
    same_cores: IndexMap<LRCoreItems, IndexSet<NodeID>>,
    mode: MergeMode,
}

impl<'g> DFAGenerator<'g> {
    fn new(grammar: &'g Grammar, mode: MergeMode) -> Self {
        let mut pending_nodes = PendingNodes {
            next_node_id: 1,
            queue: VecDeque::new(),
        };
        let mut item_set = BTreeMap::new();
        item_set.insert(
            LRCoreItem {
                rule_id: RuleID::ACCEPT,
                marker: 0,
            },
            Some(SymbolID::EOI).into_iter().collect(),
        );
        pending_nodes
            .queue
            .push_back((NodeID::START, item_set, None));

        Self {
            extractor: NodeExtractor {
                grammar,
                first_sets: FirstSets::new(grammar),
            },
            pending_nodes,
            nodes: IndexMap::default(),
            same_cores: IndexMap::default(),
            mode,
        }
    }

    fn populate_nodes(&mut self) {
        // 新規にノードが生成されなくなるまで繰り返す
        'dequeue: while let Some((new_id, mut new_item_set, prev_node)) =
            self.pending_nodes.dequeue()
        {
            // クロージャ展開
            self.extractor.expand_closures(&mut new_item_set);

            let core_items: LRCoreItems = new_item_set.keys().copied().collect();

            // 互換性のあるノードを検索し、存在する場合はそのノードを修正し新規にノードは生成しない
            if let Some(same_cores) = self.same_cores.get(&core_items) {
                for &orig_id in same_cores {
                    let orig_node = &mut self.nodes[&orig_id];
                    match compare_item_sets(self.mode, &orig_node.0, &new_item_set) {
                        ItemSetDiff::Same => {
                            // 完全に一致しているので修正すら不要
                        }

                        ItemSetDiff::Compatible => {
                            // 先読み記号をマージする
                            let mut modified = false;
                            for (new_core, new_lookaheads) in &new_item_set {
                                let lookaheads = orig_node.0.get_mut(new_core).unwrap();
                                for l in new_lookaheads {
                                    modified |= lookaheads.insert(*l);
                                }
                            }

                            // 先読み記号の変更があった場合のみ後続ノードの生成を再度行なう
                            if modified {
                                for (symbol, new_item_set) in
                                    self.extractor.extract_transitions(&new_item_set)
                                {
                                    let id =
                                        self.pending_nodes.enqueue(new_item_set, Some(orig_id));
                                    orig_node.1.insert(symbol, id);
                                }
                            }
                        }

                        // LALR が無効化されている場合など
                        ItemSetDiff::Different => continue,
                    }

                    // 使用する予定だったNodeIDはenqueueされた時点で遷移前のnodeに登録されているので書き換える
                    if let Some(prev_node_id) = prev_node {
                        let prev_node = &mut self.nodes[&prev_node_id];
                        for edge in prev_node.1.values_mut() {
                            if *edge == new_id {
                                *edge = orig_id;
                            }
                        }
                    }

                    continue 'dequeue;
                }
            }

            // 遷移先のitems setを生成し、ノード生成のキューに登録する
            let mut edges = IndexMap::default();
            for (symbol, new_item_set) in self.extractor.extract_transitions(&new_item_set) {
                let id = self.pending_nodes.enqueue(new_item_set, Some(new_id));
                edges.insert(symbol, id);
            }

            self.nodes.insert(new_id, (new_item_set, edges));

            self.same_cores
                .entry(core_items)
                .or_default()
                .insert(new_id);
        }
    }

    fn finalize(self) -> DFA {
        // 状態ノードの併合によってIDが飛び飛びになってる可能性があるので圧縮する
        let mut new_node_ids = IndexMap::default();
        let mut next_new_id = 0;
        for &orig_id in self.nodes.keys() {
            let new_id = NodeID::new(next_new_id);
            next_new_id += 1;
            new_node_ids.insert(orig_id, new_id);
        }
        let nodes = self
            .nodes
            .into_iter()
            .map(|(orig_id, (item_set, edges))| {
                let id = new_node_ids[&orig_id];
                let edges = edges
                    .into_iter()
                    .map(|(s, id)| (s, new_node_ids[&id]))
                    .collect();
                (id, DFANode { item_set, edges })
            })
            .collect();

        DFA { nodes }
    }
}

enum ItemSetDiff {
    Same,
    Compatible,
    Different,
}

fn compare_item_sets(mode: MergeMode, left: &LRItemSet, right: &LRItemSet) -> ItemSetDiff {
    // Assume that `left` and `right` have the same LR(0) core.

    let mut is_canonically_same = true;
    for (left, right) in left.values().zip(right.values()) {
        if !left.is_superset(right) {
            is_canonically_same = false;
            break;
        }
    }
    if is_canonically_same {
        return ItemSetDiff::Same;
    }

    match mode {
        MergeMode::LALR => ItemSetDiff::Compatible,
        MergeMode::PGM if is_pgm_weakly_compatible(left, right) => ItemSetDiff::Compatible,
        _ => ItemSetDiff::Different,
    }
}

fn is_pgm_weakly_compatible(left: &LRItemSet, right: &LRItemSet) -> bool {
    is_pgm_weakly_compatible_c1(left, right)
        || is_pgm_weakly_compatible_c2(left)
        || is_pgm_weakly_compatible_c2(right)
}

fn is_pgm_weakly_compatible_c1(left: &LRItemSet, right: &LRItemSet) -> bool {
    for (i, left) in left.values().enumerate() {
        for (j, right) in right.values().enumerate() {
            if i == j {
                continue;
            }
            if !left.is_disjoint(right) {
                return false;
            }
        }
    }
    true
}

fn is_pgm_weakly_compatible_c2(items: &LRItemSet) -> bool {
    for (i, c1) in items.values().enumerate() {
        for c2 in items.values().skip(i + 1) {
            if c1.is_disjoint(c2) {
                return false;
            }
        }
    }
    true
}

#[derive(Debug)]
struct FirstSets {
    nulls: IndexSet<SymbolID>,
    first_sets: IndexMap<SymbolID, IndexSet<SymbolID>>,
}

impl FirstSets {
    fn new(grammar: &Grammar) -> Self {
        let nulls = nulls_set(grammar);
        let first_sets = first_set(grammar, &nulls);
        Self { nulls, first_sets }
    }

    /// `First(prefix lookaheads)`
    pub fn get<'i, I>(&self, prefix: &[SymbolID], lookaheads: I) -> IndexSet<SymbolID>
    where
        I: IntoIterator<Item = &'i SymbolID>,
    {
        let mut res = IndexSet::default();

        let mut is_end = false;
        for token in prefix {
            res.extend(self.first_sets[token].iter().copied());
            if !self.nulls.contains(token) {
                is_end = true;
                break;
            }
        }

        if !is_end {
            res.extend(lookaheads.into_iter().flat_map(|x| &self.first_sets[x]));
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoketest1() {
        let grammar = Grammar::define(|def| {
            let equal = def.token("EQUAL")?;
            let plus = def.token("PLUS")?;
            let ident = def.token("ID")?;
            let num = def.token("NUM")?;

            let a = def.symbol("A")?;
            let e = def.symbol("E")?;
            let t = def.symbol("T")?;

            def.start_symbol(a)?;

            def.rule("A1", a, [e, equal, e])?;
            def.rule("A2", a, [ident])?;
            def.rule("E1", e, [e, plus, t])?;
            def.rule("E2", e, [t])?;
            def.rule("T1", t, [num])?;
            def.rule("T2", t, [ident])?;

            Ok(())
        })
        .unwrap();
        eprintln!("{}", grammar);

        let dfa = DFA::generate(&grammar);
        eprintln!("DFA Nodes:\n---\n{}", dfa.display(&grammar));
    }

    #[test]
    fn smoketest2() {
        let grammar = Grammar::define(|g| {
            // declare terminal symbols.
            let lparen = g.token("LPAREN")?;
            let rparen = g.token("RPAREN")?;
            let plus = g.token("PLUS")?;
            let minus = g.token("MINUS")?;
            let star = g.token("STAR")?;
            let slash = g.token("SLASH")?;
            let num = g.token("NUM")?;
            let _ = g.token("UNUSED_0")?;

            // declare nonterminal symbols.
            let expr = g.symbol("EXPR")?;
            let factor = g.symbol("FACTOR")?;
            let term = g.symbol("TERM")?;
            let _ = g.symbol("UNUSED_1")?;

            // declare syntax rules.
            g.rule("EXPR1", expr, [expr, plus, factor])?; // expr '+' factor
            g.rule("EXPR2", expr, [expr, minus, factor])?; // expr '-' factor
            g.rule("EXPR3", expr, [factor])?; // factor

            g.rule("FACTOR1", factor, [factor, star, term])?; // factor '*' term
            g.rule("FACTOR2", factor, [factor, slash, term])?; // factor '/' term
            g.rule("FACTOR3", factor, [term])?; // term

            g.rule("TERM1", term, [num])?; // num
            g.rule("TERM2", term, [lparen, expr, rparen])?; // '(' expr ')'

            g.start_symbol(expr)?;

            Ok(())
        })
        .unwrap();
        eprintln!("{}", grammar);

        let dfa = DFA::generate(&grammar);
        eprintln!("DFA Nodes:\n---\n{}", dfa.display(&grammar));
    }
}
