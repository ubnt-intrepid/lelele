//! Definition and generation of LR(1) automata.

use crate::{
    grammar::{Assoc, Grammar, Nonterminal, Precedence, Rule, RuleID, Symbol, Terminal},
    IndexMap, IndexSet,
};
use std::{
    borrow::{Borrow, Cow},
    cmp::Ordering,
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
}

impl fmt::Display for DFA {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, (id, node)) in self.nodes().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }

            writeln!(f, "#### State {:02}", id)?;
            writeln!(f, "## item_sets")?;
            for (core_item, lookaheads) in &node.item_set {
                let LRCoreItem { rule, marker } = core_item;
                write!(f, "- {} :=", rule.left())?;
                for (i, prod) in rule.right().iter().enumerate() {
                    if i == *marker {
                        f.write_str(" .")?;
                    }
                    write!(f, " {}", prod)?;
                }
                if *marker == rule.right().len() {
                    f.write_str(" .")?;
                }
                write!(f, "  [")?;
                for (i, lookahead) in lookaheads.iter().enumerate() {
                    if i > 0 {
                        f.write_str(" ")?;
                    }
                    write!(f, "{}", lookahead)?;
                }
                writeln!(f, "]")?;
            }

            writeln!(f, "## actions")?;
            for (token, action) in &node.actions {
                match action {
                    Action::Shift(n) => {
                        writeln!(f, "- {} => shift({:02})", token, n)?;
                    }
                    Action::Reduce(reduce) => {
                        write!(f, "- {} => reduce({} :=", token, reduce.left())?;
                        for r in reduce.right() {
                            write!(f, " {}", r)?;
                        }
                        writeln!(f, ")")?;
                    }
                    Action::Accept => {
                        writeln!(f, "- {} => accept", token)?;
                    }
                    Action::Fail => {
                        writeln!(f, "- {} => fail", token)?;
                    }
                    Action::Inconsistent {
                        reason,
                        shift,
                        reduces,
                        accepted,
                    } => {
                        writeln!(f, "- {} => inconsistent(reason = {:?})", token, reason)?;
                        writeln!(f, "## conflicted actions")?;
                        if let Some(n) = shift {
                            writeln!(f, "  - shift({:02})", n)?;
                        }
                        for reduce in reduces {
                            write!(f, "  - reduce({} :=", reduce.left())?;
                            for r in reduce.right() {
                                write!(f, " {}", r)?;
                            }
                            writeln!(f, ")")?;
                        }
                        if *accepted {
                            writeln!(f, "  - accept")?;
                        }
                    }
                }
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
    actions: IndexMap<Terminal, Action>,
    gotos: IndexMap<Nonterminal, NodeID>,
}

// LR(1) item
// X: Y1 Y2 ... Yn という構文規則があったとき、それにマーカ位置を付与したもの
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct LRCoreItem {
    // grammer内におけるruleの識別子
    rule: Rule,
    // marker位置
    marker: usize,
}

//  - key: core item
//  - value: 紐付けられた先読み記号 (Eq,Hashを実装できないので別に持つ)
type LRItemSet = BTreeMap<LRCoreItem, IndexSet<Terminal>>;
type LRCoreItems = BTreeSet<LRCoreItem>;

impl DFANode {
    pub fn actions(&self) -> impl Iterator<Item = (&Terminal, &Action)> + '_ {
        self.actions.iter()
    }

    pub fn gotos(&self) -> impl Iterator<Item = (&Nonterminal, NodeID)> {
        self.gotos.iter().map(|(symbol, goto)| (symbol, *goto))
    }
}

/// The action that the LR automaton in a state performs on a particular
/// lookahead symbol.
#[derive(Debug)]
#[non_exhaustive]
pub enum Action {
    /// Read a lookahead symbol and transition to the specified state.
    Shift(NodeID),

    /// Reduce to the specified production rule.
    Reduce(Rule),

    /// Reduce to the top-level production rule and accept symbols.
    Accept,

    /// Reject the specified lookahead symbol.
    ///
    /// The behavior of this action is the same as if no action exists
    /// for the given lookahead symbol, but is explicitly inserted by
    /// resolving some shift/reduce conflicts.
    Fail,

    /// There are multiple conflicting actions for the lookahead symbol.
    Inconsistent {
        shift: Option<NodeID>,
        reduces: Vec<Rule>,
        accepted: bool,
        reason: ConflictReason,
    },
}
impl Action {
    pub fn is_consistent(&self) -> bool {
        !matches!(self, Self::Inconsistent { .. })
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub enum ConflictReason {
    /// The state has multiple reductions even though it has no shift action
    /// on the target lookahead symbol.
    MultipleReductionWithoutShift,

    /// At least one of the competing actions has no precedence.
    MissingPrecedence,

    /// At least one of shift/reduce conflict resolution result is
    /// inconsistent with the others.
    InconsistentShiftResolution,
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
            let mut added: IndexMap<LRCoreItem, IndexSet<Terminal>> = IndexMap::default();
            for (core, lookaheads) in &mut *items {
                let rule = &core.rule;

                // [X -> ... @ Y beta]
                //  Y: one nonterminal symbol
                let (y_symbol, beta) = match &rule.right()[core.marker..] {
                    [Symbol::N(y_symbol), beta @ ..] => (y_symbol, beta),
                    _ => continue,
                };

                // lookaheads = {x1,x2,...,xk} としたとき、
                //   x \in First(beta x1) \cup ... \cup First(beta xk)
                // を満たすすべての終端記号を考える
                let x = self.first_sets.get(beta, lookaheads.iter().cloned());
                for rule in self.grammar.rules() {
                    // Y: ... という形式の構文規則のみを対象にする
                    if rule.left().id() != y_symbol.id() {
                        continue;
                    }

                    added
                        .entry(LRCoreItem {
                            rule: rule.clone(),
                            marker: 0,
                        })
                        .or_default()
                        .extend(x.iter().cloned());
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
    fn extract_transitions(&self, items: &LRItemSet) -> IndexMap<Symbol, LRItemSet> {
        let mut item_sets: IndexMap<Symbol, LRItemSet> = IndexMap::default();
        for (core, lookaheads) in items {
            let rule = &core.rule;

            // markerが終わりまで到達していれば無視する
            if core.marker >= rule.right().len() {
                continue;
            }

            let label = &rule.right()[core.marker];
            let new_item_set = item_sets.entry(label.clone()).or_default();
            new_item_set
                .entry(LRCoreItem {
                    marker: core.marker + 1,
                    ..core.clone()
                })
                .or_default()
                .extend(lookaheads.iter().cloned());
        }
        item_sets
    }
}

#[derive(Debug)]
struct DFAGenerator<'g> {
    extractor: NodeExtractor<'g>,
    pending_nodes: PendingNodes,
    nodes: IndexMap<NodeID, (LRItemSet, IndexMap<Symbol, NodeID>)>,
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
                rule: grammar.accept_rule().clone(),
                marker: 0,
            },
            Some(grammar.eoi().clone()).into_iter().collect(),
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

            let core_items: LRCoreItems = new_item_set.keys().cloned().collect();

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
                                    modified |= lookaheads.insert(l.clone());
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

        let mut nodes = IndexMap::default();
        for (orig_id, (item_set, edges)) in self.nodes {
            let id = new_node_ids[&orig_id];

            let mut pending_actions: IndexMap<Terminal, PendingAction> = IndexMap::default();
            let mut gotos: IndexMap<Nonterminal, NodeID> = IndexMap::default();
            for (symbol, target) in edges {
                // shift, goto
                let target = new_node_ids[&target];
                match symbol {
                    Symbol::T(t) => {
                        let action = pending_actions.entry(t).or_default();
                        action.shift.replace(target);
                    }
                    Symbol::N(n) => {
                        gotos.insert(n, target);
                    }
                }
            }
            for (core_item, lookaheads) in &item_set {
                // reduce, accept
                if core_item.marker < core_item.rule.right().len() {
                    continue;
                }
                for lookahead in lookaheads {
                    let action = pending_actions.entry(lookahead.clone()).or_default();
                    if core_item.rule.id() == RuleID::ACCEPT {
                        action.accepted = true;
                    } else {
                        action.reduces.push(core_item.rule.clone());
                    }
                }
            }

            let mut actions: IndexMap<Terminal, Action> = IndexMap::default();
            for (symbol, action) in pending_actions {
                let resolved = match resolve_conflict(&symbol, &action) {
                    Ok(resolved) => resolved,
                    Err(reason) => Action::Inconsistent {
                        reason,
                        shift: action.shift,
                        reduces: action.reduces,
                        accepted: action.accepted,
                    },
                };
                actions.insert(symbol, resolved);
            }

            nodes.insert(
                id,
                DFANode {
                    item_set,
                    actions,
                    gotos,
                },
            );
        }

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
    nulls: IndexSet<Nonterminal>,
    map: IndexMap<Symbol, IndexSet<Terminal>>,
}

impl FirstSets {
    fn new(grammar: &Grammar) -> Self {
        let nulls = nulls_set(grammar);

        // First(T) = {} と初期化する
        let mut map: IndexMap<Symbol, IndexSet<Terminal>> = IndexMap::default();
        for terminal in grammar.terminals() {
            map.insert(
                Symbol::T(terminal.clone()),
                Some(terminal.clone()).into_iter().collect(),
            );
        }
        for symbol in grammar.nonterminals() {
            map.insert(Symbol::N(symbol.clone()), IndexSet::default());
        }

        // 制約条件の抽出
        // X -> Y1 Y2 ... Yn という構文規則に対し、
        //  1. Y1,Y2,...と検索していき、最初に来る非nullableな記号を Yk とする
        //    - Y1 Y2 ... Y(k-1) が nullable で Yk が non-nullable となる
        //  2. Yi (i=1,2,..,k) それぞれに対し First(X) \supseteq First(Yi) という制約を追加する
        #[derive(Debug)]
        struct Constraint<'g> {
            sup: Cow<'g, Symbol>,
            sub: &'g Symbol,
        }
        let mut constraints = vec![];
        for rule in grammar.rules().filter(|rule| rule.id() != RuleID::ACCEPT) {
            for symbol in rule.right() {
                if !matches!(symbol, Symbol::N(n) if rule.left().id() == n.id()) {
                    constraints.push(Constraint {
                        sup: Cow::Owned(Symbol::N(rule.left().clone())),
                        sub: symbol,
                    });
                }
                if !matches!(symbol, Symbol::N(n) if nulls.contains(n)) {
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
                let mut superset = map.remove((**sup).borrow()).unwrap();
                let subset = map.get(*sub).unwrap();
                for tok in subset {
                    if !superset.contains(tok) {
                        superset.insert(tok.clone());
                        changed = true;
                    }
                }
                map.insert((**sup).to_owned(), superset);
            }
        }

        Self { nulls, map }
    }

    /// `First(prefix lookaheads)`
    pub fn get<L>(&self, prefix: &[Symbol], lookaheads: L) -> IndexSet<Terminal>
    where
        L: IntoIterator<Item = Terminal>,
    {
        let mut res = IndexSet::default();

        let mut is_end = false;
        for symbol in prefix {
            res.extend(self.map[symbol].iter().cloned());
            if !matches!(symbol, Symbol::N(n) if self.nulls.contains(n)) {
                is_end = true;
                break;
            }
        }

        if !is_end {
            res.extend(lookaheads);
        }

        res
    }
}

/// Calculate the set of nullable symbols in this grammar.
fn nulls_set(grammar: &Grammar) -> IndexSet<Nonterminal> {
    // ruleからnullableであることが分かっている場合は追加する
    let mut nulls: IndexSet<Nonterminal> = grammar
        .rules()
        .filter_map(|rule| rule.right().is_empty().then(|| rule.left().clone()))
        .collect();

    // 値が更新されなくなるまで繰り返す
    let mut changed = true;
    while changed {
        changed = false;
        for rule in grammar.rules() {
            if nulls.contains(rule.left()) {
                continue;
            }
            // 右辺のsymbolsがすべてnullableかどうか
            let is_rhs_nullable = rule
                .right()
                .iter()
                .all(|symbol| matches!(symbol, Symbol::N(n) if nulls.contains(n)));
            if is_rhs_nullable {
                changed = true;
                nulls.insert(rule.left().clone());
                continue;
            }
        }
    }

    nulls
}

#[derive(Debug, Default)]
struct PendingAction {
    shift: Option<NodeID>,
    reduces: Vec<Rule>,
    accepted: bool,
}

/// Attempts to resolve shift/reduce conflicts based on precedence/associativity.
fn resolve_conflict(symbol: &Terminal, action: &PendingAction) -> Result<Action, ConflictReason> {
    use Action::*;

    if action.accepted {
        debug_assert!(action.shift.is_none(), "detect shift/accept conflict");
        debug_assert!(action.reduces.is_empty(), "detect reduce/accept conflict");
        return Ok(Accept);
    }

    match (action.shift, &action.reduces[..]) {
        (Some(next), []) => Ok(Shift(next)),
        (None, [reduce]) => Ok(Reduce(reduce.clone())),

        (Some(next), [reduce, remains @ ..]) => {
            let shift_prec = symbol.precedence();

            let reduce_prec = reduce.precedence();
            let resolved = resolve_shift_reduce_conflict(shift_prec, reduce_prec)?;

            if matches!(resolved, Some(false)) && !remains.is_empty() {
                return Err(ConflictReason::InconsistentShiftResolution);
            }

            for reduce in remains {
                let reduce_prec = reduce.precedence();
                let new_resolved = resolve_shift_reduce_conflict(shift_prec, reduce_prec)?;
                if resolved != new_resolved {
                    return Err(ConflictReason::InconsistentShiftResolution);
                }
            }

            match resolved {
                Some(true) => Ok(Shift(next)),
                Some(false) => Ok(Reduce(reduce.clone())),
                None => Ok(Fail),
            }
        }

        (None, [_, ..]) => Err(ConflictReason::MultipleReductionWithoutShift),

        (None, []) => unreachable!(),
    }
}

fn resolve_shift_reduce_conflict(
    shift_prec: Option<&Precedence>,
    reduce_prec: Option<&Precedence>,
) -> Result<Option<bool>, ConflictReason> {
    match (shift_prec, reduce_prec) {
        (Some(p1), Some(p2)) => match Ord::cmp(&p1.priority, &p2.priority) {
            Ordering::Greater => Ok(Some(true)),
            Ordering::Less => Ok(Some(false)),
            Ordering::Equal => match p1.assoc {
                Assoc::Left => Ok(Some(false)),
                Assoc::Right => Ok(Some(true)),
                Assoc::Nonassoc => Ok(None),
            },
        },
        _ => return Err(ConflictReason::MissingPrecedence), // 比較不可能
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoketest1() {
        let grammar = Grammar::define(|def| {
            let equal = def.terminal("EQUAL", None)?;
            let plus = def.terminal("PLUS", None)?;
            let ident = def.terminal("ID", None)?;
            let num = def.terminal("NUM", None)?;

            let a = def.nonterminal("A")?;
            let e = def.nonterminal("E")?;
            let t = def.nonterminal("T")?;

            def.start_symbol(a)?;

            def.rule(a, [e, equal, e], None)?;
            def.rule(a, [ident], None)?;
            def.rule(e, [e, plus, t], None)?;
            def.rule(e, [t], None)?;
            def.rule(t, [num], None)?;
            def.rule(t, [ident], None)?;

            Ok(())
        })
        .unwrap();
        eprintln!("{}", grammar);

        let dfa = DFA::generate(&grammar);
        eprintln!("DFA Nodes:\n---\n{}", dfa);
    }

    #[test]
    fn smoketest2() {
        let grammar = Grammar::define(|g| {
            // declare terminal symbols.
            let lparen = g.terminal("LPAREN", None)?;
            let rparen = g.terminal("RPAREN", None)?;
            let plus = g.terminal("PLUS", None)?;
            let minus = g.terminal("MINUS", None)?;
            let star = g.terminal("STAR", None)?;
            let slash = g.terminal("SLASH", None)?;
            let num = g.terminal("NUM", None)?;
            let _ = g.terminal("UNUSED_0", None)?;

            // declare nonterminal symbols.
            let expr = g.nonterminal("EXPR")?;
            let factor = g.nonterminal("FACTOR")?;
            let term = g.nonterminal("TERM")?;
            let _ = g.nonterminal("UNUSED_1")?;

            // declare syntax rules.
            g.rule(expr, [expr, plus, factor], None)?;
            g.rule(expr, [expr, minus, factor], None)?;
            g.rule(expr, [factor], None)?;

            g.rule(factor, [factor, star, term], None)?;
            g.rule(factor, [factor, slash, term], None)?;
            g.rule(factor, [term], None)?;

            g.rule(term, [num], None)?;
            g.rule(term, [lparen, expr, rparen], None)?;

            g.start_symbol(expr)?;

            Ok(())
        })
        .unwrap();
        eprintln!("{}", grammar);

        let dfa = DFA::generate(&grammar);
        eprintln!("DFA Nodes:\n---\n{}", dfa);
    }
}
