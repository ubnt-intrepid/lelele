//! Definition and generation of LR(1) automata.

use crate::{
    grammar::{Assoc, Grammar, NonterminalID, Precedence, RuleID, Symbol, TerminalID},
    IndexMap, IndexSet,
};
use std::{
    borrow::{Borrow, Cow},
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet, VecDeque},
    fmt,
};

#[derive(Debug, thiserror::Error)]
pub enum DFAError {
    #[error("error during resolving conflicts")]
    ConflictResolution(
        #[from]
        #[source]
        ConflictResolutionError,
    ),
}

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

    pub fn generate(&self, grammar: &Grammar) -> Result<DFA, DFAError> {
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
    pub fn generate(grammar: &Grammar) -> Result<Self, DFAError> {
        Config::new().generate(grammar)
    }

    pub fn nodes(&self) -> impl Iterator<Item = (NodeID, &DFANode)> + '_ {
        self.nodes.iter().map(|(id, node)| (*id, node))
    }

    pub fn node(&self, id: NodeID) -> &DFANode {
        &self.nodes[&id]
    }

    pub fn display<'g>(&'g self, g: &'g Grammar) -> impl fmt::Display + 'g {
        DFADisplay {
            grammar: g,
            dfa: self,
        }
    }
}

struct DFADisplay<'g> {
    grammar: &'g Grammar,
    dfa: &'g DFA,
}
impl fmt::Display for DFADisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { grammar, dfa, .. } = self;

        for (i, (id, node)) in dfa.nodes().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }

            writeln!(f, "#### State {:02}", id)?;
            writeln!(f, "## item_sets")?;
            for (core_item, lookaheads) in &node.item_set {
                write!(f, "- {}  [", core_item.display(grammar))?;
                for (i, lookahead) in lookaheads.iter().enumerate() {
                    if i > 0 {
                        f.write_str(" ")?;
                    }
                    write!(f, "{}", grammar.terminal(lookahead))?;
                }
                f.write_str("]\n")?;
            }

            writeln!(f, "## actions")?;
            for (token, action) in &node.actions {
                let token = grammar.terminal(token);
                match action {
                    Action::Shift(n) => {
                        writeln!(f, "- {} => shift({:02})", token, n)?;
                    }
                    Action::Reduce(reduce) => {
                        let reduce = grammar.rule(reduce);
                        writeln!(f, "- {} => reduce({})", token, reduce.display(grammar))?;
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
                    } => {
                        let token = grammar.terminal(token);
                        writeln!(f, "- {} => inconsistent(reason = {:?})", token, reason)?;
                        writeln!(f, "## conflicted actions on {}", token)?;
                        if let Some(n) = shift {
                            writeln!(f, "  - shift({:02})", n)?;
                            writeln!(f, "  ## corresponding items:")?;
                            for core_item in node.item_set.keys() {
                                let rule = self.grammar.rule(&core_item.rule);
                                if rule.right().get(core_item.marker).map_or(
                                    false,
                                    |n| matches!(n, Symbol::T(t) if *t == token.id()),
                                ) {
                                    writeln!(f, "    - {}", core_item.display(self.grammar))?;
                                }
                            }
                        }
                        for reduce in reduces {
                            let reduce = grammar.rule(reduce);
                            writeln!(f, "  - reduce({})", reduce.display(grammar))?;
                        }
                    }
                }
            }

            writeln!(f, "## gotos")?;
            for (symbol, goto) in &node.gotos {
                writeln!(f, "- {} => goto({:02})", grammar.nonterminal(symbol), goto)?;
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
    pub(crate) actions: IndexMap<TerminalID, Action>,
    pub(crate) gotos: IndexMap<NonterminalID, NodeID>,
}

// LR(1) item
// X: Y1 Y2 ... Yn という構文規則があったとき、それにマーカ位置を付与したもの
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct LRCoreItem {
    // grammer内におけるruleの識別子
    rule: RuleID,
    // marker位置
    marker: usize,
}
impl LRCoreItem {
    fn display<'g>(&'g self, grammar: &'g Grammar) -> impl fmt::Display + 'g {
        LRCoreItemDisplay {
            core_item: self,
            grammar,
        }
    }
}

struct LRCoreItemDisplay<'g> {
    core_item: &'g LRCoreItem,
    grammar: &'g Grammar,
}

impl fmt::Display for LRCoreItemDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let LRCoreItem { rule, marker } = self.core_item;
        let rule = self.grammar.rule(rule);
        write!(f, "({} :=", self.grammar.nonterminal(&rule.left()))?;
        for (i, prod) in rule.right().iter().enumerate() {
            if i == *marker {
                f.write_str(" .")?;
            }
            match prod {
                Symbol::T(t) => write!(f, " {}", self.grammar.terminal(t))?,
                Symbol::N(n) => write!(f, " {}", self.grammar.nonterminal(n))?,
            }
        }
        if *marker == rule.right().len() {
            f.write_str(" .")?;
        }

        f.write_str(")")
    }
}

//  - key: core item
//  - value: 紐付けられた先読み記号 (Eq,Hashを実装できないので別に持つ)
type LRItemSet = BTreeMap<LRCoreItem, IndexSet<TerminalID>>;
type LRCoreItems = BTreeSet<LRCoreItem>;

impl DFANode {
    pub fn actions(&self) -> impl Iterator<Item = (TerminalID, &Action)> + '_ {
        self.actions.iter().map(|(token, action)| (*token, action))
    }

    pub fn gotos(&self) -> impl Iterator<Item = (NonterminalID, NodeID)> + '_ {
        self.gotos.iter().map(|(symbol, goto)| (*symbol, *goto))
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
    Reduce(RuleID),

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
        reduces: Vec<RuleID>,
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
            let mut added: IndexMap<LRCoreItem, IndexSet<TerminalID>> = IndexMap::default();
            for (core, lookaheads) in &mut *items {
                let rule = self.grammar.rule(&core.rule);

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
                    if rule.left() != *y_symbol {
                        continue;
                    }

                    added
                        .entry(LRCoreItem {
                            rule: rule.id(),
                            marker: 0,
                        })
                        .or_default()
                        .extend(x.iter().copied());
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
            let rule = self.grammar.rule(&core.rule);

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
                rule: RuleID::ACCEPT,
                marker: 0,
            },
            Some(TerminalID::EOI).into_iter().collect(),
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

    fn finalize(self) -> Result<DFA, DFAError> {
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

            #[derive(Default)]
            struct PendingAction {
                shift: Option<NodeID>,
                reduces: Vec<RuleID>,
            }
            let mut pending_actions: IndexMap<TerminalID, PendingAction> = IndexMap::default();
            let mut gotos: IndexMap<NonterminalID, NodeID> = IndexMap::default();
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
                let rule = self.extractor.grammar.rule(&core_item.rule);
                // reduce, accept
                if core_item.marker < rule.right().len() {
                    continue;
                }
                for lookahead in lookaheads {
                    let action = pending_actions.entry(lookahead.clone()).or_default();
                    action.reduces.push(core_item.rule.clone());
                }
            }

            let mut actions: IndexMap<TerminalID, Action> = IndexMap::default();
            for (symbol, action) in pending_actions {
                let resolved = resolve_conflict(
                    self.extractor.grammar,
                    symbol,
                    action.shift,
                    &action.reduces,
                )?;
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

        Ok(DFA { nodes })
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
    nulls: IndexSet<NonterminalID>,
    map: IndexMap<Symbol, IndexSet<TerminalID>>,
}

impl FirstSets {
    fn new(grammar: &Grammar) -> Self {
        let nulls = nulls_set(grammar);

        // First(T) = {} と初期化する
        let mut map: IndexMap<Symbol, IndexSet<TerminalID>> = IndexMap::default();
        for terminal in grammar.terminals() {
            map.insert(
                Symbol::T(terminal.id()),
                Some(terminal.id()).into_iter().collect(),
            );
        }
        for symbol in grammar.nonterminals() {
            map.insert(Symbol::N(symbol.id()), IndexSet::default());
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
        for rule in grammar.rules() {
            for symbol in rule.right() {
                if !matches!(symbol, Symbol::N(n) if rule.left() == *n) {
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
    pub fn get<L>(&self, prefix: &[Symbol], lookaheads: L) -> IndexSet<TerminalID>
    where
        L: IntoIterator<Item = TerminalID>,
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
fn nulls_set(grammar: &Grammar) -> IndexSet<NonterminalID> {
    // ruleからnullableであることが分かっている場合は追加する
    let mut nulls: IndexSet<NonterminalID> = grammar
        .rules()
        .filter_map(|rule| rule.right().is_empty().then(|| rule.left()))
        .collect();

    // 値が更新されなくなるまで繰り返す
    let mut changed = true;
    while changed {
        changed = false;
        for rule in grammar.rules() {
            if nulls.contains(&rule.left()) {
                continue;
            }
            // 右辺のsymbolsがすべてnullableかどうか
            let is_rhs_nullable = rule
                .right()
                .iter()
                .all(|symbol| matches!(symbol, Symbol::N(n) if nulls.contains(n)));
            if is_rhs_nullable {
                changed = true;
                nulls.insert(rule.left());
                continue;
            }
        }
    }

    nulls
}

#[derive(Debug, thiserror::Error)]
pub enum ConflictResolutionError {
    #[error("there is no actions")]
    EmptyActions,

    #[error("detected shift/accept conflict(s)")]
    ShiftAcceptConflict,

    #[error("detected reduce/accept conflict(s)")]
    ReduceAcceptConflict,
}

/// Attempts to resolve shift/reduce conflicts based on precedence/associativity.
fn resolve_conflict(
    g: &Grammar,
    symbol: TerminalID,
    shift: Option<NodeID>,
    reduces: &[RuleID],
) -> Result<Action, ConflictResolutionError> {
    use Action::*;

    match (shift, reduces) {
        (Some(next), []) => Ok(Shift(next)),
        (None, [RuleID::ACCEPT]) => Ok(Accept),
        (None, [reduce]) => Ok(Reduce(*reduce)),
        (None, []) => Err(ConflictResolutionError::EmptyActions),

        // exactly one shift/reduce conflict
        (Some(next), [reduce]) => {
            if *reduce == RuleID::ACCEPT {
                return Err(ConflictResolutionError::ShiftAcceptConflict);
            }

            let shift_prec = g.terminal(&symbol).precedence();
            let reduce_prec = g.rule(reduce).precedence(g);

            match compare_precs(shift_prec, reduce_prec) {
                Some(PrecDiff::Left) => Ok(Shift(next)),
                Some(PrecDiff::Right) => Ok(Reduce(*reduce)),
                Some(PrecDiff::Neither) => Ok(Fail),
                None => Ok(Inconsistent {
                    shift,
                    reduces: reduces.to_owned(),
                    reason: ConflictReason::MissingPrecedence,
                }),
            }
        }

        // multiple shift/reduce conflicts
        (Some(next), reduces) => {
            let shift_prec = g.terminal(&symbol).precedence();

            let mut resolved = None;
            for reduce in reduces {
                if *reduce == RuleID::ACCEPT {
                    return Err(ConflictResolutionError::ShiftAcceptConflict);
                }
                let reduce_prec = g.rule(reduce).precedence(g);
                let new_resolved = match compare_precs(shift_prec, reduce_prec) {
                    Some(diff) => diff,
                    None => {
                        return Ok(Inconsistent {
                            shift,
                            reduces: reduces.to_owned(),
                            reason: ConflictReason::MissingPrecedence,
                        })
                    }
                };

                match (resolved, new_resolved) {
                    (Some(PrecDiff::Left), PrecDiff::Left)
                    | (Some(PrecDiff::Neither), PrecDiff::Neither) => (),
                    (None, diff) => resolved = Some(diff),
                    _ => {
                        return Ok(Inconsistent {
                            shift,
                            reduces: reduces.to_owned(),
                            reason: ConflictReason::InconsistentShiftResolution,
                        })
                    }
                }
            }

            match resolved {
                Some(PrecDiff::Left) => Ok(Shift(next)),
                Some(PrecDiff::Neither) => Ok(Fail),
                _ => unreachable!(),
            }
        }

        // reduce/reduce conflict(s)
        (None, reduces) => {
            debug_assert!(reduces.len() > 1);
            if reduces.contains(&RuleID::ACCEPT) {
                return Err(ConflictResolutionError::ReduceAcceptConflict);
            }

            Ok(Inconsistent {
                shift: None,
                reduces: reduces.to_owned(),
                reason: ConflictReason::MultipleReductionWithoutShift,
            })
        }
    }
}

#[derive(Copy, Clone)]
enum PrecDiff {
    Left,
    Right,
    Neither,
}
fn compare_precs(
    shift_prec: Option<&Precedence>,
    reduce_prec: Option<&Precedence>,
) -> Option<PrecDiff> {
    match (shift_prec, reduce_prec) {
        (Some(p1), Some(p2)) => match Ord::cmp(&p1.priority, &p2.priority) {
            Ordering::Greater => Some(PrecDiff::Left),
            Ordering::Less => Some(PrecDiff::Right),
            Ordering::Equal => match p1.assoc {
                Assoc::Left => Some(PrecDiff::Right),
                Assoc::Right => Some(PrecDiff::Left),
                Assoc::Nonassoc => Some(PrecDiff::Neither),
            },
        },
        _ => None,
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

        let dfa = DFA::generate(&grammar).unwrap();
        eprintln!("DFA Nodes:\n---\n{}", dfa.display(&grammar));
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

        let dfa = DFA::generate(&grammar).unwrap();
        eprintln!("DFA Nodes:\n---\n{}", dfa.display(&grammar));
    }
}
