use indexmap::{IndexMap, IndexSet};
use lelele::{FirstSet, Grammar, RuleID, Symbol};
use std::mem;

fn main() {
    let mut builder = lelele::Grammar::builder();
    builder
        .start("A")
        .terminals(&["EQUAL", "PLUS", "ID", "NUM"])
        .rule("A", &["E", "EQUAL", "E"])
        .rule("A", &["ID"])
        .rule("E", &["E", "PLUS", "T"])
        .rule("E", &["T"])
        .rule("T", &["NUM"])
        .rule("T", &["ID"]);

    let grammar = builder.build();
    println!("Grammar:\n{}", grammar);

    let first_set = grammar.first_set();
    println!("First(A): {:?}", first_set.get(&[], "A"));
    println!("First(E): {:?}", first_set.get(&[], "E"));
    println!("First(T): {:?}", first_set.get(&[], "T"));

    // DFA construction
    let mut gen = DFAGenerator {
        grammar: &grammar,
        first_set,
    };
    let nodes = gen.process();

    println!("\nDFA nodes:");
    for (id, node) in &nodes {
        println!(" - {:02}:", id);
        println!("     item_set:");
        for item in &node.item_set {
            let rule = &grammar.rules.get(&item.rule_id).unwrap();
            print!("       - [{} -> ", rule.lhs);
            for (i, s) in rule.rhs.iter().enumerate() {
                if i > 0 {
                    print!(" ");
                }
                if i == item.marker {
                    print!("@ ");
                }
                print!("{}", s);
            }
            if item.marker == rule.rhs.len() {
                print!(" @");
            }
            println!("] {{ {:?} }}", item.lookahead);
        }
        if !node.edges.is_empty() {
            println!("     edges:");
            for (symbol, id) in &node.edges {
                println!("       - {} -> {:02}", symbol, id);
            }
        }
    }
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
    rule_id: RuleID,
    // marker位置
    marker: usize,
    // 先読み記号
    lookahead: Symbol,
}
pub type LRItemSet = IndexSet<LRItem>;

#[derive(Debug)]
pub struct DFANode {
    // 各DFA nodeに所属するLR item set
    pub item_set: LRItemSet,
    // 各DFAノード起点のedge
    pub edges: IndexMap<Symbol, usize>,
}
pub type NodeID = usize;

struct DFAGenerator<'g> {
    grammar: &'g Grammar,
    first_set: FirstSet,
}

impl DFAGenerator<'_> {
    fn process(&mut self) -> IndexMap<NodeID, DFANode> {
        let mut nodes: IndexMap<NodeID, DFANode> = IndexMap::new();
        let mut next_node_id = 0;
        let mut node_id = || {
            let id = next_node_id;
            next_node_id += 1;
            id
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
                lookahead: "$",
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

        nodes
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
                let new_lookahead: Vec<Symbol> =
                    self.first_set.get(beta, &lookahead).into_iter().collect();
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
    fn extract_transitions(&self, items: &LRItemSet) -> IndexMap<Symbol, LRItemSet> {
        let mut item_sets: IndexMap<Symbol, LRItemSet> = IndexMap::new();
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
}
