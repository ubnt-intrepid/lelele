use indexmap::{IndexMap, IndexSet};
use lelele::{Grammar, Symbol};
use std::mem;

fn main() {
    let mut builder = lelele::Grammar::builder();
    builder
    .start("EXPR")
        .terminals(&["NUM", "PLUS", "TIMES"])
        .rule("EXPR", &["EXPR", "PLUS", "TERM"])
        .rule("EXPR", &["TERM"])
        .rule("TERM", &["TERM", "TIMES", "NUM"])
        .rule("TERM", &["NUM"]);

    let grammar = builder.build();
    println!("Grammar:\n{}", grammar);

    let first_set = grammar.first_set();
    println!("First(EXPR): {:?}", first_set.get(&["EXPR"]));
    println!("First(TERM): {:?}", first_set.get(&["TERM"]));

    // DFA construction
    let mut gen = DFAGenerator { grammar: &grammar };
    let nodes = gen.process();

    println!("\nDFA nodes:");
    for (id, node) in &nodes {
        println!(" - {:02}:", id);
        println!("     item_set:");
        for item in &node.item_set {
            match item.rule_index {
                RuleIndex::Start => {
                    if item.marker == 0 {
                        println!("       - [S' -> @ {}]", grammar.start);
                    } else {
                        println!("       - [S' -> {} @]", grammar.start);
                    }
                }
                RuleIndex::Num(i) => {
                    let rule = &grammar.rules[i];
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
                    println!("]");
                }
            }
        }
        if !node.edges.is_empty() {
            println!("     edges:");
            for (symbol, id) in &node.edges {
                println!("       - {} -> {:02}", symbol, id);
            }
        }
    }
}

// LR(0) item
// X: Y1 Y2 ... Yn という構文規則があったとき、それに位置情報を付与したもの
// example:
//   [ X -> @ Y1   Y2 ... Yn ]
//   [ X ->   Y1 @ Y2 ... Yn ]
//   [ X ->   Y1   Y2 ... Yn @ ]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LR0Item {
    // grammer内におけるruleの位置
    rule_index: RuleIndex,
    // marker位置
    marker: usize,
}
pub type LR0ItemSet = IndexSet<LR0Item>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum RuleIndex {
    Start,
    Num(usize),
}

#[derive(Debug)]
pub struct DFANode {
    // 各DFA nodeに所属するLR(0) item
    pub item_set: LR0ItemSet,
    // 各DFAノード起点のedge
    pub edges: IndexMap<Symbol, usize>,
}
pub type NodeID = usize;

struct DFAGenerator<'g> {
    grammar: &'g Grammar,
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
            item_set: LR0ItemSet,
        }
        let mut pending_items: Vec<PendingItem> = vec![];
        pending_items.push({
            // 初期ノードの構築
            let mut item_set = IndexSet::new();
            // [S' -> @ S]
            item_set.insert(LR0Item {
                rule_index: RuleIndex::Start,
                marker: 0,
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
                let new_item_sets = self.extract_transitions(&item_set);
                for (symbol, new_items) in new_item_sets {
                    let id = if let Some(id) = nodes
                        .iter()
                        .find_map(|(id, node)| (node.item_set == new_items).then(|| *id))
                    {
                        // クロージャ展開後のitem setが同じなら同一のノードとみなす
                        id
                    } else {
                        // ノードを新規に作る
                        let id = node_id();
                        pending_items.push(PendingItem {
                            id,
                            item_set: new_items,
                        });
                        id
                    };
                    edges.insert(symbol, id);
                }

                nodes.insert(id, DFANode { item_set, edges });
            }
        }

        nodes
    }

    /// クロージャ展開
    fn expand_closures(&self, items: &mut LR0ItemSet) {
        let grammar = self.grammar;

        let mut changed = true;
        while changed {
            changed = false;

            let mut added = IndexSet::new();
            for LR0Item { rule_index, marker } in &*items {
                let y_symbol = match rule_index {
                    RuleIndex::Start if *marker == 0 => Some(grammar.start), // S' -> S @
                    RuleIndex::Start => None,                                // S' -> S @
                    RuleIndex::Num(i) => {
                        let rule = &grammar.rules[*i];
                        match &rule.rhs[*marker..] {
                            [y_symbol, ..] => Some(*y_symbol), // X -> ... @ Y [... one or more symbols]
                            _ => None,                         // X -> ... @ Y
                        }
                    }
                };

                if let Some(y_symbol) = y_symbol {
                    if grammar.nonterminals.contains(y_symbol) {
                        // nonterminal symbol Y に関する構文規則から LR(0) item を生成し追加する
                        for (i, rule) in grammar.rules.iter().enumerate() {
                            if rule.lhs == y_symbol {
                                added.insert(LR0Item {
                                    rule_index: RuleIndex::Num(i),
                                    marker: 0,
                                });
                            }
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
    fn extract_transitions(&self, items: &LR0ItemSet) -> IndexMap<Symbol, LR0ItemSet> {
        let grammar = self.grammar;

        let mut item_sets: IndexMap<Symbol, LR0ItemSet> = IndexMap::new();
        for item in items {
            match item.rule_index {
                RuleIndex::Start if item.marker == 0 => {
                    // add S' -> S @
                    item_sets.entry(grammar.start).or_default().insert(LR0Item {
                        rule_index: RuleIndex::Start,
                        marker: 1,
                    });
                }
                RuleIndex::Num(i) => {
                    let rule = &grammar.rules[i];
                    // markerが終わりまで到達していれば無視する
                    if item.marker >= rule.rhs.len() {
                        continue;
                    }
                    // edgeのラベルとなるsymbol
                    let label = rule.rhs[item.marker];
                    //
                    item_sets.entry(label).or_default().insert(LR0Item {
                        marker: item.marker + 1,
                        ..*item
                    });
                }
                _ => (),
            }
        }

        // a
        for item_set in item_sets.values_mut() {
            self.expand_closures(item_set);
        }

        item_sets
    }
}
