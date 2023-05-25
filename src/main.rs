use indexmap::IndexMap;
use lelele::{
    grammar::{Grammar, SymbolID},
    parser::{ParseEvent, ParseItem, Parser},
};

fn main() {
    // 文法定義
    let grammar = Grammar::builder()
        .start("A")
        .rule("A", ["E", "EQUAL", "E"])
        .rule("A", ["ID"])
        .rule("E", ["E", "PLUS", "T"])
        .rule("E", ["T"])
        .rule("T", ["NUM"])
        .rule("T", ["ID"])
        .build();

    // シンボル名からSymbolIDへの逆引き用map
    let symbol_ids: IndexMap<&str, SymbolID> = grammar
        .symbols()
        .map(|(id, sym)| (sym.name(), id))
        .collect();
    let t_equal = symbol_ids["EQUAL"];
    let t_plus = symbol_ids["PLUS"];
    let t_ident = symbol_ids["ID"];
    let t_num = symbol_ids["NUM"];

    // 入力のトークン列
    let mut tokens = [
        (t_num, "1"),
        (t_plus, "+"),
        (t_num, "2"),
        (t_equal, "="),
        (t_ident, "a"),
    ]
    .into_iter()
    .peekable();

    let mut parser = Parser::new(&grammar);
    loop {
        match parser.next_event(&mut tokens) {
            ParseEvent::Reduce(rule_id, args) => {
                let rule = grammar.rule(rule_id);
                let lhs = grammar.symbol(rule.lhs);
                println!(
                    "reduce: {:?} -> {}",
                    args.iter()
                        .map(|s| match s {
                            ParseItem::N(t) => format!("<{}>", grammar.symbol(*t).name()),
                            ParseItem::T(_, value) => value.to_string(),
                        })
                        .collect::<Vec<_>>(),
                    lhs.name(),
                );
            }
            ParseEvent::Accept(s) => {
                let s = match s {
                    ParseItem::N(s) => grammar.symbol(s),
                    ParseItem::T(s, _) => grammar.symbol(s),
                };
                println!("accept: {}", s.name());
                break;
            }
        }
    }
}
