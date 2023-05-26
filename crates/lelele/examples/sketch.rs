use indexmap::IndexMap;
use lelele::{
    grammar::{Grammar, SymbolID},
    parser::ParserDefinition,
};
use lelele_runtime::parser::{ParseEvent, Parser};

#[derive(Debug)]
enum RuleName {
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
}

fn main() {
    // 文法定義
    let grammar = Grammar::define(|def| {
        use RuleName::*;

        def.token("ID");
        def.token("NUM");
        def.token("PLUS");
        def.token("EQUAL");

        def.rule(R1, "A", ["E", "EQUAL", "E"]);
        def.rule(R2, "A", ["ID"]);
        def.rule(R3, "E", ["E", "PLUS", "T"]);
        def.rule(R4, "E", ["T"]);
        def.rule(R5, "T", ["NUM"]);
        def.rule(R6, "T", ["ID"]);
        def.start("A");
    });

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

    let def = ParserDefinition::new(&grammar);
    let mut parser = Parser::new(def);
    loop {
        match parser.next_event(&mut tokens) {
            ParseEvent::Reduce(rule, args) => {
                println!("reduce({:?}, {:?})", rule, args);
            }
            ParseEvent::Accept(s) => {
                println!("accept: {:?}", s);
                break;
            }
        }
    }
}
