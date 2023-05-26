use lelele::{
    grammar::{Grammar, SymbolID},
    parser::ParserDefinition,
};
use lelele_runtime::parser::{ParseEvent, Parser};

fn main() {
    // 文法定義
    let mut def = Grammar::definition();

    let t_ident = def.token("ID");
    let t_num = def.token("NUM");
    let t_plus = def.token("PLUS");
    let t_equal = def.token("EQUAL");

    let e_A = def.symbol("A");
    let e_E = def.symbol("E");
    let e_T = def.symbol("T");

    def.rule(e_A, [e_E, t_equal, e_E]);
    def.rule(e_A, [t_ident]);
    def.rule(e_E, [e_E, t_plus, e_T]);
    def.rule(e_E, [e_T]);
    def.rule(e_T, [t_num]);
    def.rule(e_T, [t_ident]);

    def.start(e_A);

    let grammar = def.end();

    // 入力のトークン列
    #[derive(Debug)]
    struct Token(SymbolID, &'static str);
    impl lelele_runtime::parser::Token<SymbolID> for Token {
        fn as_symbol(&self) -> SymbolID {
            self.0
        }
    }
    let mut tokens = [
        Token(t_num, "1"),
        Token(t_plus, "+"),
        Token(t_num, "2"),
        Token(t_equal, "="),
        Token(t_ident, "a"),
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
