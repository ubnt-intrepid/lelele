use lelele_example_sketch::parser::{parser, SymbolID};
use lelele_runtime::parser::ParseEvent;

// 入力のトークン列
#[derive(Debug)]
enum Token<'t> {
    Plus,
    Equal,
    Num(&'static str),
    Ident(&'t str),
}
impl lelele_runtime::parser::Token<SymbolID> for Token<'_> {
    fn as_symbol(&self) -> SymbolID {
        match self {
            Self::Plus => SymbolID::PLUS,
            Self::Equal => SymbolID::EQUAL,
            Self::Num(..) => SymbolID::NUM,
            Self::Ident(..) => SymbolID::ID,
        }
    }
}

fn main() {
    let mut tokens = [
        Token::Num("1"),
        Token::Plus,
        Token::Num("2"),
        Token::Equal,
        Token::Ident("a"),
    ]
    .into_iter();

    let mut parser = parser();
    let mut args = vec![];
    loop {
        match parser.next_event(&mut tokens, &mut args) {
            ParseEvent::Reduce(rule) => {
                println!("reduce({:?}, {:?})", rule, args);
            }
            ParseEvent::Accept => {
                println!("accept: {:?}", args);
                break;
            }
        }
    }
}
