use anyhow::Result;
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

fn tokens() -> impl Iterator<Item = Result<Token<'static>>> {
    [
        Ok(Token::Num("1")),
        Ok(Token::Plus),
        Ok(Token::Num("2")),
        Ok(Token::Equal),
        Ok(Token::Ident("a")),
    ]
    .into_iter()
}

fn main() -> anyhow::Result<()> {
    let mut parser = parser();
    let mut tokens = tokens();
    let mut args = vec![];
    loop {
        match parser.next_event(&mut tokens, &mut args)? {
            ParseEvent::Reduce(rule) => {
                println!("reduce({:?}, {:?})", rule, args);
            }
            ParseEvent::Accept => {
                println!("accept: {:?}", args);
                break;
            }
        }
    }
    Ok(())
}
