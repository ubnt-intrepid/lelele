use crate::parser::SymbolID;

// 入力のトークン列
#[derive(Debug)]
pub enum Token<'t> {
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

/// `1 + 2 = a`
pub fn tokens() -> impl Iterator<Item = anyhow::Result<Token<'static>>> {
    [
        Ok(Token::Num("1")),
        Ok(Token::Plus),
        Ok(Token::Num("2")),
        Ok(Token::Equal),
        Ok(Token::Ident("a")),
    ]
    .into_iter()
}
