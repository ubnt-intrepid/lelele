use crate::parser::SymbolID;
use logos::Logos;

// 入力のトークン列
#[derive(Debug, Logos, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token<'source> {
    #[token("+")]
    Plus,

    #[token("=")]
    Equal,

    #[regex(r"[0-9]+")]
    Num(&'source str),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident(&'source str),
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

pub fn lexer<'source>(input: &'source str) -> impl Iterator<Item = anyhow::Result<Token>> {
    Token::lexer(input).map(|res| res.map_err(|_| anyhow::anyhow!("lexer error")))
}
