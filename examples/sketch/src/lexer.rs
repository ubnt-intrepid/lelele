use crate::parser::SymbolID;
use logos::Logos;

// 入力のトークン列
#[derive(Debug, Logos, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token<'source> {
    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[regex(r"[0-9]+")]
    Num(&'source str),
}

impl lelele_runtime::parser::Token<SymbolID> for Token<'_> {
    fn as_symbol(&self) -> SymbolID {
        match self {
            Self::LParen => SymbolID::LPAREN,
            Self::RParen => SymbolID::RPAREN,
            Self::Plus => SymbolID::PLUS,
            Self::Minus => SymbolID::MINUS,
            Self::Star => SymbolID::STAR,
            Self::Slash => SymbolID::SLASH,
            Self::Num(..) => SymbolID::NUM,
        }
    }
}

pub fn lexer<'source>(input: &'source str) -> impl Iterator<Item = anyhow::Result<Token>> {
    Token::lexer(input).map(|res| res.map_err(|_| anyhow::anyhow!("lexer error")))
}
