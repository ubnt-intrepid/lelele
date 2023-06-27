use crate::parser::TokenID;
use logos::Logos;

// 入力のトークン列
#[derive(Debug, Copy, Clone, Logos, PartialEq)]
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

impl lelele_runtime::parser::Token<TokenID> for Token<'_> {
    fn to_index(&self) -> TokenID {
        match self {
            Self::LParen => TokenID::LPAREN,
            Self::RParen => TokenID::RPAREN,
            Self::Plus => TokenID::PLUS,
            Self::Minus => TokenID::MINUS,
            Self::Star => TokenID::STAR,
            Self::Slash => TokenID::SLASH,
            Self::Num(..) => TokenID::NUM,
        }
    }
}

pub fn lexer<'source>(input: &'source str) -> impl Iterator<Item = anyhow::Result<Token>> {
    Token::lexer(input).map(|res| res.map_err(|_| anyhow::anyhow!("lexer error")))
}
