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

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("not")]
    Not,

    #[regex(r"[0-9]+", priority = 1)]
    Int(&'source str),

    #[regex(r"[0-9]+(\.[0-9]*)?([eE][\+\-]?[0-9]+)?", priority = 0)]
    Float(&'source str),

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("+.")]
    PlusDot,

    #[token("-.")]
    MinusDot,

    #[token("*.")]
    StarDot,

    #[token("/.")]
    SlashDot,

    #[token("=")]
    Equal,

    #[token("<>")]
    LessGreater,

    #[token("<")]
    Less,

    #[token(">")]
    Greater,

    #[token("<=")]
    LessEqual,

    #[token(">=")]
    GreaterEqual,

    #[token("<-")]
    LessMinus,

    #[token("if")]
    If,

    #[token("then")]
    Then,

    #[token("else")]
    Else,

    #[token("let")]
    Let,

    #[token("in")]
    In,

    #[token("rec")]
    Rec,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token(";")]
    Semicolon,

    #[regex(r"Array\.create|Array.make")]
    ArrayMake,

    #[regex(r"[a-zA-Z_][0-9a-zA-Z_]*")]
    Ident(&'source str),
}

impl lelele_runtime::parser::Token<TokenID> for Token<'_> {
    fn as_symbol(&self) -> TokenID {
        match self {
            Self::LParen => TokenID::LPAREN,
            Self::RParen => TokenID::RPAREN,
            Self::True => TokenID::TRUE,
            Self::False => TokenID::FALSE,
            Self::Not => TokenID::NOT,
            Self::Int(..) => TokenID::INTEGER,
            Self::Float(..) => TokenID::FLOAT,
            Self::Plus => TokenID::PLUS,
            Self::Minus => TokenID::MINUS,
            Self::PlusDot => TokenID::PLUS_DOT,
            Self::MinusDot => TokenID::MINUS_DOT,
            Self::StarDot => TokenID::STAR_DOT,
            Self::SlashDot => TokenID::SLASH_DOT,
            Self::Equal => TokenID::EQUAL,
            Self::LessGreater => TokenID::LESS_GREATER,
            Self::Less => TokenID::LESS,
            Self::Greater => TokenID::GREATER,
            Self::LessEqual => TokenID::LESS_EQUAL,
            Self::GreaterEqual => TokenID::GREATER_EQUAL,
            Self::Ident(..) => TokenID::IDENT,
            Self::LessMinus => TokenID::LESS_MINUS,
            Self::If => TokenID::IF,
            Self::Then => TokenID::THEN,
            Self::Else => TokenID::ELSE,
            Self::Let => TokenID::LET,
            Self::In => TokenID::IN,
            Self::Rec => TokenID::REC,
            Self::Comma => TokenID::COMMA,
            Self::Dot => TokenID::DOT,
            Self::Semicolon => TokenID::SEMICOLON,
            Self::ArrayMake => TokenID::ARRAY_MAKE,
        }
    }
}

pub fn lexer<'source>(input: &'source str) -> impl Iterator<Item = anyhow::Result<Token>> {
    Token::lexer(input).map(|res| res.map_err(|_| anyhow::anyhow!("lexer error")))
}
