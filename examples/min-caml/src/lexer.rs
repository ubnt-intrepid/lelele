use crate::parser::TokenID;
use logos::Logos;
use std::mem;

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

    #[regex(r"[0-9]+", priority = 1)]
    Int(&'source str),

    #[regex(r"[0-9]+(\.[0-9]*)?([eE][\+\-]?[0-9]+)?", priority = 0)]
    Float(&'source str),

    #[regex(r"[a-zA-Z_][0-9a-zA-Z_]*")]
    Ident(&'source str),

    #[token("(*")]
    CommentBegin,
}

impl lelele_runtime::parser::Token<TokenID> for Token<'_> {
    fn to_index(&self) -> TokenID {
        match self {
            Self::LParen => TokenID::LPAREN,
            Self::RParen => TokenID::RPAREN,
            Self::True => TokenID::TRUE,
            Self::False => TokenID::FALSE,
            Self::Not => TokenID::NOT,
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
            Self::Ident(..) => TokenID::IDENT,
            Self::Int(..) => TokenID::INTEGER,
            Self::Float(..) => TokenID::FLOAT,
            Self::CommentBegin => unreachable!(),
        }
    }
}

#[derive(Debug, Logos)]
enum CommentToken {
    #[token("(*")]
    CommentBegin,

    #[token("*)")]
    CommentEnd,

    #[regex("(?s:.)")]
    Anything,
}

#[derive(Debug)]
pub struct Lexer<'source> {
    mode: LexerMode<'source>,
}

#[derive(Debug, Default)]
enum LexerMode<'source> {
    Main(logos::Lexer<'source, Token<'source>>),
    Comment {
        lexer: logos::Lexer<'source, CommentToken>,
        nest_level: usize,
    },
    #[default]
    Gone,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            mode: LexerMode::Main(Token::lexer(source)),
        }
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = anyhow::Result<Token<'source>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match &mut self.mode {
                LexerMode::Main(lexer) => {
                    let next = lexer.next()?;
                    if let Ok(Token::CommentBegin) = next {
                        let LexerMode::Main(lexer) = mem::take(&mut self.mode) else { unreachable!() };
                        self.mode = LexerMode::Comment {
                            lexer: lexer.morph(),
                            nest_level: 1,
                        };
                        continue;
                    }
                    return Some(next.map_err(|_| anyhow::anyhow!("lexer error")));
                }

                LexerMode::Comment { lexer, nest_level } => {
                    let next = lexer.next()?;
                    match next {
                        Ok(CommentToken::CommentBegin) => {
                            *nest_level += 1;
                            continue;
                        }
                        Ok(CommentToken::CommentEnd) => {
                            if *nest_level > 1 {
                                *nest_level -= 1;
                                continue;
                            }

                            let LexerMode::Comment { lexer, .. } = mem::take(&mut self.mode) else { unreachable!() };
                            self.mode = LexerMode::Main(lexer.morph());
                            continue;
                        }
                        Ok(_) => continue,
                        Err(_) => return Some(Err(anyhow::anyhow!("lexer error"))),
                    }
                }

                LexerMode::Gone => unreachable!(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nested_comment() {
        let source = "(* (* hi *) hello *) 10 (** foo =*) false";
        let tokens = Lexer::new(source)
            .collect::<anyhow::Result<Vec<_>>>()
            .unwrap();
        assert!(matches!(
            dbg!(&tokens[..]),
            [Token::Int("10"), Token::False]
        ));
    }
}
