use super::grammar::TokenID;
use logos::Logos;
use std::mem;

#[derive(Debug, Logos)]
#[logos(skip "[ \r\n\t]")]
pub enum Token<'source> {
    #[token("//")]
    LineCommentBegin,

    #[token("/*")]
    BlockCommentBegin,

    #[token("{")]
    LBracket,

    #[token("}")]
    RBracket,

    #[token("@{")]
    AtLBracket,

    #[token(":=")]
    ColonEq,

    #[token("=")]
    Eq,

    #[token(",")]
    Comma,

    #[token(";")]
    Semicolon,

    #[token("|")]
    VertBar,

    #[token("@terminal")]
    Terminal,

    #[token("@nonterminal")]
    Nonterminal,

    #[token("@start")]
    Start,

    #[token("@rule")]
    Rule,

    #[token("@prec")]
    Prec,

    #[token("@empty")]
    Empty,

    #[regex(r"[_a-zA-Z][_0-9a-zA-Z]*")]
    Ident(&'source str),
}

impl lelele_runtime::parser::Token<TokenID> for Token<'_> {
    fn to_index(&self) -> TokenID {
        match self {
            Self::LBracket => TokenID::LBRACKET,
            Self::RBracket => TokenID::RBRACKET,
            Self::AtLBracket => TokenID::AT_LBRACKET,
            Self::ColonEq => TokenID::COLON_EQ,
            Self::Eq => TokenID::EQ,
            Self::Comma => TokenID::COMMA,
            Self::Semicolon => TokenID::SEMICOLON,
            Self::VertBar => TokenID::VERT_BAR,
            Self::Terminal => TokenID::TERMINAL,
            Self::Nonterminal => TokenID::NONTERMINAL,
            Self::Start => TokenID::START,
            Self::Rule => TokenID::RULE,
            Self::Prec => TokenID::PREC,
            Self::Empty => TokenID::EMPTY,
            Self::Ident(..) => TokenID::IDENT,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Logos)]
enum LineCommentToken {
    #[regex(r"(?:\r?\n|\r)")]
    Newline,

    #[regex(".")]
    Anything,
}

#[derive(Debug, Logos)]
enum BlockCommentToken {
    #[token("/*")]
    BlockCommentBegin,

    #[token("*/")]
    BlockCommentEnd,

    #[regex("(?s:.)")]
    Anything,
}

#[derive(Debug)]
pub struct Lexer<'source> {
    mode: LexerMode<'source>,
}

#[derive(Debug)]
enum LexerMode<'source> {
    Main(logos::Lexer<'source, Token<'source>>),
    LineComment(logos::Lexer<'source, LineCommentToken>),
    BlockComment {
        lexer: logos::Lexer<'source, BlockCommentToken>,
        nest_level: usize,
    },
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
                LexerMode::Main(lexer) => match lexer.next()? {
                    Ok(Token::LineCommentBegin) => {
                        let LexerMode::Main(main) = mem::replace(&mut self.mode, LexerMode::Gone) else { unreachable!() };
                        self.mode = LexerMode::LineComment(main.morph());
                        continue;
                    }
                    Ok(Token::BlockCommentBegin) => {
                        let LexerMode::Main(main) = mem::replace(&mut self.mode, LexerMode::Gone) else { unreachable!() };
                        self.mode = LexerMode::BlockComment {
                            lexer: main.morph(),
                            nest_level: 1,
                        };
                        continue;
                    }
                    next => return Some(next.map_err(|_| anyhow::anyhow!("lexer error"))),
                },
                LexerMode::LineComment(lexer) => match lexer.next()? {
                    Ok(LineCommentToken::Newline) => {
                        let LexerMode::LineComment(lexer) = mem::replace(&mut self.mode, LexerMode::Gone) else { unreachable!() };
                        self.mode = LexerMode::Main(lexer.morph());
                        continue;
                    }
                    _ => continue,
                },
                LexerMode::BlockComment { lexer, nest_level } => match lexer.next()? {
                    Ok(BlockCommentToken::BlockCommentBegin) => {
                        *nest_level += 1;
                        continue;
                    }
                    Ok(BlockCommentToken::BlockCommentEnd) if *nest_level > 1 => {
                        *nest_level -= 1;
                        continue;
                    }
                    Ok(BlockCommentToken::BlockCommentEnd) => {
                        let LexerMode::BlockComment { lexer, .. } = mem::replace(&mut self.mode, LexerMode::Gone) else { unreachable!() };
                        self.mode = LexerMode::Main(lexer.morph());
                        continue;
                    }
                    Ok(BlockCommentToken::Anything) => continue,
                    Err(_) => return Some(Err(anyhow::anyhow!("lexer error"))),
                },
                LexerMode::Gone => unreachable!(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[test]
    fn case1() {
        let input = "\
@prec { assoc = left } prec1;
@terminal FOO, BAR, BAZ;
@nonterminal Expr;
@rule Expr :=
    { FOO BAR BAZ
    | @{ prec = prec1 } Expr FOO
    };
";
        let tokens = Token::lexer(input).collect::<Result<Vec<_>, _>>().unwrap();
        assert!(matches!(
            dbg!(&tokens[..]),
            [
                // @prec ... ;
                Prec,
                LBracket,
                Ident("assoc"),
                Eq,
                Ident("left"),
                RBracket,
                Ident("prec1"),
                Semicolon,
                // @terminal ... ;
                Terminal,
                Ident("FOO"),
                Comma,
                Ident("BAR"),
                Comma,
                Ident("BAZ"),
                Semicolon,
                // @nonterminal ... ;
                Nonterminal,
                Ident("Expr"),
                Semicolon,
                // @rule ... ;
                Rule,
                Ident("Expr"),
                ColonEq,
                LBracket,
                Ident("FOO"),
                Ident("BAR"),
                Ident("BAZ"),
                VertBar,
                AtLBracket,
                Ident("prec"),
                Eq,
                Ident("prec1"),
                RBracket,
                Ident("Expr"),
                Ident("FOO"),
                RBracket,
                Semicolon,
            ]
        ));
    }
}
