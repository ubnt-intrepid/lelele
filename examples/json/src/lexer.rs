use crate::parser::TokenID;
use logos::Logos;

// 入力のトークン列
#[derive(Debug, Copy, Clone, Logos, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token<'source> {
    #[token("{")]
    LBracket,

    #[token("}")]
    RBracket,

    #[token("[")]
    LBrace,

    #[token("]")]
    RBrace,

    #[token(",")]
    Comma,

    #[token(":")]
    Colon,

    #[token("null")]
    Null,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[regex(r###"-?(?:0|[1-9][0-9]*)(?:\.[0-9]+)?(?:[eE][-+]?(?:0|[1-9][0-9]*)?)?"###)]
    Number(&'source str),

    #[regex(r###"\u{0022}(?:\\[\u{0022}\\/bfnrt]|u[0-9a-fA-F]{4}|[^\u{0022}\\\u0000-\u001F])*\u{0022}"###)]
    String(&'source str),
}

impl lelele_runtime::parser::Token<TokenID> for Token<'_> {
    fn to_index(&self) -> TokenID {
        match self {
            Self::LBracket => TokenID::LBRACKET,
            Self::RBracket => TokenID::RBRACKET,
            Self::LBrace => TokenID::LBRACE,
            Self::RBrace => TokenID::RBRACE,
            Self::Comma => TokenID::COMMA,
            Self::Colon => TokenID::COLON,
            Self::Null => TokenID::NULL,
            Self::True => TokenID::TRUE,
            Self::False => TokenID::FALSE,
            Self::Number(..) => TokenID::NUMBER,
            Self::String(..) => TokenID::STRING,
        }
    }
}

pub fn lexer<'source>(input: &'source str) -> impl Iterator<Item = anyhow::Result<Token>> {
    Token::lexer(input).map(|res| res.map_err(|_| anyhow::anyhow!("lexer error")))
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_matches {
        ($($t:tt)*) => {
            assert!(matches!($($t)*))
        };
    }

    #[test]
    fn simple() {
        let tokens = lexer("null").collect::<Result<Vec<_>, _>>().unwrap();
        assert_matches!(&tokens[..], [Token::Null]);

        let tokens = lexer("\"foo bar baz\"")
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_matches!(&tokens[..], [Token::String("\"foo bar baz\"")]);
    }

    #[test]
    fn compound() {
        let tokens = lexer(r#"{ "foo": [1, true] }"#)
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_matches!(
            &tokens[..],
            [
                Token::LBracket,
                Token::String(r#""foo""#),
                Token::Colon,
                Token::LBrace,
                Token::Number("1"),
                Token::Comma,
                Token::True,
                Token::RBrace,
                Token::RBracket
            ]
        );
    }
}
