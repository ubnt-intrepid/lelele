//! Lexer implementation.

use super::grammar::TokenID;
use lexgen_util::Loc;
use std::convert::Infallible;

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'input> {
    pub kind: TokenKind<'input>,
    pub begin: Loc,
    pub end: Loc,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenKind<'input> {
    LBracket,
    RBracket,
    AtLBracket,
    ColonEq,
    Eq,
    Comma,
    Semicolon,
    VertBar,
    Terminal,
    Nonterminal,
    Start,
    Rule,
    Prec,
    Empty,
    Ident(&'input str),
}

impl lelele_runtime::parser::Token<TokenID> for Token<'_> {
    fn to_index(&self) -> TokenID {
        match self.kind {
            TokenKind::LBracket => TokenID::LBRACKET,
            TokenKind::RBracket => TokenID::RBRACKET,
            TokenKind::AtLBracket => TokenID::AT_LBRACKET,
            TokenKind::ColonEq => TokenID::COLON_EQ,
            TokenKind::Eq => TokenID::EQ,
            TokenKind::Comma => TokenID::COMMA,
            TokenKind::Semicolon => TokenID::SEMICOLON,
            TokenKind::VertBar => TokenID::VERT_BAR,
            TokenKind::Terminal => TokenID::TERMINAL,
            TokenKind::Nonterminal => TokenID::NONTERMINAL,
            TokenKind::Start => TokenID::START,
            TokenKind::Rule => TokenID::RULE,
            TokenKind::Prec => TokenID::PREC,
            TokenKind::Empty => TokenID::EMPTY,
            TokenKind::Ident(..) => TokenID::IDENT,
        }
    }
}

pub struct Lexer<'input> {
    inner: imp::Lexer<'input, std::str::Chars<'input>>,
}
impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            inner: imp::Lexer::new_with_state(input, imp::LexerState::default()),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<Token<'input>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next()? {
            Ok((begin, kind, end)) => Some(Ok(Token { kind, begin, end })),
            Err(inner) => Some(Err(LexerError { inner })),
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("lexer error")]
pub struct LexerError {
    inner: lexgen_util::LexerError<Infallible>,
}

mod imp {
    use super::*;

    #[derive(Debug, Default)]
    pub struct LexerState {
        comment_depth: usize,
    }

    lexgen::lexer! {
        pub Lexer(LexerState) -> TokenKind<'input>;

        let whitespace = [' ' '\t' '\n'];
        let newline = '\r'* '\n' | '\r';
        let ident = $$XID_Start $$XID_Continue*;

        rule Init {
            $whitespace+,
            "//" => |lexer| {
                lexer.switch(LexerRule::LineComment)
            },
            "/*" => |lexer| {
                lexer.state().comment_depth += 1;
                lexer.switch(LexerRule::BlockComment)
            },
            "{" = TokenKind::LBracket,
            "}" = TokenKind::RBracket,
            "@{" = TokenKind::AtLBracket,
            ":=" = TokenKind::ColonEq,
            "=" = TokenKind::Eq,
            "," = TokenKind::Comma,
            ";" = TokenKind::Semicolon,
            "|" = TokenKind::VertBar,
            "@terminal" = TokenKind::Terminal,
            "@nonterminal" = TokenKind::Nonterminal,
            "@start" = TokenKind::Start,
            "@rule" = TokenKind::Rule,
            "@prec" = TokenKind::Prec,
            "@empty" = TokenKind::Empty,
            $ident => |lexer| {
                let token = TokenKind::Ident(lexer.match_());
                lexer.return_(token)
            },
        }

        rule LineComment {
            $newline => |lexer| {
                lexer.switch(LexerRule::Init)
            },
            _,
        }

        rule BlockComment {
            "/*" => |lexer| {
                lexer.state().comment_depth += 1;
                lexer.continue_()
            },
            "*/" => |lexer| {
                let depth = &mut lexer.state().comment_depth;
                if *depth == 1 {
                    lexer.switch(LexerRule::Init)
                } else {
                    *depth -= 1;
                    lexer.continue_()
                }
            },
            _,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenKind::*;

    #[test]
    fn smoketest() {
        let input = "\
@prec { assoc = left } prec1; // precedences /* block-comment in line-comment */
@terminal FOO, BAR, BAZ; /* block comment /* nested */ */
@nonterminal Expr, ｔｒｕｅ;
@rule Expr :=
    { FOO BAR BAZ
    | @{ prec = prec1 } Expr FOO
    };
";
        let lexer = Lexer::new(input);
        let tokens = lexer
            .map(|res| res.map(|t| t.kind))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
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
                Comma,
                Ident("ｔｒｕｅ"),
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
