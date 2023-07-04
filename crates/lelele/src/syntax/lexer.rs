//! Lexer implementation.

use super::grammar::TokenID;
use lexgen_util::Loc;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Token<'input> {
    LBracket,
    RBracket,
    AtLBracket,
    ColonEq,
    Eq,
    Comma,
    Semicolon,
    VertBar,
    Kw(Keyword),
    Ident(&'input str),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Keyword {
    Terminal,
    Nonterminal,
    Start,
    Rule,
    Prec,
    Empty,
}

impl lelele_runtime::engine::Token<TokenID> for Token<'_> {
    fn to_index(&self) -> TokenID {
        match self {
            Token::LBracket => TokenID::LBRACKET,
            Token::RBracket => TokenID::RBRACKET,
            Token::AtLBracket => TokenID::AT_LBRACKET,
            Token::ColonEq => TokenID::COLON_EQ,
            Token::Eq => TokenID::EQ,
            Token::Comma => TokenID::COMMA,
            Token::Semicolon => TokenID::SEMICOLON,
            Token::VertBar => TokenID::VERT_BAR,
            Token::Kw(Keyword::Terminal) => TokenID::KW_TERMINAL,
            Token::Kw(Keyword::Nonterminal) => TokenID::KW_NONTERMINAL,
            Token::Kw(Keyword::Start) => TokenID::KW_START,
            Token::Kw(Keyword::Rule) => TokenID::KW_RULE,
            Token::Kw(Keyword::Prec) => TokenID::KW_PREC,
            Token::Kw(Keyword::Empty) => TokenID::KW_EMPTY,
            Token::Ident(..) => TokenID::IDENT,
        }
    }
}

pub type Spanned<'input> = (Loc, Token<'input>, Loc);
impl lelele_runtime::engine::Token<TokenID> for Spanned<'_> {
    #[inline]
    fn to_index(&self) -> TokenID {
        self.1.to_index()
    }
}

#[derive(Debug, Default)]
pub struct LexerState {
    comment_depth: usize,
}

lexgen::lexer! {
    pub Lexer(LexerState) -> Token<'input>;

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
        "{" = Token::LBracket,
        "}" = Token::RBracket,
        "@{" = Token::AtLBracket,
        ":=" = Token::ColonEq,
        "=" = Token::Eq,
        "," = Token::Comma,
        ";" = Token::Semicolon,
        "|" = Token::VertBar,
        "@terminal" = Token::Kw(Keyword::Terminal),
        "@nonterminal" = Token::Kw(Keyword::Nonterminal),
        "@start" = Token::Kw(Keyword::Start),
        "@rule" = Token::Kw(Keyword::Rule),
        "@prec" = Token::Kw(Keyword::Prec),
        "@empty" = Token::Kw(Keyword::Empty),
        $ident => |lexer| {
            let token = Token::Ident(lexer.match_());
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

#[cfg(test)]
mod tests {
    use super::*;
    use Keyword::*;
    use Token::*;

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
            .map(|res| res.map(|(_, t, _)| t))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert!(matches!(
            dbg!(&tokens[..]),
            [
                // @prec ... ;
                Kw(Prec),
                LBracket,
                Ident("assoc"),
                Eq,
                Ident("left"),
                RBracket,
                Ident("prec1"),
                Semicolon,
                // @terminal ... ;
                Kw(Terminal),
                Ident("FOO"),
                Comma,
                Ident("BAR"),
                Comma,
                Ident("BAZ"),
                Semicolon,
                // @nonterminal ... ;
                Kw(Nonterminal),
                Ident("Expr"),
                Comma,
                Ident("ｔｒｕｅ"),
                Semicolon,
                // @rule ... ;
                Kw(Rule),
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
