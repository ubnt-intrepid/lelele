//! Syntax definition.

use crate::lexer::Token;

#[derive(Debug)]
pub enum Expr<'source> {
    Add {
        lhs: Box<Expr<'source>>,
        op: Token<'source>,
        rhs: Box<Factor<'source>>,
    },
    Sub {
        lhs: Box<Expr<'source>>,
        op: Token<'source>,
        rhs: Box<Factor<'source>>,
    },
    Factor(Box<Factor<'source>>),
}

#[derive(Debug)]
pub enum Factor<'source> {
    Mul {
        lhs: Box<Factor<'source>>,
        op: Token<'source>,
        rhs: Box<Term<'source>>,
    },
    Div {
        lhs: Box<Factor<'source>>,
        op: Token<'source>,
        rhs: Box<Term<'source>>,
    },
    Term(Box<Term<'source>>),
}

#[derive(Debug)]
pub enum Term<'source> {
    Num(Token<'source>),
    Paren {
        l_paren: Token<'source>,
        expr: Box<Expr<'source>>,
        r_paren: Token<'source>,
    },
}
