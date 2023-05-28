//! Syntax definition.

use crate::lexer::Token;

#[derive(Debug)]
pub enum Ast<'source> {
    Equal {
        lhs: Box<Expr<'source>>,
        op: Token<'source>,
        rhs: Box<Expr<'source>>,
    },
    Ident(Token<'source>),
}

#[derive(Debug)]
pub enum Expr<'source> {
    Plus {
        lhs: Box<Expr<'source>>,
        op: Token<'source>,
        rhs: Box<Term<'source>>,
    },
    Term(Box<Term<'source>>),
}

#[derive(Debug)]
pub enum Term<'source> {
    Num(Token<'source>),
    Ident(Token<'source>),
}
