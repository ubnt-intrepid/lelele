//! Syntax definition.

use crate::lexer::Token;

#[derive(Debug)]
pub enum Ast<'t> {
    Equal {
        lhs: Box<Expr<'t>>,
        op: Token<'t>,
        rhs: Box<Expr<'t>>,
    },
    Ident(Token<'t>),
}

#[derive(Debug)]
pub enum Expr<'t> {
    Plus {
        lhs: Box<Expr<'t>>,
        op: Token<'t>,
        rhs: Box<Term<'t>>,
    },
    Term(Box<Term<'t>>),
}

#[derive(Debug)]
pub enum Term<'t> {
    Num(Token<'t>),
    Ident(Token<'t>),
}
